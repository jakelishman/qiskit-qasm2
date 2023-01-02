from qiskit.circuit import (
    QuantumCircuit,
    QuantumRegister,
    ClassicalRegister,
    Measure,
    Reset,
    Barrier,
    CircuitInstruction,
    Gate,
    Qubit,
    library as lib,
)
from .core import OpCode

# Constructors of the form `*params -> Gate` for the special 'qelib1.inc' include.  This is
# essentially a pre-parsed state for the file as given in the arXiv paper defining OQ2.
QELIB1 = (
    lib.U3Gate,
    lib.U2Gate,
    lib.U1Gate,
    lib.CXGate,
    lambda: lib.UGate(0, 0, 0),
    lib.XGate,
    lib.YGate,
    lib.ZGate,
    lib.HGate,
    lib.SGate,
    lib.SdgGate,
    lib.TGate,
    lib.TdgGate,
    lib.RXGate,
    lib.RYGate,
    lib.RZGate,
    lib.CZGate,
    lib.CYGate,
    lib.CHGate,
    lib.CCXGate,
    lib.CRZGate,
    lib.CU1Gate,
    lambda a, b, c: lib.CUGate(a, b, c, 0),
)


def from_bytecode(bytecode):
    """Loop through the Rust bytecode iterator `bytecode` producing a
    :class:`~qiskit.circuit.QuantumCircuit` instance from it.  All the hard work is done in Rust
    space where operations are faster; here, we're just about looping through the instructions as
    fast as possible, doing as little calculation as we can in Python space.  The Python-space
    components are the vast majority of the runtime.

    The "bytecode", and so also this Python function, is very tightly coupled to the output of the
    Rust parser.  The bytecode itself is largely defined by Rust; from Python space, the iterator is
    over essentially a 2-tuple of `(opcode, operands)`.  The `operands` are fixed by Rust, and
    assumed to be correct by this function.

    The Rust code is responsible for all validation.  If this function causes any errors to be
    raised by Qiskit (except perhaps for some symbolic manipulations of `Parameter` objects), we
    should consider that a bug in the Rust code."""
    # The method `QuantumCircuit._append` is a semi-public method, so isn't really subject to
    # "protected access".
    # pylint: disable=protected-access
    qc = QuantumCircuit()
    qubits = []
    clbits = []
    gates = [lib.UGate, lib.CXGate]
    # Pull this out as an explicit iterator so we can manually advance the loop in `DeclareGate`
    # contexts easily.
    bc = iter(bytecode)
    for op in bc:
        # We have to check `op.opcode` so many times, it's worth pulling out the extra attribute
        # access.  We should check the opcodes in order of their likelihood to be in the OQ2 program
        # for speed.  Gate applications are by far the most common for long programs.  This function
        # is deliberately long and does not use hashmaps or function lookups for speed in
        # Python-space.
        opcode = op.opcode
        # `OpCode` is an `enum` in Rust, but its instances don't have the same singleton property as
        # Python `enum.Enum` objects.
        if opcode == OpCode.Gate:
            gate_id, parameters, op_qubits = op.operands
            qc._append(
                CircuitInstruction(gates[gate_id](*parameters), [qubits[q] for q in op_qubits])
            )
        elif opcode == OpCode.ConditionedGate:
            gate_id, parameters, op_qubits, creg, value = op.operands
            gate = gates[gate_id](*parameters)
            gate.condition = (qc.cregs[creg], value)
            qc._append(CircuitInstruction(gate, [qubits[q] for q in op_qubits]))
        elif opcode == OpCode.Measure:
            qubit, clbit = op.operands
            qc._append(CircuitInstruction(Measure(), (qubits[qubit],), (clbits[clbit],)))
        elif opcode == OpCode.ConditionedMeasure:
            qubit, clbit, creg, value = op.operands
            measure = Measure()
            measure.condition = (qc.cregs[creg], value)
            qc._append(CircuitInstruction(measure, (qubits[qubit],), (clbits[clbit],)))
        elif opcode == OpCode.Reset:
            qc._append(CircuitInstruction(Reset(), (qubits[op.operands[0]],)))
        elif opcode == OpCode.ConditionedReset:
            qubit, creg, value = op.operands
            reset = Reset()
            reset.condition = (qc.cregs[creg], value)
            qc._append(CircuitInstruction(reset, (qubits[qubit],)))
        elif opcode == OpCode.Barrier:
            op_qubits = op.operands[0]
            qc._append(CircuitInstruction(Barrier(len(op_qubits)), [qubits[q] for q in op_qubits]))
        elif opcode == OpCode.DeclareQreg:
            name, size = op.operands
            register = QuantumRegister(size, name)
            qubits += register[:]
            qc.add_register(register)
        elif opcode == OpCode.DeclareCreg:
            name, size = op.operands
            register = ClassicalRegister(size, name)
            clbits += register[:]
            qc.add_register(register)
        elif opcode == OpCode.SpecialInclude:
            # Including `qelib1.inc` is pretty much universal, and we treat its gates as having
            # special relationships to the Qiskit ones, so we don't actually parse it; we just
            # short-circuit to add its pre-calculated content to our state.
            gates += QELIB1
        elif opcode == OpCode.DeclareGate:
            name, params, n_qubits = op.operands
            inner_qubits = [Qubit() for _ in [None] * n_qubits]
            inner = QuantumCircuit(inner_qubits)
            # This inner loop advances the iterator of the outer loop as well, since `bc` is a
            # manually created iterator, rather than an implicit one from the first loop.
            for inner_op in bc:
                inner_opcode = inner_op.opcode
                if inner_opcode == OpCode.EndDeclareGate:
                    break
                elif inner_opcode == OpCode.Gate:
                    gate_id, parameters, op_qubits = inner_op.operands
                    inner._append(
                        CircuitInstruction(
                            gates[gate_id](*parameters), [inner_qubits[q] for q in op_qubits]
                        )
                    )
                elif inner_opcode == OpCode.Barrier:
                    op_qubits = inner_op.operands[0]
                    inner._append(
                        CircuitInstruction(
                            Barrier(len(op_qubits)), [inner_qubits[q] for q in op_qubits]
                        )
                    )
                else:
                    raise ValueError(f"invalid operation inside gate: {op}")
            gates.append(_gate_builder(name, params, inner))
        elif opcode == OpCode.DeclareOpaque:
            name, n_params, n_qubits = op.operands
            gates.append(_opaque_builder(name, n_params, n_qubits))
        else:
            raise ValueError(f"invalid operation: {op}")
    return qc


class _DefinedGate(Gate):
    """A gate object defined by a `gate` statement in an OpenQASM 2 program.  This object lazily
    binds its parameters to its definition, so it is only synthesised when required."""

    def __init__(self, name, base_definition, parameter_order, params):
        # This `base_definition` object is deliberately shared between all instances of the same
        # OQ2-defined gate.  It must not be mutated.
        self._base_definition = base_definition
        self._parameter_order = parameter_order
        circuit_parameters = set(base_definition.parameters)
        self._ignore = {p for p in parameter_order if p not in circuit_parameters}
        super().__init__(name, base_definition.num_qubits, list(params))

    def _define(self):
        self._definition = self._base_definition.assign_parameters(
            {p: v for p, v in zip(self._parameter_order, self.params) if p not in self._ignore}
        )


def _gate_builder(name, parameter_objects, definition):
    """Create a gate-builder function of the signature `*params -> Gate` for a gate with a given
    `name`, which takes the :class:`.qiskit.circuit.Parameter` objects (in the order they are given)
    to build a `definition`."""
    parameter_objects = tuple(parameter_objects)
    # This has two levels of indirection (`definer` and then `_DefinedGate`) to serve different
    # purposes: `definer` is a simple closure used during the creation of the circuit object, while
    # `_DefinedGate` is a concrete instance that will be in the resultant circuit and consequently
    # somewhat exposed to users.  It is important that the object in the circuit is fully defined
    # within itself so it can be pickled, for example.  We use a custom class rather than an ad-hoc
    # direct `Gate` instance so the `definition` field can be lazily populated when it is actually
    # needed, rather than eagerly during circuit construction.
    def definer(*params):
        if len(params) != len(parameter_objects):
            raise ValueError(
                "incorrect number of parameters in constructor:"
                f" expected {len(parameter_objects)}, got {len(params)}"
            )
        # `definition` is shared between instances; this is deliberate and part of the lazy
        # evaluation, and it is not mutated.
        return _DefinedGate(name, definition, parameter_objects, params)

    return definer


def _opaque_builder(name, n_params, n_qubits):
    """Create a gate-builder function of the signature `*params -> Gate` for an opaque gate with a given
    `name`, which takes the given numbers of parameters and qubits."""

    def definer(*params):
        if len(params) != n_params:
            raise ValueError(
                "incorrect number of parameters in constructor:"
                f" expected {n_params}, got {len(params)}"
            )
        return Gate(name, n_qubits, params)

    return definer
