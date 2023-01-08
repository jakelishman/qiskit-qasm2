import math

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
from .core import (
    OpCode,
    UnaryOpCode,
    BinaryOpCode,
    ExprConstant,
    ExprArgument,
    ExprUnary,
    ExprBinary,
)

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
            name, n_qubits = op.operands
            # This inner loop advances the iterator of the outer loop as well, since `bc` is a
            # manually created iterator, rather than an implicit one from the first loop.
            inner_bc = []
            for inner_op in bc:
                if inner_op.opcode == OpCode.EndDeclareGate:
                    break
                inner_bc.append(inner_op)
            # Technically there's a quadratic dependency in the number of gates here, which could be
            # fixed by just sharing a reference to `gates` rather than recreating a new object.
            # Gates can't ever be removed from the list, so it wouldn't get out-of-date, though
            # there's a minor risk of somewhere accidentally mutating it instead, and in practice
            # the cost shouldn't really matter.
            gates.append(_gate_builder(name, n_qubits, tuple(gates), inner_bc))
        elif opcode == OpCode.DeclareOpaque:
            name, n_qubits = op.operands
            gates.append(_opaque_builder(name, n_qubits))
        else:
            raise ValueError(f"invalid operation: {op}")
    return qc


class _DefinedGate(Gate):
    """A gate object defined by a `gate` statement in an OpenQASM 2 program.  This object lazily
    binds its parameters to its definition, so it is only synthesised when required."""

    def __init__(self, name, n_qubits, params, gates, bytecode):
        self._gates = gates
        self._bytecode = bytecode
        super().__init__(name, n_qubits, list(params))

    def _define(self):
        # This is a stripped-down version of the bytecode interpreter; there's very few opcodes that
        # we actually need to handle within gate bodies.
        # pylint: disable=protected-access
        qubits = [Qubit() for _ in [None] * self.num_qubits]
        qc = QuantumCircuit(qubits)
        for op in self._bytecode:
            if op.opcode == OpCode.Gate:
                gate_id, args, op_qubits = op.operands
                qc._append(
                    CircuitInstruction(
                        self._gates[gate_id](*(_evaluate_argument(a, self.params) for a in args)),
                        [qubits[q] for q in op_qubits],
                    )
                )
            elif op.opcode == OpCode.Barrier:
                op_qubits = op.operands[0]
                qc._append(
                    CircuitInstruction(Barrier(len(op_qubits)), [qubits[q] for q in op_qubits])
                )
            else:
                raise ValueError(f"received invalid bytecode to build gate: {op}")
        self._definition = qc

    # It's fiddly to implement pickling for PyO3 types (the bytecode stream), so instead if we need
    # to pickle ourselves, we just eagerly create the definition and pickle that.

    def __getstate__(self):
        return (self.name, self.num_qubits, self.params, self.definition)

    def __setstate__(self, state):
        name, n_qubits, params, definition = state
        super().__init__(name, n_qubits, params)
        self._gates = ()
        self._bytecode = ()
        self._definition = definition


def _gate_builder(name, n_qubits, known_gates, bytecode):
    """Create a gate-builder function of the signature `*params -> Gate` for a gate with a given
    `name`.  This produces a `_DefinedGate` class, whose `_define` method runs through the given
    `bytecode` using the current list of `known_gates` to interpret the gate indices.

    The indirection here is mostly needed to correctly close over `known_gates` and `bytecode`."""

    def definer(*params):
        return _DefinedGate(name, n_qubits, params, known_gates, tuple(bytecode))

    return definer


def _opaque_builder(name, n_qubits):
    """Create a gate-builder function of the signature `*params -> Gate` for an opaque gate with a
    given `name`, which takes the given numbers of qubits."""

    def definer(*params):
        return Gate(name, n_qubits, params)

    return definer


def _evaluate_argument(expr, parameters):
    """Inner recursive function to calculate the value of a mathematical expression given the
    concrete values in the `parameters` field."""
    if isinstance(expr, ExprConstant):
        return expr.value
    if isinstance(expr, ExprArgument):
        return parameters[expr.index]
    if isinstance(expr, ExprUnary):
        inner = _evaluate_argument(expr.argument, parameters)
        opcode = expr.opcode
        if opcode == UnaryOpCode.Negate:
            return -inner
        if opcode == UnaryOpCode.Cos:
            return math.cos(inner)
        if opcode == UnaryOpCode.Exp:
            return math.exp(inner)
        if opcode == UnaryOpCode.Ln:
            return math.log(inner)
        if opcode == UnaryOpCode.Sin:
            return math.sin(inner)
        if opcode == UnaryOpCode.Sqrt:
            return math.sqrt(inner)
        if opcode == UnaryOpCode.Tan:
            return math.tan(inner)
        raise ValueError(f"unhandled unary opcode: {opcode}")
    if isinstance(expr, ExprBinary):
        left = _evaluate_argument(expr.left, parameters)
        right = _evaluate_argument(expr.right, parameters)
        opcode = expr.opcode
        if opcode == BinaryOpCode.Add:
            return left + right
        if opcode == BinaryOpCode.Subtract:
            return left - right
        if opcode == BinaryOpCode.Multiply:
            return left * right
        if opcode == BinaryOpCode.Divide:
            return left / right
        if opcode == BinaryOpCode.Power:
            return left**right
        raise ValueError(f"unhandled binary opcode: {opcode}")
    raise ValueError(f"unhandled expression type: {expr}")
