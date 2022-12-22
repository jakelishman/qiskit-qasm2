# pylint: disable=no-name-in-module,protected-access,import-error

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
    qc = QuantumCircuit()
    qubits = []
    clbits = []
    gates = [lib.UGate, lib.CXGate]
    bc = iter(bytecode)
    for op in bc:
        opcode = op.opcode
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
            qubits += register._bits
            qc.add_register(register)
        elif opcode == OpCode.DeclareCreg:
            name, size = op.operands
            register = ClassicalRegister(size, name)
            clbits += register._bits
            qc.add_register(register)
        elif opcode == OpCode.SpecialInclude:
            gates += QELIB1
        elif opcode == OpCode.DeclareGate:
            name, params, n_qubits = op.operands
            inner_qubits = [Qubit() for _ in [None] * n_qubits]
            inner = QuantumCircuit(inner_qubits)
            for inner_op in bc:
                if inner_op.opcode == OpCode.EndDeclareGate:
                    break
                if inner_op.opcode != OpCode.Gate:
                    raise ValueError(f"invalid operation inside gate: {op}")
                gate_id, parameters, op_qubits = inner_op.operands
                inner._append(
                    CircuitInstruction(
                        gates[gate_id](*parameters), [inner_qubits[q] for q in op_qubits]
                    )
                )
            gates.append(_gate_builder(name, params, inner))
        else:
            raise ValueError(f"invalid operation: {op}")
    return qc


class _DefinedGate(Gate):
    def __init__(self, name, base_definition, parameter_order, params):
        self._base_definition = base_definition
        self._parameter_order = parameter_order
        super().__init__(name, base_definition.num_qubits, list(params))

    def _define(self):
        self._definition = self._base_definition.assign_parameters(
            dict(zip(self._parameter_order, self.params))
        )


def _gate_builder(name, parameter_objects, definition):
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
