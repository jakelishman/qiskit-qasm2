# pylint: disable=no-name-in-module,protected-access,import-error

from qiskit.circuit import (
    QuantumCircuit,
    QuantumRegister,
    ClassicalRegister,
    Measure,
    Reset,
    Barrier,
    CircuitInstruction,
    library as lib,
)
from .core import OpCode, bytecode_from_string

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


def loads(string):
    qc = QuantumCircuit()
    qubits = []
    clbits = []
    gates = [lib.UGate, lib.CXGate]
    bc = iter(bytecode_from_string(string))
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
        else:
            raise ValueError(f"invalid operation: {op}")
    return qc
