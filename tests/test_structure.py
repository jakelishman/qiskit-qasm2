import io
import math
import pickle

import pytest
from qiskit.circuit import (
    ClassicalRegister,
    Gate,
    Parameter,
    QuantumCircuit,
    QuantumRegister,
    Qubit,
    library as lib,
)
from qiskit import qpy

import qiskit_qasm2

from .conftest import gate_builder


def test_allows_empty():
    assert qiskit_qasm2.loads("") == QuantumCircuit()


class TestVersion:
    def test_complete_version(self):
        program = "OPENQASM 2.0;"
        parsed = qiskit_qasm2.loads(program)
        assert parsed == QuantumCircuit()

    def test_incomplete_version(self):
        program = "OPENQASM 2;"
        parsed = qiskit_qasm2.loads(program)
        assert parsed == QuantumCircuit()

    def test_after_comment(self):
        program = """
            // hello, world
            OPENQASM 2.0;
            qreg q[2];
        """
        parsed = qiskit_qasm2.loads(program)
        qc = QuantumCircuit(QuantumRegister(2, "q"))
        assert parsed == qc


class TestRegisters:
    def test_qreg(self):
        program = "qreg q1[2]; qreg q2[1]; qreg q3[4];"
        parsed = qiskit_qasm2.loads(program)
        regs = [QuantumRegister(2, "q1"), QuantumRegister(1, "q2"), QuantumRegister(4, "q3")]
        assert list(parsed.qregs) == regs
        assert list(parsed.cregs) == []

    def test_creg(self):
        program = "creg c1[2]; creg c2[1]; creg c3[4];"
        parsed = qiskit_qasm2.loads(program)
        regs = [ClassicalRegister(2, "c1"), ClassicalRegister(1, "c2"), ClassicalRegister(4, "c3")]
        assert list(parsed.cregs) == regs
        assert list(parsed.qregs) == []

    def test_interleaved_registers(self):
        program = "qreg q1[3]; creg c1[2]; qreg q2[1]; creg c2[1];"
        parsed = qiskit_qasm2.loads(program)
        qregs = [QuantumRegister(3, "q1"), QuantumRegister(1, "q2")]
        cregs = [ClassicalRegister(2, "c1"), ClassicalRegister(1, "c2")]
        assert list(parsed.qregs) == qregs
        assert list(parsed.cregs) == cregs

    def test_registers_after_gate(self):
        program = "qreg before[2]; CX before[0], before[1]; qreg after[2]; CX after[0], after[1];"
        parsed = qiskit_qasm2.loads(program)
        before = QuantumRegister(2, "before")
        after = QuantumRegister(2, "after")
        qc = QuantumCircuit(before, after)
        qc.cx(before[0], before[1])
        qc.cx(after[0], after[1])
        assert parsed == qc

    def test_empty_registers(self):
        program = "qreg q[0]; creg c[0];"
        parsed = qiskit_qasm2.loads(program)
        qc = QuantumCircuit(QuantumRegister(0, "q"), ClassicalRegister(0, "c"))
        assert parsed == qc


class TestGateApplication:
    def test_builtin_single(self):
        program = """
            qreg q[2];
            U(0, 0, 0) q[0];
            CX q[0], q[1];
        """
        parsed = qiskit_qasm2.loads(program)
        qc = QuantumCircuit(QuantumRegister(2, "q"))
        qc.u(0, 0, 0, 0)
        qc.cx(0, 1)
        assert parsed == qc

    def test_builtin_1q_broadcast(self):
        program = "qreg q[2]; U(0, 0, 0) q;"
        parsed = qiskit_qasm2.loads(program)
        qc = QuantumCircuit(QuantumRegister(2, "q"))
        qc.u(0, 0, 0, 0)
        qc.u(0, 0, 0, 1)
        assert parsed == qc

    def test_builtin_2q_broadcast(self):
        program = """
            qreg q1[2];
            qreg q2[2];
            CX q1[0], q2;
            barrier;
            CX q1, q2[1];
            barrier;
            CX q1, q2;
        """
        parsed = qiskit_qasm2.loads(program)
        q1 = QuantumRegister(2, "q1")
        q2 = QuantumRegister(2, "q2")
        qc = QuantumCircuit(q1, q2)
        qc.cx(q1[0], q2[0])
        qc.cx(q1[0], q2[1])
        qc.barrier()
        qc.cx(q1[0], q2[1])
        qc.cx(q1[1], q2[1])
        qc.barrier()
        qc.cx(q1[0], q2[0])
        qc.cx(q1[1], q2[1])
        assert parsed == qc

    def test_3q_broadcast(self):
        program = """
            include "qelib1.inc";
            qreg q1[2];
            qreg q2[2];
            qreg q3[2];

            ccx q1, q2[0], q3[1];
            ccx q1[1], q2, q3[0];
            ccx q1[0], q2[1], q3;
            barrier;

            ccx q1, q2, q3[1];
            ccx q1[1], q2, q3;
            ccx q1, q2[1], q3;
            barrier;

            ccx q1, q2, q3;
        """
        parsed = qiskit_qasm2.loads(program)
        q1 = QuantumRegister(2, "q1")
        q2 = QuantumRegister(2, "q2")
        q3 = QuantumRegister(2, "q3")
        qc = QuantumCircuit(q1, q2, q3)
        qc.ccx(q1[0], q2[0], q3[1])
        qc.ccx(q1[1], q2[0], q3[1])
        qc.ccx(q1[1], q2[0], q3[0])
        qc.ccx(q1[1], q2[1], q3[0])
        qc.ccx(q1[0], q2[1], q3[0])
        qc.ccx(q1[0], q2[1], q3[1])
        qc.barrier()
        qc.ccx(q1[0], q2[0], q3[1])
        qc.ccx(q1[1], q2[1], q3[1])
        qc.ccx(q1[1], q2[0], q3[0])
        qc.ccx(q1[1], q2[1], q3[1])
        qc.ccx(q1[0], q2[1], q3[0])
        qc.ccx(q1[1], q2[1], q3[1])
        qc.barrier()
        qc.ccx(q1[0], q2[0], q3[0])
        qc.ccx(q1[1], q2[1], q3[1])
        assert parsed == qc

    @pytest.mark.parametrize("conditioned", [True, False], ids=["conditioned", "bare"])
    def test_broadcast_against_empty_register(self, conditioned):
        cond = "if (cond == 0) " if conditioned else ""
        program = f"""
            OPENQASM 2;
            include "qelib1.inc";
            qreg q1[1];
            qreg q2[1];
            qreg empty1[0];
            qreg empty2[0];
            qreg empty3[0];
            creg cond[1];

            // None of the following statements should produce any gate applications.
            {cond}h empty1;

            {cond}cx q1[0], empty1;
            {cond}cx empty1, q2[0];
            {cond}cx empty1, empty2;

            {cond}ccx empty1, q1[0], q2[0];
            {cond}ccx q1[0], empty2, q2[0];
            {cond}ccx q1[0], q2[0], empty3;

            {cond}ccx empty1, empty2, q1[0];
            {cond}ccx empty1, q1[0], empty2;
            {cond}ccx q1[0], empty1, empty2;

            {cond}ccx empty1, empty2, empty3;
        """
        parsed = qiskit_qasm2.loads(program)
        qc = QuantumCircuit(
            QuantumRegister(1, "q1"),
            QuantumRegister(1, "q2"),
            QuantumRegister(0, "empty1"),
            QuantumRegister(0, "empty2"),
            QuantumRegister(0, "empty3"),
            ClassicalRegister(1, "cond"),
        )
        assert parsed == qc

    def test_conditioned(self):
        program = """
            qreg q[2];
            creg cond[1];
            if (cond == 0) U(0, 0, 0) q[0];
            if (cond == 1) CX q[1], q[0];
        """
        parsed = qiskit_qasm2.loads(program)
        cond = ClassicalRegister(1, "cond")
        qc = QuantumCircuit(QuantumRegister(2, "q"), cond)
        qc.u(0, 0, 0, 0).c_if(cond, 0)
        qc.cx(1, 0).c_if(cond, 1)
        assert parsed == qc

    def test_conditioned_broadcast(self):
        program = """
            qreg q1[2];
            qreg q2[2];
            creg cond[1];
            if (cond == 0) U(0, 0, 0) q1;
            if (cond == 1) CX q1[0], q2;
        """
        parsed = qiskit_qasm2.loads(program)
        cond = ClassicalRegister(1, "cond")
        q1 = QuantumRegister(2, "q1")
        q2 = QuantumRegister(2, "q2")
        qc = QuantumCircuit(q1, q2, cond)
        qc.u(0, 0, 0, q1[0]).c_if(cond, 0)
        qc.u(0, 0, 0, q1[1]).c_if(cond, 0)
        qc.cx(q1[0], q2[0]).c_if(cond, 1)
        qc.cx(q1[0], q2[1]).c_if(cond, 1)
        assert parsed == qc

    def test_constant_folding(self):
        # Most expression-related things are tested in `test_expression.py` instead.
        program = """
            qreg q[1];
            U(4 + 3 * 2 ^ 2, cos(pi) * (1 - ln(1)), 2 ^ 3 ^ 2) q[0];
        """
        parsed = qiskit_qasm2.loads(program)
        qc = QuantumCircuit(QuantumRegister(1, "q"))
        qc.u(16.0, -1.0, 512.0, 0)
        assert parsed == qc

    def test_call_defined_gate(self):
        program = """
            gate my_gate a {
                U(0, 0, 0) a;
            }
            qreg q[2];
            my_gate q[0];
            my_gate q;
        """
        parsed = qiskit_qasm2.loads(program)
        my_gate_def = QuantumCircuit([Qubit()])
        my_gate_def.u(0, 0, 0, 0)
        my_gate = gate_builder("my_gate", [], my_gate_def)
        qc = QuantumCircuit(QuantumRegister(2, "q"))
        qc.append(my_gate(), [0])
        qc.append(my_gate(), [0])
        qc.append(my_gate(), [1])
        assert parsed == qc

    def test_parameterless_gates_accept_parentheses(self):
        program = """
            qreg q[2];
            CX q[0], q[1];
            CX() q[1], q[0];
        """
        parsed = qiskit_qasm2.loads(program)
        qc = QuantumCircuit(QuantumRegister(2, "q"))
        qc.cx(0, 1)
        qc.cx(1, 0)
        assert parsed == qc


class TestGateDefinition:
    def test_simple_definition(self):
        program = """
            gate not_bell a, b {
                U(0, 0, 0) a;
                CX a, b;
            }
            qreg q[2];
            not_bell q[0], q[1];
        """
        parsed = qiskit_qasm2.loads(program)
        not_bell_def = QuantumCircuit([Qubit(), Qubit()])
        not_bell_def.u(0, 0, 0, 0)
        not_bell_def.cx(0, 1)
        not_bell = gate_builder("not_bell", [], not_bell_def)
        qc = QuantumCircuit(QuantumRegister(2, "q"))
        qc.append(not_bell(), [0, 1])
        assert parsed == qc

    def test_conditioned(self):
        program = """
            gate not_bell a, b {
                U(0, 0, 0) a;
                CX a, b;
            }
            qreg q[2];
            creg cond[1];
            if (cond == 0) not_bell q[0], q[1];
        """
        parsed = qiskit_qasm2.loads(program)
        not_bell_def = QuantumCircuit([Qubit(), Qubit()])
        not_bell_def.u(0, 0, 0, 0)
        not_bell_def.cx(0, 1)
        not_bell = gate_builder("not_bell", [], not_bell_def)
        cond = ClassicalRegister(1, "cond")
        qc = QuantumCircuit(QuantumRegister(2, "q"), cond)
        qc.append(not_bell().c_if(cond, 0), [0, 1])
        assert parsed == qc

    def test_constant_folding_in_definition(self):
        program = """
            gate bell a, b {
                U(pi/2, 0, pi) a;
                CX a, b;
            }
            qreg q[2];
            bell q[0], q[1];
        """
        parsed = qiskit_qasm2.loads(program)
        bell_def = QuantumCircuit([Qubit(), Qubit()])
        bell_def.u(math.pi / 2, 0, math.pi, 0)
        bell_def.cx(0, 1)
        bell = gate_builder("bell", [], bell_def)
        qc = QuantumCircuit(QuantumRegister(2, "q"))
        qc.append(bell(), [0, 1])
        assert parsed == qc

    def test_parameterised_gate(self):
        # Most of the tests of deep parameter expressions are in `test_expression.py`.
        program = """
            gate my_gate(a, b) c {
                U(a, b, a + 2 * b) c;
            }
            qreg q[1];
            my_gate(0.25, 0.5) q[0];
            my_gate(0.5, 0.25) q[0];
        """
        parsed = qiskit_qasm2.loads(program)
        a, b = Parameter("a"), Parameter("b")
        my_gate_def = QuantumCircuit([Qubit()])
        my_gate_def.u(a, b, a + 2 * b, 0)
        my_gate = gate_builder("my_gate", [a, b], my_gate_def)
        qc = QuantumCircuit(QuantumRegister(1, "q"))
        qc.append(my_gate(0.25, 0.5), [0])
        qc.append(my_gate(0.5, 0.25), [0])
        assert parsed == qc

        # Also check the decomposition has come out exactly as expected.  The floating-point
        # assertions are safe as exact equality checks because there are no lossy operations with
        # these parameters, and the answer should be exact.
        decomposed = qc.decompose()
        assert decomposed.data[0].operation.name == "u"
        assert list(decomposed.data[0].operation.params) == [0.25, 0.5, 1.25]
        assert decomposed.data[1].operation.name == "u"
        assert list(decomposed.data[1].operation.params) == [0.5, 0.25, 1.0]

    def test_parameterless_gate_with_parentheses(self):
        program = """
            gate my_gate() a {
                U(0, 0, 0) a;
            }
            qreg q[1];
            my_gate q[0];
            my_gate() q[0];
        """
        parsed = qiskit_qasm2.loads(program)
        my_gate_def = QuantumCircuit([Qubit()])
        my_gate_def.u(0, 0, 0, 0)
        my_gate = gate_builder("my_gate", [], my_gate_def)
        qc = QuantumCircuit(QuantumRegister(1, "q"))
        qc.append(my_gate(), [0])
        qc.append(my_gate(), [0])
        assert parsed == qc

    def test_access_includes_in_definition(self):
        program = """
            include "qelib1.inc";
            gate bell a, b {
                h a;
                cx a, b;
            }
            qreg q[2];
            bell q[0], q[1];
        """
        parsed = qiskit_qasm2.loads(program)
        bell_def = QuantumCircuit([Qubit(), Qubit()])
        bell_def.h(0)
        bell_def.cx(0, 1)
        bell = gate_builder("bell", [], bell_def)
        qc = QuantumCircuit(QuantumRegister(2, "q"))
        qc.append(bell(), [0, 1])
        assert parsed == qc

    def test_access_previous_defined_gate(self):
        program = """
            include "qelib1.inc";
            gate bell a, b {
                h a;
                cx a, b;
            }
            gate second_bell a, b {
                bell b, a;
            }
            qreg q[2];
            second_bell q[0], q[1];
        """
        parsed = qiskit_qasm2.loads(program)
        bell_def = QuantumCircuit([Qubit(), Qubit()])
        bell_def.h(0)
        bell_def.cx(0, 1)
        bell = gate_builder("bell", [], bell_def)

        second_bell_def = QuantumCircuit([Qubit(), Qubit()])
        second_bell_def.append(bell(), [1, 0])
        second_bell = gate_builder("second_bell", [], second_bell_def)

        qc = QuantumCircuit(QuantumRegister(2, "q"))
        qc.append(second_bell(), [0, 1])
        assert parsed == qc

    def test_qubits_lookup_differently_to_gates(self):
        # The spec is somewhat unclear on this, and this leads to super weird text, but it's
        # technically unambiguously resolvable and this is more permissive.
        program = """
            include "qelib1.inc";
            gate bell h, cx {
                h h;
                cx h, cx;
            }
            qreg q[2];
            bell q[0], q[1];
        """
        parsed = qiskit_qasm2.loads(program)
        bell_def = QuantumCircuit([Qubit(), Qubit()])
        bell_def.h(0)
        bell_def.cx(0, 1)
        bell = gate_builder("bell", [], bell_def)
        qc = QuantumCircuit(QuantumRegister(2, "q"))
        qc.append(bell(), [0, 1])
        assert parsed == qc

    def test_parameters_lookup_differently_to_gates(self):
        # The spec is somewhat unclear on this, and this leads to super weird text, but it's
        # technically unambiguously resolvable and this is more permissive.
        program = """
            include "qelib1.inc";
            gate shadow(rx, rz) a {
                rz(rz) a;
                rx(rx) a;
            }
            qreg q[1];
            shadow(0.5, 2.0) q[0];
        """
        parsed = qiskit_qasm2.loads(program)
        rx, rz = Parameter("rx"), Parameter("rz")
        shadow_def = QuantumCircuit([Qubit()])
        shadow_def.rz(rz, 0)
        shadow_def.rx(rx, 0)
        shadow = gate_builder("shadow", [rx, rz], shadow_def)
        qc = QuantumCircuit(QuantumRegister(1, "q"))
        qc.append(shadow(0.5, 2.0), [0])
        assert parsed == qc

    def test_unused_parameters_convert_correctly(self):
        # The main risk here is that there might be lazy application in the gate definition
        # bindings, and we might accidentally try and bind parameters that aren't actually in the
        # definition.
        program = """
            gate my_gate(p) q {
                U(0, 0, 0) q;
            }
            qreg q[1];
            my_gate(0.5) q[0];
        """
        parsed = qiskit_qasm2.loads(program)
        # No top-level circuit equality test here, because all the internals of gate application are
        # an implementation detail, and we don't want to tie the tests and implementation together
        # too closely.
        assert list(parsed.qregs) == [QuantumRegister(1, "q")]
        assert list(parsed.cregs) == []
        assert len(parsed.data) == 1
        assert parsed.data[0].qubits == (parsed.qubits[0],)
        assert parsed.data[0].clbits == ()
        assert parsed.data[0].operation.name == "my_gate"
        assert list(parsed.data[0].operation.params) == [0.5]

        decomposed = QuantumCircuit(QuantumRegister(1, "q"))
        decomposed.u(0, 0, 0, 0)
        assert parsed.decompose() == decomposed

    def test_qubit_barrier_in_definition(self):
        program = """
            gate my_gate a, b {
                barrier a;
                barrier b;
                barrier a, b;
            }
            qreg q[2];
            my_gate q[0], q[1];
        """
        parsed = qiskit_qasm2.loads(program)
        my_gate_def = QuantumCircuit([Qubit(), Qubit()])
        my_gate_def.barrier(0)
        my_gate_def.barrier(1)
        my_gate_def.barrier([0, 1])
        my_gate = gate_builder("my_gate", [], my_gate_def)
        qc = QuantumCircuit(QuantumRegister(2, "q"))
        qc.append(my_gate(), [0, 1])
        assert parsed == qc

    def test_bare_barrier_in_definition(self):
        program = """
            gate my_gate a, b {
                barrier;
            }
            qreg q[2];
            my_gate q[0], q[1];
        """
        parsed = qiskit_qasm2.loads(program)
        my_gate_def = QuantumCircuit([Qubit(), Qubit()])
        my_gate_def.barrier(my_gate_def.qubits)
        my_gate = gate_builder("my_gate", [], my_gate_def)
        qc = QuantumCircuit(QuantumRegister(2, "q"))
        qc.append(my_gate(), [0, 1])
        assert parsed == qc

    def test_duplicate_barrier_in_definition(self):
        program = """
            gate my_gate a, b {
                barrier a, a;
                barrier b, a, b;
            }
            qreg q[2];
            my_gate q[0], q[1];
        """
        parsed = qiskit_qasm2.loads(program)
        my_gate_def = QuantumCircuit([Qubit(), Qubit()])
        my_gate_def.barrier(0)
        my_gate_def.barrier([1, 0])
        my_gate = gate_builder("my_gate", [], my_gate_def)
        qc = QuantumCircuit(QuantumRegister(2, "q"))
        qc.append(my_gate(), [0, 1])
        assert parsed == qc

    def test_pickleable(self):
        program = """
            include "qelib1.inc";
            gate my_gate(a) b, c {
                rz(2 * a) b;
                h b;
                cx b, c;
            }
            qreg q[2];
            my_gate(0.5) q[0], q[1];
            my_gate(0.25) q[1], q[0];
        """
        parsed = qiskit_qasm2.loads(program)
        a = Parameter("a")
        my_gate_def = QuantumCircuit([Qubit(), Qubit()])
        my_gate_def.rz(2 * a, 0)
        my_gate_def.h(0)
        my_gate_def.cx(0, 1)
        my_gate = gate_builder("my_gate", [a], my_gate_def)
        qc = QuantumCircuit(QuantumRegister(2, "q"))
        qc.append(my_gate(0.5), [0, 1])
        qc.append(my_gate(0.25), [1, 0])
        assert parsed == qc
        with io.BytesIO() as f:
            pickle.dump(parsed, f)
            f.seek(0)
            loaded = pickle.load(f)
        assert parsed == loaded

    def test_qpy_single_call_roundtrip(self):
        program = """
            include "qelib1.inc";
            gate my_gate(a) b, c {
                rz(2 * a) b;
                h b;
                cx b, c;
            }
            qreg q[2];
            my_gate(0.5) q[0], q[1];
        """
        parsed = qiskit_qasm2.loads(program)

        # QPY won't persist custom gates by design choice, so instead let us check against the
        # explicit form it uses.
        my_gate_def = QuantumCircuit([Qubit(), Qubit()])
        my_gate_def.rz(1.0, 0)
        my_gate_def.h(0)
        my_gate_def.cx(0, 1)
        my_gate = Gate("my_gate", 2, [0.5])
        my_gate.definition = my_gate_def
        qc = QuantumCircuit(QuantumRegister(2, "q"))
        qc.append(my_gate, [0, 1])

        with io.BytesIO() as f:
            qpy.dump(parsed, f)
            f.seek(0)
            loaded = qpy.load(f)[0]
        assert loaded == qc

    @pytest.mark.xfail(reason="Terra bug, see https://github.com/Qiskit/qiskit-terra/issues/8941")
    def test_qpy_double_call_roundtrip(self):
        program = """
            include "qelib1.inc";
            gate my_gate(a) b, c {
                rz(2 * a) b;
                h b;
                cx b, c;
            }
            qreg q[2];
            my_gate(0.5) q[0], q[1];
            my_gate(0.25) q[1], q[0];
        """
        parsed = qiskit_qasm2.loads(program)

        my_gate1_def = QuantumCircuit([Qubit(), Qubit()])
        my_gate1_def.rz(1.0, 0)
        my_gate1_def.h(0)
        my_gate1_def.cx(0, 1)
        my_gate1 = Gate("my_gate", 2, [0.5])
        my_gate1.definition = my_gate1_def

        my_gate2_def = QuantumCircuit([Qubit(), Qubit()])
        my_gate2_def.rz(0.5, 0)
        my_gate2_def.h(0)
        my_gate2_def.cx(0, 1)
        my_gate2 = Gate("my_gate", 2, [0.25])
        my_gate2.definition = my_gate2_def

        qc = QuantumCircuit(QuantumRegister(2, "q"))
        qc.append(my_gate1, [0, 1])
        qc.append(my_gate2, [1, 0])

        with io.BytesIO() as f:
            qpy.dump(parsed, f)
            f.seek(0)
            loaded = qpy.load(f)[0]
        assert loaded == qc


class TestOpaque:
    def test_simple(self):
        program = """
            opaque my_gate a;
            opaque my_gate2() a;
            qreg q[2];
            my_gate q[0];
            my_gate() q[1];
            my_gate2 q[0];
            my_gate2() q[1];
        """
        parsed = qiskit_qasm2.loads(program)
        qc = QuantumCircuit(QuantumRegister(2, "q"))
        qc.append(Gate("my_gate", 1, []), [0])
        qc.append(Gate("my_gate", 1, []), [1])
        qc.append(Gate("my_gate2", 1, []), [0])
        qc.append(Gate("my_gate2", 1, []), [1])
        assert parsed == qc

    def test_parameterised(self):
        program = """
            opaque my_gate(a, b) c, d;
            qreg q[2];
            my_gate(0.5, 0.25) q[1], q[0];
        """
        parsed = qiskit_qasm2.loads(program)
        qc = QuantumCircuit(QuantumRegister(2, "q"))
        qc.append(Gate("my_gate", 2, [0.5, 0.25]), [1, 0])
        assert parsed == qc


class TestBarrier:
    def test_single_register_argument(self):
        program = """
            qreg first[3];
            qreg second[3];
            barrier first;
            barrier second;
        """
        parsed = qiskit_qasm2.loads(program)
        first = QuantumRegister(3, "first")
        second = QuantumRegister(3, "second")
        qc = QuantumCircuit(first, second)
        qc.barrier(first)
        qc.barrier(second)
        assert parsed == qc

    def test_single_qubit_argument(self):
        program = """
            qreg first[3];
            qreg second[3];
            barrier first[1];
            barrier second[0];
        """
        parsed = qiskit_qasm2.loads(program)
        first = QuantumRegister(3, "first")
        second = QuantumRegister(3, "second")
        qc = QuantumCircuit(first, second)
        qc.barrier(first[1])
        qc.barrier(second[0])
        assert parsed == qc

    def test_empty_circuit_empty_arguments(self):
        program = "barrier;"
        parsed = qiskit_qasm2.loads(program)
        qc = QuantumCircuit()
        assert parsed == qc

    def test_one_register_circuit_empty_arguments(self):
        program = "qreg q1[2]; barrier;"
        parsed = qiskit_qasm2.loads(program)
        qc = QuantumCircuit(QuantumRegister(2, "q1"))
        qc.barrier(qc.qubits)
        assert parsed == qc

    def test_multi_register_circuit_empty_arguments(self):
        program = "qreg q1[2]; qreg q2[3]; qreg q3[1]; barrier;"
        parsed = qiskit_qasm2.loads(program)
        qc = QuantumCircuit(
            QuantumRegister(2, "q1"), QuantumRegister(3, "q2"), QuantumRegister(1, "q3")
        )
        qc.barrier(qc.qubits)
        assert parsed == qc

    def test_include_empty_register(self):
        program = """
            qreg q[2];
            qreg empty[0];
            barrier empty;
            barrier q, empty;
            barrier;
        """
        parsed = qiskit_qasm2.loads(program)
        q = QuantumRegister(2, "q")
        qc = QuantumCircuit(q, QuantumRegister(0, "empty"))
        qc.barrier(q)
        qc.barrier(qc.qubits)
        assert parsed == qc

    def test_allows_duplicate_arguments(self):
        # There's nothing in the paper that implies this should be forbidden.
        program = """
            qreg q1[3];
            qreg q2[2];
            barrier q1, q1;
            barrier q1[0], q1;
            barrier q1, q1[0];
            barrier q1, q2, q1;
        """
        parsed = qiskit_qasm2.loads(program)
        q1 = QuantumRegister(3, "q1")
        q2 = QuantumRegister(2, "q2")
        qc = QuantumCircuit(q1, q2)
        qc.barrier(q1)
        qc.barrier(q1)
        qc.barrier(q1)
        qc.barrier(q1, q2)
        assert parsed == qc


class TestMeasure:
    def test_single(self):
        program = """
            qreg q[1];
            creg c[1];
            measure q[0] -> c[0];
        """
        parsed = qiskit_qasm2.loads(program)
        qc = QuantumCircuit(QuantumRegister(1, "q"), ClassicalRegister(1, "c"))
        qc.measure(0, 0)
        assert parsed == qc

    def test_broadcast(self):
        program = """
            qreg q[2];
            creg c[2];
            measure q -> c;
        """
        parsed = qiskit_qasm2.loads(program)
        qc = QuantumCircuit(QuantumRegister(2, "q"), ClassicalRegister(2, "c"))
        qc.measure(0, 0)
        qc.measure(1, 1)
        assert parsed == qc

    def test_conditioned(self):
        program = """
            qreg q[2];
            creg c[2];
            creg cond[1];
            if (cond == 0) measure q[0] -> c[0];
            if (cond == 1) measure q -> c;
        """
        parsed = qiskit_qasm2.loads(program)
        cond = ClassicalRegister(1, "cond")
        qc = QuantumCircuit(QuantumRegister(2, "q"), ClassicalRegister(2, "c"), cond)
        qc.measure(0, 0).c_if(cond, 0)
        qc.measure(0, 0).c_if(cond, 1)
        qc.measure(1, 1).c_if(cond, 1)
        assert parsed == qc

    def test_broadcast_against_empty_register(self):
        program = """
            qreg q_empty[0];
            creg c_empty[0];
            measure q_empty -> c_empty;
        """
        parsed = qiskit_qasm2.loads(program)
        qc = QuantumCircuit(QuantumRegister(0, "q_empty"), ClassicalRegister(0, "c_empty"))
        assert parsed == qc

    def test_conditioned_broadcast_against_empty_register(self):
        program = """
            qreg q_empty[0];
            creg c_empty[0];
            creg cond[1];
            if (cond == 0) measure q_empty -> c_empty;
        """
        parsed = qiskit_qasm2.loads(program)
        qc = QuantumCircuit(
            QuantumRegister(0, "q_empty"),
            ClassicalRegister(0, "c_empty"),
            ClassicalRegister(1, "cond"),
        )
        assert parsed == qc


class TestReset:
    def test_single(self):
        program = """
            qreg q[1];
            reset q[0];
        """
        parsed = qiskit_qasm2.loads(program)
        qc = QuantumCircuit(QuantumRegister(1, "q"))
        qc.reset(0)
        assert parsed == qc

    def test_broadcast(self):
        program = """
            qreg q[2];
            reset q;
        """
        parsed = qiskit_qasm2.loads(program)
        qc = QuantumCircuit(QuantumRegister(2, "q"))
        qc.reset(0)
        qc.reset(1)
        assert parsed == qc

    def test_conditioned(self):
        program = """
            qreg q[2];
            creg cond[1];
            if (cond == 0) reset q[0];
            if (cond == 1) reset q;
        """
        parsed = qiskit_qasm2.loads(program)
        cond = ClassicalRegister(1, "cond")
        qc = QuantumCircuit(QuantumRegister(2, "q"), cond)
        qc.reset(0).c_if(cond, 0)
        qc.reset(0).c_if(cond, 1)
        qc.reset(1).c_if(cond, 1)
        assert parsed == qc

    def test_broadcast_against_empty_register(self):
        program = """
            qreg empty[0];
            reset empty;
        """
        parsed = qiskit_qasm2.loads(program)
        qc = QuantumCircuit(QuantumRegister(0, "empty"))
        assert parsed == qc

    def test_conditioned_broadcast_against_empty_register(self):
        program = """
            qreg empty[0];
            creg cond[1];
            if (cond == 0) reset empty;
        """
        parsed = qiskit_qasm2.loads(program)
        qc = QuantumCircuit(QuantumRegister(0, "empty"), ClassicalRegister(1, "cond"))
        assert parsed == qc


class TestInclude:
    def test_qelib1_include(self):
        program = """
            include "qelib1.inc";
            qreg q[3];
            u3(0.5, 0.25, 0.125) q[0];
            u2(0.5, 0.25) q[0];
            u1(0.5) q[0];
            cx q[0], q[1];
            id q[0];
            x q[0];
            y q[0];
            z q[0];
            h q[0];
            s q[0];
            sdg q[0];
            t q[0];
            tdg q[0];
            rx(0.5) q[0];
            ry(0.5) q[0];
            rz(0.5) q[0];
            cz q[0], q[1];
            cy q[0], q[1];
            ch q[0], q[1];
            ccx q[0], q[1], q[2];
            crz(0.5) q[0], q[1];
            cu1(0.5) q[0], q[1];
            cu3(0.5, 0.25, 0.125) q[0], q[1];
        """
        parsed = qiskit_qasm2.loads(program)
        qc = QuantumCircuit(QuantumRegister(3, "q"))
        qc.append(lib.U3Gate(0.5, 0.25, 0.125), [0])
        qc.append(lib.U2Gate(0.5, 0.25), [0])
        qc.append(lib.U1Gate(0.5), [0])
        qc.append(lib.CXGate(), [0, 1])
        qc.append(lib.UGate(0, 0, 0), [0])  # Stand-in for id.
        qc.append(lib.XGate(), [0])
        qc.append(lib.YGate(), [0])
        qc.append(lib.ZGate(), [0])
        qc.append(lib.HGate(), [0])
        qc.append(lib.SGate(), [0])
        qc.append(lib.SdgGate(), [0])
        qc.append(lib.TGate(), [0])
        qc.append(lib.TdgGate(), [0])
        qc.append(lib.RXGate(0.5), [0])
        qc.append(lib.RYGate(0.5), [0])
        qc.append(lib.RZGate(0.5), [0])
        qc.append(lib.CZGate(), [0, 1])
        qc.append(lib.CYGate(), [0, 1])
        qc.append(lib.CHGate(), [0, 1])
        qc.append(lib.CCXGate(), [0, 1, 2])
        qc.append(lib.CRZGate(0.5), [0, 1])
        qc.append(lib.CU1Gate(0.5), [0, 1])
        qc.append(lib.CU3Gate(0.5, 0.25, 0.125), [0, 1])
        assert parsed == qc

    def test_qelib1_after_gate_definition(self):
        program = """
            gate bell a, b {
                U(pi/2, 0, pi) a;
                CX a, b;
            }
            include "qelib1.inc";
            qreg q[2];
            bell q[0], q[1];
            rx(0.5) q[0];
            bell q[1], q[0];
        """
        parsed = qiskit_qasm2.loads(program)
        bell_def = QuantumCircuit([Qubit(), Qubit()])
        bell_def.u(math.pi / 2, 0, math.pi, 0)
        bell_def.cx(0, 1)
        bell = gate_builder("bell", [], bell_def)

        qc = QuantumCircuit(QuantumRegister(2, "q"))
        qc.append(bell(), [0, 1])
        qc.rx(0.5, 0)
        qc.append(bell(), [1, 0])
        assert parsed == qc

    def test_include_can_define_version(self, tmp_path):
        include = """
            OPENQASM 2.0;
            qreg inner_q[2];
        """
        with open(tmp_path / "include.qasm", "w") as fp:
            fp.write(include)
        program = """
            OPENQASM 2.0;
            include "include.qasm";
        """
        parsed = qiskit_qasm2.loads(program, include_path=(tmp_path,))
        qc = QuantumCircuit(QuantumRegister(2, "inner_q"))
        assert parsed == qc

    def test_can_define_gates(self, tmp_path):
        include = """
            gate bell a, b {
                h a;
                cx a, b;
            }
        """
        with open(tmp_path / "include.qasm", "w") as fp:
            fp.write(include)
        program = """
            OPENQASM 2.0;
            include "qelib1.inc";
            include "include.qasm";
            qreg q[2];
            bell q[0], q[1];
        """
        parsed = qiskit_qasm2.loads(program, include_path=(tmp_path,))
        bell_def = QuantumCircuit([Qubit(), Qubit()])
        bell_def.h(0)
        bell_def.cx(0, 1)
        bell = gate_builder("bell", [], bell_def)

        qc = QuantumCircuit(QuantumRegister(2, "q"))
        qc.append(bell(), [0, 1])
        assert parsed == qc

    def test_nested_include(self, tmp_path):
        inner = "creg c[2];"
        with open(tmp_path / "inner.qasm", "w") as fp:
            fp.write(inner)
        outer = """
            qreg q[2];
            include "inner.qasm";
        """
        with open(tmp_path / "outer.qasm", "w") as fp:
            fp.write(outer)
        program = """
            OPENQASM 2.0;
            include "outer.qasm";
        """
        parsed = qiskit_qasm2.loads(program, include_path=(tmp_path,))
        qc = QuantumCircuit(QuantumRegister(2, "q"), ClassicalRegister(2, "c"))
        assert parsed == qc

    def test_first_hit_is_used(self, tmp_path):
        empty = tmp_path / "empty"
        empty.mkdir()
        first = tmp_path / "first"
        first.mkdir()
        with open(first / "include.qasm", "w") as fp:
            fp.write("qreg q[1];")
        second = tmp_path / "second"
        second.mkdir()
        with open(second / "include.qasm", "w") as fp:
            fp.write("qreg q[2];")
        program = 'include "include.qasm";'
        parsed = qiskit_qasm2.loads(program, include_path=(empty, first, second))
        qc = QuantumCircuit(QuantumRegister(1, "q"))
        assert parsed == qc

    def test_qelib1_ignores_search_path(self, tmp_path):
        with open(tmp_path / "qelib1.inc", "w") as fp:
            fp.write("qreg not_used[2];")
        program = 'include "qelib1.inc";'
        parsed = qiskit_qasm2.loads(program, include_path=(tmp_path,))
        qc = QuantumCircuit()
        assert parsed == qc

    def test_include_from_current_directory(self, tmp_path, monkeypatch):
        include = """
            qreg q[2];
        """
        with open(tmp_path / "include.qasm", "w") as fp:
            fp.write(include)
        program = """
            OPENQASM 2.0;
            include "include.qasm";
        """
        monkeypatch.chdir(tmp_path)
        parsed = qiskit_qasm2.loads(program)
        qc = QuantumCircuit(QuantumRegister(2, "q"))
        assert parsed == qc

    def test_load_searches_source_directory(self, tmp_path):
        with open(tmp_path / "include.qasm", "w") as fp:
            fp.write("qreg q[2];")
        program = 'include "include.qasm";'
        with open(tmp_path / "program.qasm", "w") as fp:
            fp.write(program)
        parsed = qiskit_qasm2.load(tmp_path / "program.qasm")
        qc = QuantumCircuit(QuantumRegister(2, "q"))
        assert parsed == qc

    def test_load_searches_source_directory_last(self, tmp_path):
        first = tmp_path / "first"
        first.mkdir()
        with open(first / "include.qasm", "w") as fp:
            fp.write("qreg q[2];")
        with open(tmp_path / "include.qasm", "w") as fp:
            fp.write("qreg not_used[2];")
        program = 'include "include.qasm";'
        with open(tmp_path / "program.qasm", "w") as fp:
            fp.write(program)
        parsed = qiskit_qasm2.load(tmp_path / "program.qasm", include_path=(first,))
        qc = QuantumCircuit(QuantumRegister(2, "q"))
        assert parsed == qc

    def test_load_searches_source_directory_prepend(self, tmp_path):
        first = tmp_path / "first"
        first.mkdir()
        with open(first / "include.qasm", "w") as fp:
            fp.write("qreg not_used[2];")
        with open(tmp_path / "include.qasm", "w") as fp:
            fp.write("qreg q[2];")
        program = 'include "include.qasm";'
        with open(tmp_path / "program.qasm", "w") as fp:
            fp.write(program)
        parsed = qiskit_qasm2.load(
            tmp_path / "program.qasm", include_path=(first,), include_input_directory="prepend"
        )
        qc = QuantumCircuit(QuantumRegister(2, "q"))
        assert parsed == qc

    def test_load_can_ignore_source_directory(self, tmp_path):
        with open(tmp_path / "include.qasm", "w") as fp:
            fp.write("qreg q[2];")
        program = 'include "include.qasm";'
        with open(tmp_path / "program.qasm", "w") as fp:
            fp.write(program)
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="unable to find 'include.qasm'"):
            qiskit_qasm2.load(tmp_path / "program.qasm", include_input_directory=None)


class TestCustomInstructions:
    def test_qelib1_include_overridden(self):
        program = """
            include "qelib1.inc";
            qreg q[3];
            u3(0.5, 0.25, 0.125) q[0];
            u2(0.5, 0.25) q[0];
            u1(0.5) q[0];
            cx q[0], q[1];
            id q[0];
            x q[0];
            y q[0];
            z q[0];
            h q[0];
            s q[0];
            sdg q[0];
            t q[0];
            tdg q[0];
            rx(0.5) q[0];
            ry(0.5) q[0];
            rz(0.5) q[0];
            cz q[0], q[1];
            cy q[0], q[1];
            ch q[0], q[1];
            ccx q[0], q[1], q[2];
            crz(0.5) q[0], q[1];
            cu1(0.5) q[0], q[1];
            cu3(0.5, 0.25, 0.125) q[0], q[1];
        """
        parsed = qiskit_qasm2.loads(
            program, custom_instructions=qiskit_qasm2.QISKIT_CUSTOM_INSTRUCTIONS
        )
        qc = QuantumCircuit(QuantumRegister(3, "q"))
        qc.append(lib.U3Gate(0.5, 0.25, 0.125), [0])
        qc.append(lib.U2Gate(0.5, 0.25), [0])
        qc.append(lib.U1Gate(0.5), [0])
        qc.append(lib.CXGate(), [0, 1])
        qc.append(lib.IGate(), [0])
        qc.append(lib.XGate(), [0])
        qc.append(lib.YGate(), [0])
        qc.append(lib.ZGate(), [0])
        qc.append(lib.HGate(), [0])
        qc.append(lib.SGate(), [0])
        qc.append(lib.SdgGate(), [0])
        qc.append(lib.TGate(), [0])
        qc.append(lib.TdgGate(), [0])
        qc.append(lib.RXGate(0.5), [0])
        qc.append(lib.RYGate(0.5), [0])
        qc.append(lib.RZGate(0.5), [0])
        qc.append(lib.CZGate(), [0, 1])
        qc.append(lib.CYGate(), [0, 1])
        qc.append(lib.CHGate(), [0, 1])
        qc.append(lib.CCXGate(), [0, 1, 2])
        qc.append(lib.CRZGate(0.5), [0, 1])
        qc.append(lib.CU1Gate(0.5), [0, 1])
        qc.append(lib.CU3Gate(0.5, 0.25, 0.125), [0, 1])
        assert parsed == qc

        # Also test that the output matches what Qiskit puts out.
        from_qiskit = QuantumCircuit.from_qasm_str(program)
        assert parsed == from_qiskit

    def test_qiskit_extra_builtins(self):
        program = """
            qreg q[5];
            csx q[0], q[1];
            cu(0.5, 0.25, 0.125, 0.0625) q[0], q[1];
            rxx(0.5) q[0], q[1];
            rzz(0.5) q[0], q[1];
            rccx q[0], q[1], q[2];
            rc3x q[0], q[1], q[2], q[3];
            c3x q[0], q[1], q[2], q[3];
            c3sqrtx q[0], q[1], q[2], q[3];
            c4x q[0], q[1], q[2], q[3], q[4];
        """
        parsed = qiskit_qasm2.loads(
            program, custom_instructions=qiskit_qasm2.QISKIT_CUSTOM_INSTRUCTIONS
        )
        qc = QuantumCircuit(QuantumRegister(5, "q"))
        qc.append(lib.CSXGate(), [0, 1])
        qc.append(lib.CUGate(0.5, 0.25, 0.125, 0.0625), [0, 1])
        qc.append(lib.RXXGate(0.5), [0, 1])
        qc.append(lib.RZZGate(0.5), [0, 1])
        qc.append(lib.RCCXGate(), [0, 1, 2])
        qc.append(lib.RC3XGate(), [0, 1, 2, 3])
        qc.append(lib.C3XGate(), [0, 1, 2, 3])
        qc.append(lib.C3SXGate(), [0, 1, 2, 3])
        qc.append(lib.C4XGate(), [0, 1, 2, 3, 4])
        assert parsed == qc

    def test_qiskit_override_delay_opaque(self):
        program = """
            opaque delay(t) q;
            qreg q[1];
            delay(1) q[0];
        """
        parsed = qiskit_qasm2.loads(
            program, custom_instructions=qiskit_qasm2.QISKIT_CUSTOM_INSTRUCTIONS
        )
        qc = QuantumCircuit(QuantumRegister(1, "q"))
        qc.delay(1, 0, unit="dt")
        assert parsed == qc

    def test_can_override_u(self):
        program = """
            qreg q[1];
            U(0.5, 0.25, 0.125) q[0];
        """

        class MyGate(Gate):
            def __init__(self, a, b, c):
                super().__init__("u", 1, [a, b, c])

        parsed = qiskit_qasm2.loads(
            program,
            custom_instructions=[qiskit_qasm2.CustomInstruction("U", 3, 1, MyGate, builtin=True)],
        )
        qc = QuantumCircuit(QuantumRegister(1, "q"))
        qc.append(MyGate(0.5, 0.25, 0.125), [0])
        assert parsed == qc

    def test_can_override_cx(self):
        program = """
            qreg q[2];
            CX q[0], q[1];
        """

        class MyGate(Gate):
            def __init__(self):
                super().__init__("cx", 2, [])

        parsed = qiskit_qasm2.loads(
            program,
            custom_instructions=[qiskit_qasm2.CustomInstruction("CX", 0, 2, MyGate, builtin=True)],
        )
        qc = QuantumCircuit(QuantumRegister(2, "q"))
        qc.append(MyGate(), [0, 1])
        assert parsed == qc

    @pytest.mark.parametrize("order", [lambda x: x, reversed], ids=["forwards", "backwards"])
    def test_can_override_both_builtins_with_other_gates(self, order):
        program = """
            gate unimportant q {}
            qreg q[2];
            U(0.5, 0.25, 0.125) q[0];
            CX q[0], q[1];
        """

        class MyUGate(Gate):
            def __init__(self, a, b, c):
                super().__init__("u", 1, [a, b, c])

        class MyCXGate(Gate):
            def __init__(self):
                super().__init__("cx", 2, [])

        custom = [
            qiskit_qasm2.CustomInstruction("unused", 0, 1, lambda: Gate("unused", 1, [])),
            qiskit_qasm2.CustomInstruction("U", 3, 1, MyUGate, builtin=True),
            qiskit_qasm2.CustomInstruction("CX", 0, 2, MyCXGate, builtin=True),
        ]
        custom = order(custom)
        parsed = qiskit_qasm2.loads(program, custom_instructions=custom)
        qc = QuantumCircuit(QuantumRegister(2, "q"))
        qc.append(MyUGate(0.5, 0.25, 0.125), [0])
        qc.append(MyCXGate(), [0, 1])
        assert parsed == qc

    def test_custom_builtin_gate(self):
        program = """
            qreg q[1];
            builtin(0.5) q[0];
        """

        class MyGate(Gate):
            def __init__(self, a):
                super().__init__("builtin", 1, [a])

        parsed = qiskit_qasm2.loads(
            program,
            custom_instructions=[
                qiskit_qasm2.CustomInstruction("builtin", 1, 1, MyGate, builtin=True)
            ],
        )
        qc = QuantumCircuit(QuantumRegister(1, "q"))
        qc.append(MyGate(0.5), [0])
        assert parsed == qc

    def test_can_define_builtin_as_gate(self):
        program = """
            qreg q[1];
            gate builtin(t) q {}
            builtin(0.5) q[0];
        """

        class MyGate(Gate):
            def __init__(self, a):
                super().__init__("builtin", 1, [a])

        parsed = qiskit_qasm2.loads(
            program,
            custom_instructions=[
                qiskit_qasm2.CustomInstruction("builtin", 1, 1, MyGate, builtin=True)
            ],
        )
        qc = QuantumCircuit(QuantumRegister(1, "q"))
        qc.append(MyGate(0.5), [0])
        assert parsed == qc

    def test_can_define_builtin_as_opaque(self):
        program = """
            qreg q[1];
            opaque builtin(t) q;
            builtin(0.5) q[0];
        """

        class MyGate(Gate):
            def __init__(self, a):
                super().__init__("builtin", 1, [a])

        parsed = qiskit_qasm2.loads(
            program,
            custom_instructions=[
                qiskit_qasm2.CustomInstruction("builtin", 1, 1, MyGate, builtin=True)
            ],
        )
        qc = QuantumCircuit(QuantumRegister(1, "q"))
        qc.append(MyGate(0.5), [0])
        assert parsed == qc

    def test_can_define_custom_as_gate(self):
        program = """
            qreg q[1];
            gate my_gate(t) q {}
            my_gate(0.5) q[0];
        """

        class MyGate(Gate):
            def __init__(self, a):
                super().__init__("my_gate", 1, [a])

        parsed = qiskit_qasm2.loads(
            program, custom_instructions=[qiskit_qasm2.CustomInstruction("my_gate", 1, 1, MyGate)]
        )
        qc = QuantumCircuit(QuantumRegister(1, "q"))
        qc.append(MyGate(0.5), [0])
        assert parsed == qc

    def test_can_define_custom_as_opaque(self):
        program = """
            qreg q[1];
            opaque my_gate(t) q;
            my_gate(0.5) q[0];
        """

        class MyGate(Gate):
            def __init__(self, a):
                super().__init__("my_gate", 1, [a])

        parsed = qiskit_qasm2.loads(
            program, custom_instructions=[qiskit_qasm2.CustomInstruction("my_gate", 1, 1, MyGate)]
        )
        qc = QuantumCircuit(QuantumRegister(1, "q"))
        qc.append(MyGate(0.5), [0])
        assert parsed == qc
