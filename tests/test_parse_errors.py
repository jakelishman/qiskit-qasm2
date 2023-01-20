import enum

import pytest
from qiskit.circuit import Gate, library as lib

import qiskit_qasm2


class T(enum.Enum):
    # This is a deliberately stripped-down list that doesn't include most of the expression-specific
    # tokens, because we don't want to complicate matters with those in tests of the general parser
    # errors.  We test the expression subparser elsewhere.
    OPENQASM = "OPENQASM"
    BARRIER = "barrier"
    CREG = "creg"
    GATE = "gate"
    IF = "if"
    INCLUDE = "include"
    MEASURE = "measure"
    OPAQUE = "opaque"
    QREG = "qreg"
    RESET = "reset"
    PI = "pi"
    ARROW = "->"
    EQUALS = "=="
    SEMICOLON = ";"
    COMMA = ","
    LPAREN = "("
    RPAREN = ")"
    LBRACKET = "["
    RBRACKET = "]"
    LBRACE = "{"
    RBRACE = "}"
    ID = "q"
    REAL = "1.5"
    INTEGER = "1"
    FILENAME = '"qelib1.inc"'


TOKEN_SET = frozenset(T)


class TestIncompleteStructure:
    PRELUDE = "OPENQASM 2.0; qreg q[5]; creg c[5]; creg cond[1];"

    def pytest_generate_tests(self, metafunc):
        if (generate := getattr(self, f"_generate_{metafunc.function.__name__}", None)) is not None:
            generate(metafunc)

    def test_bad_token(self, statement, disallowed):
        """Test that the parser raises an error when an incorrect token is given."""
        prelude = "" if statement.startswith("OPENQASM") else self.PRELUDE
        full = f"{prelude} {statement} {disallowed}"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="needed .*, but instead"):
            qiskit_qasm2.loads(full)

    def _generate_test_bad_token(self, metafunc):
        def without(*tokens):
            return TOKEN_SET - set(tokens)

        params, ids = [], []
        for statement, disallowed in [
            # This should only include stopping points where the next token is somewhat fixed; in
            # places where there's a real decision to be made (such as number of qubits in a gate,
            # or the statement type in a gate body), there should be a better error message.
            #
            # There's a large subset of OQ2 that's reducible to a regular language, so we _could_
            # define that, build a DFA for it, and use that to very quickly generate a complete set
            # of tests.  That would be more complex to read and verify for correctness, though.
            (
                "",
                without(
                    T.OPENQASM,
                    T.ID,
                    T.INCLUDE,
                    T.OPAQUE,
                    T.GATE,
                    T.QREG,
                    T.CREG,
                    T.IF,
                    T.RESET,
                    T.BARRIER,
                    T.MEASURE,
                ),
            ),
            ("OPENQASM", without(T.REAL, T.INTEGER)),
            ("OPENQASM 2.0", without(T.SEMICOLON)),
            ("include", without(T.FILENAME)),
            ('include "qelib1.inc"', without(T.SEMICOLON)),
            ("opaque", without(T.ID)),
            ("opaque bell", without(T.LPAREN, T.ID, T.SEMICOLON)),
            ("opaque bell (", without(T.ID, T.RPAREN)),
            ("opaque bell (a", without(T.COMMA, T.RPAREN)),
            ("opaque bell (a,", without(T.ID, T.RPAREN)),
            ("opaque bell (a, b", without(T.COMMA, T.RPAREN)),
            ("opaque bell (a, b)", without(T.ID, T.SEMICOLON)),
            ("opaque bell (a, b) q1", without(T.COMMA, T.SEMICOLON)),
            ("opaque bell (a, b) q1,", without(T.ID, T.SEMICOLON)),
            ("opaque bell (a, b) q1, q2", without(T.COMMA, T.SEMICOLON)),
            ("gate", without(T.ID)),
            ("gate bell (", without(T.ID, T.RPAREN)),
            ("gate bell (a", without(T.COMMA, T.RPAREN)),
            ("gate bell (a,", without(T.ID, T.RPAREN)),
            ("gate bell (a, b", without(T.COMMA, T.RPAREN)),
            ("gate bell (a, b) q1", without(T.COMMA, T.LBRACE)),
            ("gate bell (a, b) q1,", without(T.ID, T.LBRACE)),
            ("gate bell (a, b) q1, q2", without(T.COMMA, T.LBRACE)),
            ("qreg", without(T.ID)),
            ("qreg reg", without(T.LBRACKET)),
            ("qreg reg[", without(T.INTEGER)),
            ("qreg reg[5", without(T.RBRACKET)),
            ("qreg reg[5]", without(T.SEMICOLON)),
            ("creg", without(T.ID)),
            ("creg reg", without(T.LBRACKET)),
            ("creg reg[", without(T.INTEGER)),
            ("creg reg[5", without(T.RBRACKET)),
            ("creg reg[5]", without(T.SEMICOLON)),
            ("CX", without(T.LPAREN, T.ID, T.SEMICOLON)),
            ("CX(", without(T.PI, T.INTEGER, T.REAL, T.ID, T.LPAREN, T.RPAREN)),
            ("CX()", without(T.ID, T.SEMICOLON)),
            ("CX q", without(T.LBRACKET, T.COMMA, T.SEMICOLON)),
            ("CX q[", without(T.INTEGER)),
            ("CX q[0", without(T.RBRACKET)),
            ("CX q[0]", without(T.COMMA, T.SEMICOLON)),
            ("CX q[0],", without(T.ID, T.SEMICOLON)),
            ("CX q[0], q", without(T.LBRACKET, T.COMMA, T.SEMICOLON)),
            # No need to repeatedly "every" possible number of arguments.
            ("measure", without(T.ID)),
            ("measure q", without(T.LBRACKET, T.ARROW)),
            ("measure q[", without(T.INTEGER)),
            ("measure q[0", without(T.RBRACKET)),
            ("measure q[0]", without(T.ARROW)),
            ("measure q[0] ->", without(T.ID)),
            ("measure q[0] -> c", without(T.LBRACKET, T.SEMICOLON)),
            ("measure q[0] -> c[", without(T.INTEGER)),
            ("measure q[0] -> c[0", without(T.RBRACKET)),
            ("measure q[0] -> c[0]", without(T.SEMICOLON)),
            ("reset", without(T.ID)),
            ("reset q", without(T.LBRACKET, T.SEMICOLON)),
            ("reset q[", without(T.INTEGER)),
            ("reset q[0", without(T.RBRACKET)),
            ("reset q[0]", without(T.SEMICOLON)),
            ("barrier", without(T.ID, T.SEMICOLON)),
            ("barrier q", without(T.LBRACKET, T.COMMA, T.SEMICOLON)),
            ("barrier q[", without(T.INTEGER)),
            ("barrier q[0", without(T.RBRACKET)),
            ("barrier q[0]", without(T.COMMA, T.SEMICOLON)),
            ("if", without(T.LPAREN)),
            ("if (", without(T.ID)),
            ("if (cond", without(T.EQUALS)),
            ("if (cond ==", without(T.INTEGER)),
            ("if (cond == 0", without(T.RPAREN)),
            ("if (cond == 0)", without(T.ID, T.RESET, T.MEASURE)),
        ]:
            for token in disallowed:
                params.append((statement, token.value))
                ids.append(f"'{statement}'-{token.name.lower()}")
        metafunc.parametrize(["statement", "disallowed"], params, ids=ids)

    def test_eof(self, statement):
        """Test that the parser raises an error when the end-of-file is reached instead of a token
        that is required."""
        prelude = "" if statement.startswith("OPENQASM") else self.PRELUDE
        full = f"{prelude} {statement}"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="unexpected end-of-file"):
            qiskit_qasm2.loads(full)

    def _generate_test_eof(self, metafunc):
        statements = []
        for tokens in [
            ("OPENQASM", "2.0", ";"),
            ("include", '"qelib1.inc"', ";"),
            ("opaque", "bell", "(", "a", ",", "b", ")", "q1", ",", "q2", ";"),
            ("gate", "bell", "(", "a", ",", "b", ")", "q1", ",", "q2", "{", "}"),
            ("qreg", "qr", "[", "5", "]", ";"),
            ("creg", "cr", "[", "5", "]", ";"),
            ("CX", "(", ")", "q", "[", "0", "]", ",", "q", "[", "1", "]", ";"),
            ("measure", "q", "[", "0", "]", "->", "c", "[", "0", "]", ";"),
            ("reset", "q", "[", "0", "]", ";"),
            ("barrier", "q", ";"),
            # No need to test every combination of `if`, really.
            ("if", "(", "cond", "==", "0", ")", "CX q[0], q[1];"),
        ]:
            prefix = ""
            for token in tokens[:-1]:
                prefix = f"{prefix} {token}".strip()
                statements.append(prefix)
        metafunc.parametrize("statement", statements)


def test_invalid_version():
    program = "OPENQASM 3.0;"
    with pytest.raises(qiskit_qasm2.QASM2ParseError, match="can only handle OpenQASM 2.0"):
        qiskit_qasm2.loads(program)

    program = "OPENQASM 2.1;"
    with pytest.raises(qiskit_qasm2.QASM2ParseError, match="can only handle OpenQASM 2.0"):
        qiskit_qasm2.loads(program)

    program = "OPENQASM 20.e-1;"
    with pytest.raises(qiskit_qasm2.QASM2ParseError):
        qiskit_qasm2.loads(program)


def test_openqasm_must_be_first_statement():
    program = "qreg q[0]; OPENQASM 2.0;"
    with pytest.raises(qiskit_qasm2.QASM2ParseError, match="only the first statement"):
        qiskit_qasm2.loads(program)


class TestScoping:
    def test_register_use_before_definition(self):
        program = "CX after[0], after[1]; qreg after[2];"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="not defined in this scope"):
            qiskit_qasm2.loads(program)

        program = "qreg q[2]; measure q[0] -> c[0]; creg c[2];"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="not defined in this scope"):
            qiskit_qasm2.loads(program)

    @pytest.mark.parametrize(
        "definer", ["qreg reg[2];", "creg reg[2];", "gate reg a {}", "opaque reg a;"]
    )
    @pytest.mark.parametrize("bad_definer", ["qreg reg[2];", "creg reg[2];"])
    def test_register_already_defined(self, definer, bad_definer):
        program = f"{definer} {bad_definer}"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="already defined"):
            qiskit_qasm2.loads(program)

    def test_qelib1_not_implicit(self):
        program = """
            OPENQASM 2.0;
            qreg q[2];
            cx q[0], q[1];
        """
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="'cx' is not defined"):
            qiskit_qasm2.loads(program)

    def test_cannot_access_gates_before_definition(self):
        program = """
            qreg q[2];
            cx q[0], q[1];
            gate cx a, b {
                CX a, b;
            }
        """
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="'cx' is not defined"):
            qiskit_qasm2.loads(program)

    def test_cannot_access_gate_recursively(self):
        program = """
            gate cx a, b {
                cx a, b;
            }
        """
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="'cx' is not defined"):
            qiskit_qasm2.loads(program)

    def test_cannot_access_qubits_from_previous_gate(self):
        program = """
            gate cx a, b {
                CX a, b;
            }
            gate other c {
                CX a, b;
            }
        """
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="'a' is not defined"):
            qiskit_qasm2.loads(program)

    def test_cannot_access_parameters_from_previous_gate(self):
        program = """
            gate first(a, b) q {
                U(a, 0, b) q;
            }
            gate second q {
                U(a, 0, b) q;
            }
        """
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="'a' is not a parameter defined"):
            qiskit_qasm2.loads(program)

    def test_cannot_access_quantum_registers_within_gate(self):
        program = """
            qreg q[2];
            gate my_gate a {
                CX a, q;
            }
        """
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="'q' is a quantum register"):
            qiskit_qasm2.loads(program)

    def test_parameters_not_defined_outside_gate(self):
        program = """
            gate my_gate(a) q {}
            qreg qr[2];
            U(a, 0, 0) qr;
        """
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="'a' is not a parameter defined"):
            qiskit_qasm2.loads(program)

    def test_qubits_not_defined_outside_gate(self):
        program = """
            gate my_gate(a) q {}
            U(0, 0, 0) q;
        """
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="'q' is not defined"):
            qiskit_qasm2.loads(program)

    @pytest.mark.parametrize("definer", ['include "qelib1.inc";', "gate h q { }"])
    def test_gates_cannot_redefine(self, definer):
        program = f"{definer} gate h q {{ }}"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="already defined"):
            qiskit_qasm2.loads(program)

    def test_cannot_use_undeclared_register_conditional(self):
        program = "qreg q[1]; if (c == 0) U(0, 0, 0) q[0];"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="not defined"):
            qiskit_qasm2.loads(program)


class TestTyping:
    @pytest.mark.parametrize(
        "usage",
        [
            "CX q[0], U;",
            "measure U -> c[0];",
            "measure q[0] -> U;",
            "reset U;",
            "barrier U;",
            "if (U == 0) CX q[0], q[1];",
            "gate my_gate a { U(0, 0, 0) U; }",
        ],
    )
    def test_cannot_use_gates_incorrectly(self, usage):
        program = f"qreg q[2]; creg c[2]; {usage}"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="'U' is a gate"):
            qiskit_qasm2.loads(program)

    @pytest.mark.parametrize(
        "usage",
        [
            "measure q[0] -> q[1];",
            "if (q == 0) CX q[0], q[1];",
            "q q[0], q[1];",
            "gate my_gate a { U(0, 0, 0) q; }",
        ],
    )
    def test_cannot_use_qregs_incorrectly(self, usage):
        program = f"qreg q[2]; creg c[2]; {usage}"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="'q' is a quantum register"):
            qiskit_qasm2.loads(program)

    @pytest.mark.parametrize(
        "usage",
        [
            "CX q[0], c[1];",
            "measure c[0] -> c[1];",
            "reset c[0];",
            "barrier c[0];",
            "c q[0], q[1];",
            "gate my_gate a { U(0, 0, 0) c; }",
        ],
    )
    def test_cannot_use_cregs_incorrectly(self, usage):
        program = f"qreg q[2]; creg c[2]; {usage}"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="'c' is a classical register"):
            qiskit_qasm2.loads(program)

    def test_cannot_use_parameters_incorrectly(self):
        program = "gate my_gate(p) q { CX p, q; }"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="'p' is a parameter"):
            qiskit_qasm2.loads(program)

    def test_cannot_use_qubits_incorrectly(self):
        program = "gate my_gate(p) q { U(q, q, q) q; }"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="'q' is a gate qubit"):
            qiskit_qasm2.loads(program)

    @pytest.mark.parametrize(
        ["gate", "bad_count"],
        [("h", 0), ("h", 2), ("CX", 0), ("CX", 1), ("CX", 3), ("ccx", 2), ("ccx", 4)],
    )
    def test_gates_accept_only_valid_number_qubits(self, gate, bad_count):
        arguments = ", ".join(f"q[{i}]" for i in range(bad_count))
        program = f'include "qelib1.inc"; qreg q[5];\n{gate} {arguments};'
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="takes .* quantum arguments?"):
            qiskit_qasm2.loads(program)

    @pytest.mark.parametrize(
        ["gate", "bad_count"], [("U", 2), ("U", 4), ("rx", 0), ("rx", 2), ("u3", 1)]
    )
    def test_gates_accept_only_valid_number_parameters(self, gate, bad_count):
        arguments = ", ".join("0" for _ in [None] * bad_count)
        program = f'include "qelib1.inc"; qreg q[5];\n{gate}({arguments}) q[0];'
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="takes .* parameters?"):
            qiskit_qasm2.loads(program)


class TestGateDefinition:
    def test_no_zero_qubit(self):
        program = "gate zero {}"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="gates must act on at least one"):
            qiskit_qasm2.loads(program)

        program = "gate zero(a) {}"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="gates must act on at least one"):
            qiskit_qasm2.loads(program)

    def test_no_zero_qubit_opaque(self):
        program = "opaque zero;"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="gates must act on at least one"):
            qiskit_qasm2.loads(program)

        program = "opaque zero(a);"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="gates must act on at least one"):
            qiskit_qasm2.loads(program)

    def test_cannot_subscript_qubit(self):
        program = """
            gate my_gate a {
                CX a[0], a[1];
            }
        """
        with pytest.raises(qiskit_qasm2.QASM2ParseError):
            qiskit_qasm2.loads(program)

    def test_cannot_repeat_parameters(self):
        program = "gate my_gate(a, a) q {}"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="already defined"):
            qiskit_qasm2.loads(program)

    def test_cannot_repeat_qubits(self):
        program = "gate my_gate a, a {}"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="already defined"):
            qiskit_qasm2.loads(program)

    def test_qubit_cannot_shadow_parameter(self):
        program = "gate my_gate(a) a {}"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="already defined"):
            qiskit_qasm2.loads(program)


class TestBitResolution:
    def test_disallow_out_of_range(self):
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="out-of-range"):
            qiskit_qasm2.loads("qreg q[2]; U(0, 0, 0) q[2];")

        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="out-of-range"):
            qiskit_qasm2.loads("qreg q[2]; creg c[2]; measure q[2] -> c[0];")

        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="out-of-range"):
            qiskit_qasm2.loads("qreg q[2]; creg c[2]; measure q[0] -> c[2];")

    @pytest.mark.parametrize("conditional", [True, False])
    @pytest.mark.parametrize(
        "call",
        [
            "CX q1[0], q1[0];",
            "CX q1, q1[0];",
            "CX q1[0], q1;",
            "CX q1, q1;",
            "ccx q1[0], q1[1], q1[0];",
            "ccx q2, q1, q2[0];",
        ],
    )
    def test_disallow_duplicate_qubits(self, call, conditional):
        program = """
            include "qelib1.inc";
            qreg q1[3];
            qreg q2[3];
            qreg q3[3];
        """
        if conditional:
            program += "creg cond[1]; if (cond == 0) "
        program += call
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="duplicate qubit"):
            qiskit_qasm2.loads(program)

    @pytest.mark.parametrize("conditional", [True, False])
    @pytest.mark.parametrize(
        ["registers", "call"],
        [
            (("q1[1]", "q2[2]"), "CX q1, q2"),
            (("q1[1]", "q2[2]"), "CX q2, q1"),
            (("q1[3]", "q2[2]"), "CX q1, q2"),
            (("q1[2]", "q2[3]", "q3[3]"), "ccx q1, q2, q3"),
            (("q1[2]", "q2[3]", "q3[3]"), "ccx q2, q3, q1"),
            (("q1[2]", "q2[3]", "q3[3]"), "ccx q3, q1, q2"),
            (("q1[2]", "q2[3]", "q3[3]"), "ccx q1, q2[0], q3"),
            (("q1[2]", "q2[3]", "q3[3]"), "ccx q2[0], q3, q1"),
            (("q1[2]", "q2[3]", "q3[3]"), "ccx q3, q1, q2[0]"),
        ],
    )
    def test_incorrect_gate_broadcast_lengths(self, registers, call, conditional):
        cond = "creg cond[1];\nif (cond == 0)" if conditional else ""
        setup = 'include "qelib1.inc";\n' + "\n".join(f"qreg {reg};" for reg in registers)
        program = f"{setup}\n{cond} {call};"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="cannot resolve broadcast"):
            qiskit_qasm2.loads(program)

    @pytest.mark.parametrize("conditional", [True, False])
    @pytest.mark.parametrize(
        ["setup", "operands"],
        [
            ("qreg q[2]; creg c[2];", "q[0] -> c"),
            ("qreg q[2]; creg c[2];", "q -> c[0]"),
            ("qreg q[1]; creg c[2];", "q -> c[0]"),
            ("qreg q[2]; creg c[1];", "q[0] -> c"),
            ("qreg q[2]; creg c[3];", "q -> c"),
        ],
    )
    def test_incorrect_measure_broadcast_lengths(self, setup, operands, conditional):
        cond = "creg cond[1];\nif (cond == 0)" if conditional else ""
        program = f"{setup}\n{cond} measure {operands};"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="cannot resolve broadcast"):
            qiskit_qasm2.loads(program)


class TestCustomInstructions:
    def test_cannot_use_custom_before_definition(self):
        program = "qreg q[2]; my_gate q[0], q[1];"

        class MyGate(Gate):
            def __init__(self):
                super().__init__("my_gate", 2, [])

        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="cannot use .* before definition"):
            qiskit_qasm2.loads(
                program,
                custom_instructions=[qiskit_qasm2.CustomInstruction("my_gate", 0, 2, MyGate)],
            )

    def test_cannot_misdefine_u(self):
        program = "qreg q[1]; U(0.5, 0.25) q[0]"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="custom instruction .* mismatched"):
            qiskit_qasm2.loads(
                program, custom_instructions=[qiskit_qasm2.CustomInstruction("U", 2, 1, lib.U2Gate)]
            )

    def test_cannot_misdefine_cx(self):
        program = "qreg q[1]; CX q[0]"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="custom instruction .* mismatched"):
            qiskit_qasm2.loads(
                program, custom_instructions=[qiskit_qasm2.CustomInstruction("CX", 0, 1, lib.XGate)]
            )

    def test_builtin_is_typechecked(self):
        program = "qreg q[1]; my(0.5) q[0];"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="'my' takes 2 quantum arguments"):
            qiskit_qasm2.loads(
                program,
                custom_instructions=[
                    qiskit_qasm2.CustomInstruction("my", 1, 2, lib.RXXGate, builtin=True)
                ],
            )
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="'my' takes 2 parameters"):
            qiskit_qasm2.loads(
                program,
                custom_instructions=[
                    qiskit_qasm2.CustomInstruction("my", 2, 1, lib.U2Gate, builtin=True)
                ],
            )

    @pytest.mark.parametrize(
        "program", ["gate my(a) q {}", "opaque my(a) q;"], ids=["gate", "opaque"]
    )
    @pytest.mark.parametrize("builtin", [True, False])
    def test_custom_definition_must_match_gate(self, program, builtin):
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="'my' is mismatched"):
            qiskit_qasm2.loads(
                program,
                custom_instructions=[
                    qiskit_qasm2.CustomInstruction("my", 1, 2, lib.RXXGate, builtin=builtin)
                ],
            )
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="'my' is mismatched"):
            qiskit_qasm2.loads(
                program,
                custom_instructions=[
                    qiskit_qasm2.CustomInstruction("my", 2, 1, lib.U2Gate, builtin=builtin)
                ],
            )

    def test_cannot_have_duplicate_customs(self):
        customs = [
            qiskit_qasm2.CustomInstruction("my", 1, 2, lib.RXXGate),
            qiskit_qasm2.CustomInstruction("x", 0, 1, lib.XGate),
            qiskit_qasm2.CustomInstruction("my", 1, 2, lib.RZZGate),
        ]
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="duplicate custom instruction"):
            qiskit_qasm2.loads("", custom_instructions=customs)

    def test_qiskit_delay_float_input_wraps_exception(self):
        program = "opaque delay(t) q; qreg q[1]; delay(1.5) q[0];"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="can only accept an integer"):
            qiskit_qasm2.loads(program, custom_instructions=qiskit_qasm2.QISKIT_CUSTOM_INSTRUCTIONS)
