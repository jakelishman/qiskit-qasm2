# Most of the lexer is fully exercised in the parser tests.  These tests here are really mopping up
# some error messages and whatnot that might otherwise be missed.

import pytest

import qiskit_qasm2


@pytest.mark.parametrize("prefix", ["", "qre", "cre", "."])
def test_non_ascii_bytes_error(prefix):
    token = f"{prefix}\xff"
    with pytest.raises(qiskit_qasm2.QASM2ParseError, match="encountered a non-ASCII byte"):
        qiskit_qasm2.loads(token)


def test_integers_cannot_start_with_zero():
    with pytest.raises(qiskit_qasm2.QASM2ParseError, match="integers cannot have leading zeroes"):
        qiskit_qasm2.loads("0123")


@pytest.mark.parametrize("sign", ["", "+", "-"])
def test_float_exponents_must_have_a_digit(sign):
    token = f"12.34e{sign}"
    with pytest.raises(qiskit_qasm2.QASM2ParseError, match="needed to see an integer exponent"):
        qiskit_qasm2.loads(token)


def test_non_builtins_cannot_be_capitalised():
    with pytest.raises(qiskit_qasm2.QASM2ParseError, match="identifiers cannot start with capital"):
        qiskit_qasm2.loads("Qubit")


def test_unterminated_filename_is_invalid():
    with pytest.raises(
        qiskit_qasm2.QASM2ParseError, match="unexpected end-of-file while lexing string literal"
    ):
        qiskit_qasm2.loads('include "qelib1.inc')


def test_filename_with_linebreak_is_invalid():
    with pytest.raises(
        qiskit_qasm2.QASM2ParseError, match="unexpected line break while lexing string literal"
    ):
        qiskit_qasm2.loads('include "qe\nlib1.inc";')


def test_strict_single_quoted_path_rejected():
    with pytest.raises(
        qiskit_qasm2.QASM2ParseError, match=r"\[strict\] paths must be in double quotes"
    ):
        qiskit_qasm2.loads("OPENQASM 2.0; include 'qelib1.inc';", strict=True)


def test_integers_must_have_word_boundaries_after():
    with pytest.raises(qiskit_qasm2.QASM2ParseError, match=r"expected a word boundary"):
        qiskit_qasm2.loads("OPENQASM 2.0; qreg q[2a];")


def test_floats_must_have_word_boundaries_after():
    with pytest.raises(qiskit_qasm2.QASM2ParseError, match=r"expected a word boundary"):
        qiskit_qasm2.loads("OPENQASM 2.0; qreg q[1]; U(2.0a, 0, 0) q[0];")


def test_single_equals_is_rejected():
    with pytest.raises(qiskit_qasm2.QASM2ParseError, match=r"single equals '=' is never valid"):
        qiskit_qasm2.loads("if (a = 2) U(0, 0, 0) q[0];")


def test_bare_dot_is_not_valid_float():
    with pytest.raises(qiskit_qasm2.QASM2ParseError, match=r"expected a numeric fractional part"):
        qiskit_qasm2.loads("qreg q[0]; U(2 + ., 0, 0) q[0];")

def test_invalid_token():
    with pytest.raises(qiskit_qasm2.QASM2ParseError, match=r"encountered '!', which doesn't match"):
        qiskit_qasm2.loads("!")
