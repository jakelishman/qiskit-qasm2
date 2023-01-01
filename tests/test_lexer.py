# Most of the lexer is fully exercised in the parser tests.  These tests here are really mopping up
# some error messages and whatnot that might otherwise be missed.

import pytest

import qiskit_qasm2


@pytest.mark.parametrize("prefix", ["", "qre", "cre", "=", "."])
def test_non_ascii_bytes_error(prefix):
    token = f"{prefix}\xff"
    with pytest.raises(qiskit_qasm2.QASM2ParseError, match="invalid token during lexing"):
        qiskit_qasm2.loads(token)


def test_integers_cannot_start_with_zero():
    with pytest.raises(qiskit_qasm2.QASM2ParseError, match="invalid token during lexing"):
        qiskit_qasm2.loads("0123")


@pytest.mark.parametrize("sign", ["", "+", "-"])
def test_float_exponents_must_have_a_digit(sign):
    token = f"12.34e{sign}"
    with pytest.raises(qiskit_qasm2.QASM2ParseError, match="invalid token during lexing"):
        qiskit_qasm2.loads(token)


def test_non_builtins_cannot_be_capitalised():
    with pytest.raises(qiskit_qasm2.QASM2ParseError, match="invalid token during lexing"):
        qiskit_qasm2.loads("Qubit")


def test_unterminated_filename_is_invalid():
    with pytest.raises(qiskit_qasm2.QASM2ParseError, match="invalid token during lexing"):
        qiskit_qasm2.loads('include "qelib1.inc')


def test_filename_with_linebreak_is_invalid():
    with pytest.raises(qiskit_qasm2.QASM2ParseError, match="invalid token during lexing"):
        qiskit_qasm2.loads('include "qe\nlib1.inc";')
