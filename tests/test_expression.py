import itertools
import math
import sys

import pytest
import qiskit_qasm2


class TestSimple:
    def test_unary_constants(self):
        program = "qreg q[1]; U(-(0.5 + 0.5), +(+1 - 2), -+-+2) q[0];"
        parsed = qiskit_qasm2.loads(program)
        expected = [-1.0, -1.0, 2.0]
        assert list(parsed.data[0].operation.params) == expected

    def test_unary_symbolic(self):
        program = """
            gate u(a, b, c) q {
                U(-(a + a), +(+b - c), -+-+c) q;
            }
            qreg q[1];
            u(0.5, 1.0, 2.0) q[0];
        """
        parsed = qiskit_qasm2.loads(program)
        expected = [-1.0, -1.0, 2.0]
        actual = [float(x) for x in parsed.data[0].operation.definition.data[0].operation.params]
        assert list(actual) == expected

    @pytest.mark.parametrize(
        ["str_op", "py_op"],
        [
            ("+", lambda a, b: a + b),
            ("-", lambda a, b: a - b),
            ("*", lambda a, b: a * b),
            ("/", lambda a, b: a / b),
            ("^", lambda a, b: a**b),
        ],
    )
    def test_binary_constants(self, str_op, py_op):
        program = f"qreg q[1]; U(0.25{str_op}0.5, 1.0{str_op}0.5, 3.2{str_op}-0.8) q[0];"
        parsed = qiskit_qasm2.loads(program)
        expected = [py_op(0.25, 0.5), py_op(1.0, 0.5), py_op(3.2, -0.8)]
        # These should be bit-for-bit exact.
        assert list(parsed.data[0].operation.params) == expected

    @pytest.mark.parametrize(
        ["str_op", "py_op"],
        [
            ("+", lambda a, b: a + b),
            ("-", lambda a, b: a - b),
            ("*", lambda a, b: a * b),
            ("/", lambda a, b: a / b),
            ("^", lambda a, b: a**b),
        ],
    )
    def test_binary_symbolic(self, str_op, py_op):
        program = f"""
            gate u(a, b, c) q {{
                U(a {str_op} b, a {str_op} (b {str_op} c), 0.0) q;
            }}
            qreg q[1];
            u(1.0, 2.0, 3.0) q[0];
        """
        parsed = qiskit_qasm2.loads(program)
        outer = [1.0, 2.0, 3.0]
        abstract_op = parsed.data[0].operation
        assert list(abstract_op.params) == outer
        expected = [py_op(1.0, 2.0), py_op(1.0, py_op(2.0, 3.0)), 0.0]
        actual = [float(x) for x in abstract_op.definition.data[0].operation.params]
        assert list(actual) == expected

    @pytest.mark.parametrize(
        ["function_str", "function_py"],
        [
            ("cos", math.cos),
            ("exp", math.exp),
            ("ln", math.log),
            ("sin", math.sin),
            ("sqrt", math.sqrt),
            ("tan", math.tan),
        ],
    )
    def test_function_constants(self, function_str, function_py):
        program = f"qreg q[1]; U({function_str}(0.5),{function_str}(1.0),{function_str}(pi)) q[0];"
        parsed = qiskit_qasm2.loads(program)
        expected = [function_py(0.5), function_py(1.0), function_py(math.pi)]
        # These should be bit-for-bit exact.
        assert list(parsed.data[0].operation.params) == expected

    @pytest.mark.parametrize(
        ["function_str", "function_py"],
        [
            ("cos", math.cos),
            ("exp", math.exp),
            ("ln", math.log),
            ("sin", math.sin),
            ("sqrt", math.sqrt),
            ("tan", math.tan),
        ],
    )
    def test_function_symbolic(self, function_str, function_py):
        program = f"""
            gate u(a, b, c) q {{
                U({function_str}(a), {function_str}(b), {function_str}(c)) q;
            }}
            qreg q[1];
            u(0.5, 1.0, pi) q[0];
        """
        parsed = qiskit_qasm2.loads(program)
        outer = [0.5, 1.0, math.pi]
        abstract_op = parsed.data[0].operation
        assert list(abstract_op.params) == outer
        expected = [function_py(x) for x in outer]
        actual = [float(x) for x in abstract_op.definition.data[0].operation.params]
        assert list(actual) == expected


def test_precedence():
    # OQ3's precedence rules are the same as Python's, so we can effectively just eval.
    expr = "   1.0 + 2.0 * -3.0  ^ 1.5 - 0.5 / +0.25"
    expected = 1.0 + 2.0 * -(3.0**1.5) - 0.5 / +0.25

    program = f"qreg q[1]; U({expr}, 0, 0) q[0];"
    parsed = qiskit_qasm2.loads(program)
    assert parsed.data[0].operation.params[0] == expected


class TestAssociativity:
    def test_addition_left(self):
        # `eps` is the smallest floating-point value such that `1 + eps != 1`.  That means that if
        # addition is correctly parsed and resolved as left-associative, then the first parameter
        # should first calculate `1 + (eps / 2)`, which will be 1, and then the same again, whereas
        # the second will do `(eps / 2) + (eps / 2) = eps`, then `eps + 1` will be different.
        eps = sys.float_info.epsilon
        program = f"qreg q[1]; U(1 + {eps / 2} + {eps / 2}, {eps / 2} + {eps / 2} + 1, 0) q[0];"
        parsed = qiskit_qasm2.loads(program)
        assert 1.0 + eps != 1.0  # Sanity check for the test.
        assert list(parsed.data[0].operation.params) == [1.0, 1.0 + eps, 0.0]

    def test_multiplication_left(self):
        # A similar principle to the epsilon test for addition; if multiplication associates right,
        # then `(0.5 * 2.0 * fmax)` is `inf`, otherwise it's `fmax`.
        fmax = sys.float_info.max
        program = f"qreg q[1]; U({fmax} * 0.5 * 2.0, 2.0 * 0.5 * {fmax}, 2.0 * {fmax} * 0.5) q[0];"
        parsed = qiskit_qasm2.loads(program)
        assert list(parsed.data[0].operation.params) == [fmax, fmax, math.inf]

    def test_subtraction_left(self):
        # If subtraction associated right, we'd accidentally get 2.
        program = "qreg q[1]; U(2.0 - 1.0 - 1.0, 0, 0) q[0];"
        parsed = qiskit_qasm2.loads(program)
        assert list(parsed.data[0].operation.params) == [0.0, 0.0, 0.0]

    def test_division_left(self):
        # If division associated right, we'd accidentally get 4.
        program = "qreg q[1]; U(4.0 / 2.0 / 2.0, 0, 0) q[0];"
        parsed = qiskit_qasm2.loads(program)
        assert list(parsed.data[0].operation.params) == [1.0, 0.0, 0.0]

    def test_power_right(self):
        # If the power operator associated left, we'd accidentally get 64 instead.
        program = "qreg q[1]; U(2.0 ^ 3.0 ^ 2.0, 0, 0) q[0];"
        parsed = qiskit_qasm2.loads(program)
        assert list(parsed.data[0].operation.params) == [512.0, 0.0, 0.0]


class TestCustomClassical:
    def test_evaluation_order(self):
        """We should be evaluating all functions, including custom user ones the exact number of
        times we expect, and left-to-right in parameter lists."""
        order = itertools.count()

        def f():
            return next(order)

        program = """
            qreg q[1];
            U(f(), 2 * f() + f(), atan2(f(), f()) - f()) q[0];
        """
        parsed = qiskit_qasm2.loads(
            program,
            custom_classical=[
                qiskit_qasm2.CustomClassical("f", 0, f),
                qiskit_qasm2.CustomClassical("atan2", 2, math.atan2),
            ],
        )
        assert list(parsed.data[0].operation.params) == [0, 2 * 1 + 2, math.atan2(3, 4) - 5]
        assert next(order) == 6


class TestErrors:
    @pytest.mark.parametrize("denom", ["0.0", "(1.0 - 1.0)"])
    def test_refuses_to_divide_by_zero(self, denom):
        program = f"qreg q[1]; U(2.0 / {denom}, 0.0, 0.0) q[0];"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="divide by zero"):
            qiskit_qasm2.loads(program)

        program = f"gate rx(a) q {{ U(a / {denom}, 0.0, 0.0) q; }}"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="divide by zero"):
            qiskit_qasm2.loads(program)

    @pytest.mark.parametrize("operand", ["0.0", "1.0 - 1.0", "-2.0", "2.0 - 3.0"])
    def test_refuses_to_ln_non_positive(self, operand):
        program = f"qreg q[1]; U(ln({operand}), 0.0, 0.0) q[0];"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="ln of non-positive"):
            qiskit_qasm2.loads(program)

        program = f"gate rx(a) q {{ U(a + ln({operand}), 0.0, 0.0) q; }}"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="ln of non-positive"):
            qiskit_qasm2.loads(program)

    @pytest.mark.parametrize("operand", ["-2.0", "2.0 - 3.0"])
    def test_refuses_to_sqrt_negative(self, operand):
        program = f"qreg q[1]; U(sqrt({operand}), 0.0, 0.0) q[0];"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="sqrt of negative"):
            qiskit_qasm2.loads(program)

        program = f"gate rx(a) q {{ U(a + sqrt({operand}), 0.0, 0.0) q; }}"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="sqrt of negative"):
            qiskit_qasm2.loads(program)

    @pytest.mark.parametrize("operator", ["*", "/", "^"])
    def test_cannot_use_nonunary_operators_in_unary_position(self, operator):
        program = f"qreg q[1]; U({operator}1.0, 0.0, 0.0) q[0];"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="not a valid unary operator"):
            qiskit_qasm2.loads(program)

    @pytest.mark.parametrize("operator", ["+", "-", "*", "/", "^"])
    def test_missing_binary_operand_errors(self, operator):
        program = f"qreg q[1]; U(1.0 {operator}, 0.0, 0.0) q[0];"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="missing operand"):
            qiskit_qasm2.loads(program)

        program = f"qreg q[1]; U((1.0 {operator}), 0.0, 0.0) q[0];"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="missing operand"):
            qiskit_qasm2.loads(program)

    def test_parenthesis_must_be_clsoed(self):
        program = "qreg q[1]; U((1 + 1 2), 3, 2) q[0];"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="needed a closing parenthesis"):
            qiskit_qasm2.loads(program)

    def test_premature_right_parenthesis(self):
        program = "qreg q[1]; U(sin(), 0.0, 0.0) q[0];"
        with pytest.raises(qiskit_qasm2.QASM2ParseError, match="did not find an .* expression"):
            qiskit_qasm2.loads(program)
