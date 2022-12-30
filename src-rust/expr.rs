//! An operator-precedence subparser used by the main parser for handling parameter expressions.
//! Instances of this subparser are intended to only live for as long as it takes to parse a single
//! parameter.

use core::f64;

use hashbrown::HashMap;
use pyo3::prelude::*;

use crate::error::{message_bad_eof, message_from_token, message_incorrect_requirement};
use crate::lex::{Token, TokenStream, TokenType};
use crate::parse::GateSymbol;

/// Enum representation of the builtin OpenQASM 2 functions.  The built-in Qiskit parser adds the
/// inverse trigonometric functions, but these are an extension to the version as given in the
/// arXiv paper describing OpenQASM 2.  This enum is essentially just a subset of the [TokenType]
/// enum, to allow for better pattern-match checking in the Rust compiler.
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Function {
    Cos,
    Exp,
    Ln,
    Sin,
    Sqrt,
    Tan,
}

impl From<TokenType> for Function {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::Cos => Function::Cos,
            TokenType::Exp => Function::Exp,
            TokenType::Ln => Function::Ln,
            TokenType::Sin => Function::Sin,
            TokenType::Sqrt => Function::Sqrt,
            TokenType::Tan => Function::Tan,
            _ => panic!(),
        }
    }
}

/// An operator symbol used in the expression parsing.  This is essentially just a subset of the
/// [TokenType] enum (albeit with resolved names) to allow for better pattern-match semantics in
/// the Rust compiler.
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum Op {
    Plus,
    Minus,
    Multiply,
    Divide,
    Power,
}

impl Op {
    fn text(&self) -> &'static str {
        match self {
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Multiply => "*",
            Self::Divide => "/",
            Self::Power => "^",
        }
    }
}

impl From<TokenType> for Op {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::Plus => Op::Plus,
            TokenType::Minus => Op::Minus,
            TokenType::Asterisk => Op::Multiply,
            TokenType::Slash => Op::Divide,
            TokenType::Caret => Op::Power,
            _ => panic!(),
        }
    }
}

/// An atom of the operator-precendence expression parsing.  This is a stripped-down version of the
/// [Token] and [TokenType] used in the main parser.  We can use a data enum here because we do not
/// need all the expressive flexibility in expecting and accepting many different token types as
/// we do in the main parser; it does not significantly harm legibility to simply do
///
/// ```rust
/// match atom {
///     Atom::Const(val) => (),
///     Atom::Parameter(index) => (),
///     // ...
/// }
/// ```
///
/// where required.
#[derive(Clone, Copy, Debug)]
enum Atom {
    LParen,
    RParen,
    Function(Function),
    Op(Op),
    Const(f64),
    Parameter(usize),
}

/// The type of the lookup index into an [ExprArena].
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct ExprId {
    id: usize,
}

/// A tree representation of parameter expressions in OpenQASM 2.  The expression
/// operator-precedence parser will do complete constant folding on operations that only involve
/// floating-point numbers, so these will simply be evaluated into a `Constant` variant rather than
/// represented in full tree form.  For all operation-like variants, the arguments have the type
/// [ExprId], which is an index into the [ExprArena] to retrieve the actual [Expr] object that
/// should go there.  This is done so the [ExprArena] is the clear owner of all the required
/// expressions, which can be passed around freely.
#[derive(Clone, Debug)]
pub enum Expr {
    Constant(f64),
    Parameter(usize),
    Negate(ExprId),
    Add(ExprId, ExprId),
    Subtract(ExprId, ExprId),
    Multiply(ExprId, ExprId),
    Divide(ExprId, ExprId),
    Power(ExprId, ExprId),
    Function(Function, ExprId),
}

impl Expr {
    /// Convert the given expression into a Python object.  This produces (in the general case) a
    /// Qiskit `ParameterExpression` by using the `Parameter` objects in `params` as the values
    /// indexed by the [Expr::Parameter] variant.
    pub fn to_python(
        &self,
        py: Python,
        arena: &ExprArena,
        params: &[Py<PyAny>],
    ) -> PyResult<Py<PyAny>> {
        match self {
            Expr::Constant(value) => Ok(value.to_object(py)),
            Expr::Parameter(index) => Ok(params[*index].clone()),
            Expr::Negate(id) => arena
                .get(id)
                .to_python(py, arena, params)?
                .call_method0(py, "__neg__"),
            Expr::Add(left, right) => evaluate_binop_python(
                py,
                "add",
                arena.get(left).to_python(py, arena, params)?,
                arena.get(right).to_python(py, arena, params)?,
            ),
            Expr::Subtract(left, right) => evaluate_binop_python(
                py,
                "sub",
                arena.get(left).to_python(py, arena, params)?,
                arena.get(right).to_python(py, arena, params)?,
            ),
            Expr::Multiply(left, right) => evaluate_binop_python(
                py,
                "mul",
                arena.get(left).to_python(py, arena, params)?,
                arena.get(right).to_python(py, arena, params)?,
            ),
            Expr::Divide(left, right) => evaluate_binop_python(
                py,
                "truediv",
                arena.get(left).to_python(py, arena, params)?,
                arena.get(right).to_python(py, arena, params)?,
            ),
            Expr::Power(left, right) => evaluate_binop_python(
                py,
                "pow",
                arena.get(left).to_python(py, arena, params)?,
                arena.get(right).to_python(py, arena, params)?,
            ),
            Expr::Function(func, expr) => {
                evaluate_function_python(py, func, arena.get(expr).to_python(py, arena, params)?)
            }
        }
    }
}

/// Evaluate a binary operator in Python space.  Rather than using the Python magic methods, which
/// need special handling for the left- and right-hand variants (for example, `float.__add__`
/// cannot take a `Parameter`, but `Parameter.__radd__` can), we just use the `operator` module to
/// provide all the necessary logic.
fn evaluate_binop_python(
    py: Python,
    method: &str,
    left: Py<PyAny>,
    right: Py<PyAny>,
) -> PyResult<Py<PyAny>> {
    PyModule::import(py, "operator")?
        .getattr(method)?
        .call1((left, right))
        .map(|any| any.into())
}

/// Evaluate a built-in function call in Python space on a `ParameterExpression`.
fn evaluate_function_python(
    py: Python,
    function: &Function,
    expr: Py<PyAny>,
) -> PyResult<Py<PyAny>> {
    // Functions on constants should already have been folded during the creation of the `Expr`, so
    // we shouldn't be in the situation where we're trying to call `float.sin()` in Python space
    // (which would be an `AttributeError`).
    match function {
        Function::Cos => expr.call_method0(py, "cos"),
        Function::Exp => expr.call_method0(py, "exp"),
        Function::Ln => expr.call_method0(py, "log"),
        Function::Sin => expr.call_method0(py, "sin"),
        Function::Sqrt => expr.call_method0(py, "sqrt"),
        Function::Tan => expr.call_method0(py, "tan"),
    }
}

/// A memory arena for holding `Expr` instances for their tree structure.  This is additive only.
/// It only contains `Expr` instances that exactly one expression points to; during constant
/// folding, any involved `Constant` variants will not be added to the arena if they can be eagerly
/// combined into a new `Constant` variant.
#[derive(Debug)]
pub struct ExprArena {
    exprs: Vec<Expr>,
}

impl ExprArena {
    /// Create a new arena for storing ownership of [Expr] objects.
    fn new() -> Self {
        ExprArena { exprs: vec![] }
    }

    /// Take ownership of the given [Expr], and return the identifier key that can be given to
    /// [ExprArena::get] to retrieve a reference to the expression.
    fn push(&mut self, expr: Expr) -> ExprId {
        let id = ExprId {
            id: self.exprs.len(),
        };
        self.exprs.push(expr);
        id
    }

    /// Get a reference to the [Expr] referred to by the given index.
    pub fn get(&self, id: &ExprId) -> &Expr {
        &self.exprs[id.id]
    }
}

/// Calculate the binding power of an [Op] when used in a prefix position.  Returns [None] if the
/// operation cannot be used in the prefix position.  The binding power is on the same scale as
/// those returned by [binary_power].
fn prefix_power(op: Op) -> Option<u8> {
    match op {
        Op::Plus | Op::Minus => Some(5),
        _ => None,
    }
}

/// Calculate the binding power of an [Op] when used in an infix position.  The differences between
/// left- and right-binding powers represent the associativity of the operation.
fn binary_power(op: Op) -> (u8, u8) {
    // For new binding powers, use the odd number as the "base" and the even number one larger than
    // it to represent the associativity.  Left-associative operators bind more strongly to the
    // operand on their right (i.e. in `a + b + c`, the first `+` binds to the `b` more tightly
    // than the second, so we get the left-associative form), and right-associative operators bind
    // more strongly to the operand of their left.  The separation of using the odd--even pair is
    // so there's no clash between different operator levels, even accounting for the associativity
    // distinction.
    //
    // All powers should be greater than zero; we need zero free to be the base case in the
    // entry-point to the precedence parser.
    match op {
        Op::Plus | Op::Minus => (1, 2),
        Op::Multiply | Op::Divide => (3, 4),
        Op::Power => (8, 7),
    }
}

/// A subparser used to do the operator-precedence part of the parsing for individual parameter
/// expressions.  The main parser creates a new instance of this struct for each expression it
/// expects, and the instance lives only as long as is required to parse that expression, because
/// it takes temporary resposibility for the [TokenStream] that backs the main parser.
pub struct ExprParser<'a, T: std::io::BufRead> {
    pub tokens: &'a mut TokenStream<T>,
    pub gate_symbols: &'a HashMap<String, GateSymbol>,
    pub arena: ExprArena,
}

impl<'a, T: std::io::BufRead> ExprParser<'a, T> {
    /// Create a new instance of the expression parser.  This is a relatively cheap operation; on
    /// its own, it does not require any heap allocations until a parsed [expression][Expr]
    /// actually involves some tree structure.
    pub fn new(
        tokens: &'a mut TokenStream<T>,
        gate_symbols: &'a HashMap<String, GateSymbol>,
    ) -> Self {
        ExprParser {
            tokens,
            gate_symbols,
            arena: ExprArena::new(),
        }
    }

    /// Expect a token of the correct [TokenType].  This is a direct analogue of
    /// [parse::State::expect].  The error variant of the result contains a suitable error message
    /// if the expectation is violated.
    fn expect(
        &mut self,
        expected: TokenType,
        required: &str,
        cause: &Token,
    ) -> Result<Token, String> {
        let token = match self.tokens.next() {
            None => return Err(message_bad_eof(&self.tokens.filename, required, cause)),
            Some(token) => token,
        };
        if token.ttype == expected {
            Ok(token)
        } else {
            Err(message_incorrect_requirement(
                &self.tokens.filename,
                required,
                &token,
            ))
        }
    }

    /// Apply a prefix [Op] to the current [expression][Expr].  If the current expression is a
    /// constant floating-point value the application will be eagerly constant-folded, otherwise
    /// the resulting [Expr] will have a tree structure.
    fn apply_prefix(&mut self, prefix: Op, expr: Expr) -> Result<Expr, String> {
        match prefix {
            Op::Plus => Ok(expr),
            Op::Minus => match expr {
                Expr::Constant(val) => Ok(Expr::Constant(-val)),
                _ => Ok(Expr::Negate(self.arena.push(expr))),
            },
            _ => panic!(),
        }
    }

    /// Apply a binary infix [Op] to the current [expression][Expr].  If both operands have
    /// constant floating-point values the application will be eagerly constant-folded, otherwise
    /// the resulting [Expr] will have a tree structure.
    fn apply_infix(
        &mut self,
        infix: Op,
        lhs: Expr,
        rhs: Expr,
        op_token: &Token,
    ) -> Result<Expr, String> {
        if let (Expr::Constant(val), Op::Divide) = (&rhs, infix) {
            if *val == 0.0 {
                return Err(message_from_token(
                    op_token,
                    "cannot divide by zero",
                    &self.tokens.filename,
                ));
            }
        };
        if let (Expr::Constant(val_l), Expr::Constant(val_r)) = (&lhs, &rhs) {
            // Eagerly constant-fold if possible.
            match infix {
                Op::Plus => Ok(Expr::Constant(val_l + val_r)),
                Op::Minus => Ok(Expr::Constant(val_l - val_r)),
                Op::Multiply => Ok(Expr::Constant(val_l * val_r)),
                Op::Divide => Ok(Expr::Constant(val_l / val_r)),
                Op::Power => Ok(Expr::Constant(val_l.powf(*val_r))),
            }
        } else {
            // If not, we have to build a tree.
            let id_l = self.arena.push(lhs);
            let id_r = self.arena.push(rhs);
            match infix {
                Op::Plus => Ok(Expr::Add(id_l, id_r)),
                Op::Minus => Ok(Expr::Subtract(id_l, id_r)),
                Op::Multiply => Ok(Expr::Multiply(id_l, id_r)),
                Op::Divide => Ok(Expr::Divide(id_l, id_r)),
                Op::Power => Ok(Expr::Power(id_l, id_r)),
            }
        }
    }

    /// Apply a "scientific calculator" built-in function to an [expression][Expr].  If the operand
    /// is a constant, the function will be constant-folded to produce a new constant expression,
    /// otherwise a tree-form [Expr] is returned.
    fn apply_function(
        &mut self,
        func: Function,
        expr: Expr,
        token: &Token,
    ) -> Result<Expr, String> {
        match expr {
            Expr::Constant(val) => match func {
                Function::Cos => Ok(Expr::Constant(val.cos())),
                Function::Exp => Ok(Expr::Constant(val.exp())),
                Function::Ln => {
                    if val > 0.0 {
                        Ok(Expr::Constant(val.ln()))
                    } else {
                        Err(message_from_token(
                            token,
                            &format!(
                                "failure in constant folding: cannot take ln of non-positive {}",
                                val
                            ),
                            &self.tokens.filename,
                        ))
                    }
                }
                Function::Sin => Ok(Expr::Constant(val.sin())),
                Function::Sqrt => {
                    if val > 0.0 {
                        Ok(Expr::Constant(val.sqrt()))
                    } else {
                        Err(message_from_token(
                            token,
                            &format!(
                                "failure in constant folding: cannot take sqrt of non-positive {}",
                                val
                            ),
                            &self.tokens.filename,
                        ))
                    }
                }
                Function::Tan => Ok(Expr::Constant(val.tan())),
            },
            _ => Ok(Expr::Function(func, self.arena.push(expr))),
        }
    }

    /// Convert the given general [Token] into the expression-parser-specific [Atom], if possible.
    /// Not all [Token]s have a corresponding [Atom]; if this is the case, the return value is
    /// `Ok(None)`.  The error variant is returned if the next token is grammatically valid, but
    /// not semantically, such as an identifier for a value of an incorrect type.
    fn try_atom_from_token(&self, token: &Token) -> Result<Option<Atom>, String> {
        match token.ttype {
            TokenType::LParen => Ok(Some(Atom::LParen)),
            TokenType::RParen => Ok(Some(Atom::RParen)),
            TokenType::Minus
            | TokenType::Plus
            | TokenType::Asterisk
            | TokenType::Slash
            | TokenType::Caret => Ok(Some(Atom::Op(token.ttype.into()))),
            TokenType::Cos
            | TokenType::Exp
            | TokenType::Ln
            | TokenType::Sin
            | TokenType::Sqrt
            | TokenType::Tan => Ok(Some(Atom::Function(token.ttype.into()))),
            TokenType::Real => Ok(Some(Atom::Const(token.real(&self.tokens.context)))),
            TokenType::Integer => Ok(Some(Atom::Const(token.int(&self.tokens.context) as f64))),
            TokenType::Pi => Ok(Some(Atom::Const(f64::consts::PI))),
            TokenType::Id => {
                let id = token.text(&self.tokens.context);
                match self.gate_symbols.get(id) {
                    Some(GateSymbol::Parameter { index }) => Ok(Some(Atom::Parameter(*index))),
                    Some(GateSymbol::Qubit { .. }) => Err(message_from_token(
                        token,
                        &format!("'{}' is a gate qubit, not a parameter", id),
                        &self.tokens.filename,
                    )),
                    None => Err(message_from_token(
                        token,
                        &format!("'{}' is not a parameter defined in this scope", id),
                        &self.tokens.filename,
                    )),
                }
            }
            _ => Ok(None),
        }
    }

    /// Peek at the next [Atom] (and backing [Token]) if the next token exists and can be converted
    /// into a valid [Atom].  If it can't, or if we are at the end of the input, the `None` variant
    /// is returned.
    fn peek_atom(&mut self) -> Option<(Atom, Token)> {
        if let Some(&token) = self.tokens.peek() {
            if let Ok(Some(atom)) = self.try_atom_from_token(&token) {
                Some((atom, token))
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Take the next atom from the [TokenStream], retuning both the [Atom] and the [Token].  The
    /// `cause` argument is for the token that caused us to expect an atom here, and is used by the
    /// error-message generation if necessary.
    fn next_atom(&mut self, cause: &Token) -> Result<(Atom, Token), String> {
        if let Some(token) = self.tokens.next() {
            if let Some(atom) = self.try_atom_from_token(&token)? {
                Ok((atom, token))
            } else {
                Err(message_incorrect_requirement(
                    &self.tokens.filename,
                    "a valid expression or ')'",
                    &token,
                ))
            }
        } else {
            Err(message_bad_eof(
                &self.tokens.filename,
                "a valid expression or ')'",
                cause,
            ))
        }
    }

    /// The main recursive worker routing of the operator-precedence parser.  This evaluates a
    /// series of binary infix operators that have binding powers greater than the input
    /// `power_min`, and unary prefixes on the left-hand operand.  For example, if `power_min`
    /// starts out at `2` (such as it would when evaluating the right-hand side of a binary `+`
    /// expression), then as many `*` and `^` operations as appear would be evaluated by this loop,
    /// and its parsing would finish when it saw the next `+` binary operation.  For initial entry,
    /// the `power_min` should be zero.
    fn eval_expression(&mut self, power_min: u8, cause: &Token) -> Result<Expr, String> {
        let (atom, token) = self.next_atom(cause)?;
        // First evaluate the "left-hand side" of a (potential) sequence of binary infix operators.
        // This might be a simple value, a unary operator acting on a value, or a bracketed
        // expression (either the operand of a function, or just plain parentheses).  This can also
        // invoke a recursive call; the parenthesis components feel naturally recursive, and the
        // unary operator component introduces a new precedence level that requires a recursive
        // call to evaluate.
        let mut lhs = match atom {
            Atom::LParen => {
                let out = self.eval_expression(0, cause)?;
                self.expect(TokenType::RParen, "a closing parenthesis", &token)?;
                Ok(out)
            }
            Atom::RParen => {
                if power_min == 0 {
                    Err(message_from_token(
                        &token,
                        "did not find an expected expression",
                        &self.tokens.filename,
                    ))
                } else {
                    Err(message_from_token(
                        &token,
                        "unmatched closing parenthesis",
                        &self.tokens.filename,
                    ))
                }
            }
            Atom::Function(func) => {
                let lparen_token =
                    self.expect(TokenType::LParen, "an opening parenthesis", &token)?;
                let argument = self.eval_expression(0, &token)?;
                self.expect(TokenType::RParen, "a closing parenthesis", &lparen_token)?;
                Ok(self.apply_function(func, argument, &token)?)
            }
            Atom::Op(op) => match prefix_power(op) {
                Some(power) => {
                    let expr = self.eval_expression(power, &token)?;
                    Ok(self.apply_prefix(op, expr)?)
                }
                None => Err(message_from_token(
                    &token,
                    &format!("'{}' is not a valid unary operator", op.text()),
                    &self.tokens.filename,
                )),
            },
            Atom::Const(val) => Ok(Expr::Constant(val)),
            Atom::Parameter(val) => Ok(Expr::Parameter(val)),
        }?;
        // Now loop over a series of infix operators.  We can continue as long as we're just
        // looking at operators that bind more tightly than the `power_min` passed to this
        // function.  Once they're the same power or less, we have to return, because the calling
        // evaluator needs to bind its operator before we move on to the next infix operator.
        while let Some((Atom::Op(op), peeked_token)) = self.peek_atom() {
            let (power_l, power_r) = binary_power(op);
            if power_l < power_min {
                break;
            }
            self.next_atom(cause)?;
            let rhs = self.eval_expression(power_r, &peeked_token)?;
            lhs = self.apply_infix(op, lhs, rhs, &peeked_token)?;
        }
        Ok(lhs)
    }

    /// Parse a single expression completely.  The [ExprArena] storing the expressions used in the
    /// returned [Expr] tree is the arena in the root of [self].  This is the only public entry
    /// point to the operator-precedence parser.
    pub fn parse_expression(&mut self, cause: &Token) -> Result<Expr, String> {
        self.eval_expression(0, cause)
    }
}
