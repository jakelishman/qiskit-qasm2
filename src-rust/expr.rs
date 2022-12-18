use core::f64;

use hashbrown::HashMap;

use crate::error::{message_bad_eof, message_from_token, message_incorrect_requirement};
use crate::lex::{Token, TokenStream, TokenType};
use crate::parse::GateSymbol;

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

#[derive(Clone, Copy, Debug)]
enum Atom {
    LParen,
    RParen,
    Function(Function),
    Op(Op),
    Const(f64),
    Parameter(usize),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct ExprId {
    id: usize,
}

#[derive(Debug)]
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

/// A memory arena for holding `Expr` instances for their tree structure.  This is additive only.
/// It only contains `Expr` instances that exactly one expression points to; during constant
/// folding, any involved `Constant` variants will not be added to the arena if they can be eagerly
/// combined into a new `Constant` variant.
pub struct ExprArena {
    exprs: Vec<Expr>,
}

impl ExprArena {
    fn new() -> Self {
        ExprArena { exprs: vec![] }
    }

    fn push(&mut self, expr: Expr) -> ExprId {
        let id = ExprId {
            id: self.exprs.len(),
        };
        self.exprs.push(expr);
        id
    }

    pub fn get(&self, id: ExprId) -> &Expr {
        &self.exprs[id.id]
    }
}

fn prefix_power(op: Op) -> Option<u8> {
    match op {
        Op::Plus | Op::Minus => Some(5),
        _ => None,
    }
}

fn binary_power(op: Op) -> (u8, u8) {
    match op {
        Op::Plus | Op::Minus => (1, 2),
        Op::Multiply | Op::Divide => (3, 4),
        Op::Power => (8, 7),
    }
}

pub struct ExprParser<'a, 'b> {
    pub tokens: &'a mut TokenStream<'b>,
    pub gate_symbols: &'a HashMap<String, GateSymbol>,
    pub arena: ExprArena,
}

impl<'a, 'b> ExprParser<'a, 'b> {
    pub fn new(
        tokens: &'a mut TokenStream<'b>,
        gate_symbols: &'a HashMap<String, GateSymbol>,
    ) -> Self {
        ExprParser {
            tokens,
            gate_symbols,
            arena: ExprArena::new(),
        }
    }

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
            match infix {
                Op::Plus => Ok(Expr::Constant(val_l + val_r)),
                Op::Minus => Ok(Expr::Constant(val_l - val_r)),
                Op::Multiply => Ok(Expr::Constant(val_l * val_r)),
                Op::Divide => Ok(Expr::Constant(val_l / val_r)),
                Op::Power => Ok(Expr::Constant(val_l.powf(*val_r))),
            }
        } else {
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

    fn eval_expression(&mut self, power_min: u8, cause: &Token) -> Result<Expr, String> {
        let (atom, token) = self.next_atom(cause)?;
        let mut lhs = match atom {
            Atom::LParen => {
                let out = self.eval_expression(power_min, cause)?;
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

    pub fn parse_expression(&mut self, cause: &Token) -> Result<Expr, String> {
        self.eval_expression(0, cause)
    }
}
