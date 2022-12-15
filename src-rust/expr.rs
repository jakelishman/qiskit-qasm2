use core::f64;

use hashbrown::HashMap;

use crate::error::{message_bad_eof, message_from_token, message_incorrect_requirement};
use crate::lex::{Token, TokenStream, TokenType};
use crate::parse::GateSymbol;

enum Atom {
    Const(f64),
    Parameter(usize),
}

pub struct ExprParser<'a, 'b> {
    pub tokens: &'a mut TokenStream<'b>,
    pub gate_symbols: &'a HashMap<String, GateSymbol>,
}

impl<'a, 'b> ExprParser<'a, 'b> {
    fn parse_atom(&mut self, cause: &Token) -> Result<Atom, String> {
        match self.tokens.next() {
            Some(
                float_token @ Token {
                    ttype: TokenType::Real,
                    ..
                },
            ) => Ok(Atom::Const(float_token.real(&self.tokens.context))),
            Some(
                int_token @ Token {
                    ttype: TokenType::Integer,
                    ..
                },
            ) => Ok(Atom::Const(int_token.int(&self.tokens.context) as f64)),
            Some(Token {
                ttype: TokenType::Pi,
                ..
            }) => Ok(Atom::Const(f64::consts::PI)),
            Some(
                id_token @ Token {
                    ttype: TokenType::Id,
                    ..
                },
            ) => {
                let id = id_token.text(&self.tokens.context);
                match self.gate_symbols.get(id) {
                    Some(GateSymbol::Parameter { index }) => Ok(Atom::Parameter(*index)),
                    Some(GateSymbol::Qubit { .. }) => Err(message_from_token(
                        &id_token,
                        &format!("'{}' is a gate qubit, not a parameter", id),
                        &self.tokens.filename,
                    )),
                    None => Err(message_from_token(
                        &id_token,
                        &format!("'{}' is not a parameter defined in this scope", id),
                        &self.tokens.filename,
                    )),
                }
            }
            Some(other) => Err(message_incorrect_requirement(
                &self.tokens.filename,
                "a parameter  or ')'",
                &other,
            )),
            None => Err(message_bad_eof(
                &self.tokens.filename,
                "a parameter or ')'",
                cause,
            )),
        }
    }

    pub fn fold_constant(&mut self, cause: &Token) -> Result<f64, String> {
        match self.parse_atom(cause)? {
            Atom::Const(x) => Ok(x),
            Atom::Parameter(_) => {
                panic!("Called 'fold_constant' in a context with defined non-constant symbols.")
            }
        }
    }
}
