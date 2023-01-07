//! The core of the parsing algorithm.  This module contains the core logic for the
//! recursive-descent parser, which handles all statements of OpenQASM 2.  In places where we have
//! to evaluate a mathematical expression on parameters, we instead swap to a short-lived
//! operator-precedence parser.

use hashbrown::{HashMap, HashSet};

use crate::bytecode::InternalBytecode;
use crate::error::{message_bad_eof, message_from_token, message_incorrect_requirement};
use crate::expr::{Expr, ExprParser};
use crate::lex::{Token, TokenStream, TokenType, Version};

/// The number of gates that are built in to the OpenQASM 2 language.  This is U and CX.
const N_BUILTIN_GATES: usize = 2;
/// The number of gates that 'qelib1.inc' defines.  For efficiency, this parser doesn't actually
/// parse the include file itself, since its contents are fixed by the OpenQASM 2 arXiv paper,
/// which this parser follows to the letter.  Instead, we just have it hardcoded as a special
/// import that happens quickly when the relevant 'include' statement is seen.
const N_QELIB1_GATES: usize = 23;

/// A symbol in the global symbol table.  Parameters and individual qubits can't be in the global
/// symbol table, as there is no way for them to be defined.
enum GlobalSymbol {
    Qreg {
        size: usize,
        start: usize,
    },
    Creg {
        size: usize,
        start: usize,
        index: usize,
    },
    Gate {
        n_params: usize,
        n_qubits: usize,
        index: usize,
    },
}

/// A symbol in the scope of a single gate definition.  This only includes the symbols that are
/// specifically gate-scoped.  The rest are part of [GlobalSymbol].
pub enum GateSymbol {
    Qubit { index: usize },
    Parameter { index: usize },
}

/// An operand for an instruction.  This can be both quantum or classical.  Classical operands only
/// occur in the `measure` operation.  `Single` variants are what we mostly expect to see; these
/// happen in gate definitions (always), and in regular applications when registers are indexed.
/// The `Range` operand only occurs when a register is used as an operation target.
enum Operand {
    Single(usize),
    Range(usize, usize),
}

/// The available types for the arrays of parameters in a gate call.  The `Constant` variant is for
/// general applications, whereas the more general `Expression` variant is for gate bodies, where
/// there might be mathematics occurring on symbolic parameters.  We have the special case for the
/// far more common `Constant` form; in this case we immediately unwrap the result of the
/// `ExprParser`, and we won't have to make a new vector with the conversion later.
enum GateParameters {
    Constant(Vec<f64>),
    Expression(Vec<Expr>),
}

/// An equality condition from an `if` statement.  These can condition gate applications, measures
/// and resets, although in practice they're basically only ever used on gates.
struct Condition {
    creg: usize,
    value: usize,
}

/// The state of the parser (but not its output).  This struct is opaque to the rest of the
/// program; only its associated functions ever need to modify its internals.  The counts of
/// qubits, clbits, classical registers and gates are necessary to efficiently assign index keys to
/// new symbols as they arise.  We don't need to track quantum registers like this because no part
/// of the output instruction set requires a reference to a quantum register, since we resolve any
/// broadcast gate applications from within Rust.
pub struct State<T: std::io::BufRead> {
    tokens: TokenStream<T>,
    /// Mapping of name to global-scoped symbols.
    symbols: HashMap<String, GlobalSymbol>,
    /// Mapping of name to gate-scoped symbols.  This object only logically lasts for the duration
    /// of the parsing of one gate definition, but we can save allocations by re-using the same
    /// object between calls.
    gate_symbols: HashMap<String, GateSymbol>,
    n_qubits: usize,
    n_clbits: usize,
    n_cregs: usize,
    n_gates: usize,
    /// Whether the parser has been called at all.
    called: bool,
}

impl<T: std::io::BufRead> State<T> {
    /// Create and initialise a state for the parser.
    pub fn new(tokens: TokenStream<T>) -> Self {
        let mut state = State {
            tokens,
            // For Qiskit-created circuits, all files will have the builtin gates and `qelib1.inc`,
            // so we allocate with that in mind.
            symbols: HashMap::with_capacity(N_BUILTIN_GATES + N_QELIB1_GATES),
            gate_symbols: HashMap::new(),
            n_qubits: 0,
            n_clbits: 0,
            n_cregs: 0,
            n_gates: 0,
            called: false,
        };
        let dummy_token = Token::dummy();
        state.define_gate(&dummy_token, "U".into(), 3, 1).unwrap();
        state.define_gate(&dummy_token, "CX".into(), 0, 2).unwrap();
        state
    }

    /// Take a token from the stream that is known to be present and correct, generally because it
    /// has already been peeked.  Panics if the token type is not correct.
    fn expect_known(&mut self, expected: TokenType) -> Token {
        let out = self.tokens.next().unwrap();
        if out.ttype != expected {
            panic!()
        }
        out
    }

    /// Take the next token from the stream, expecting that it is of a particular type because it
    /// is required to be in order for the input program to be valid OpenQASM 2.  This returns the
    /// token if successful, and a suitable error message if the token type is incorrect, or the
    /// end of the file is reached.
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

    /// Take the next token from the stream, if it is of the correct type.  Returns `None` and
    /// leaves the next token in the underlying iterator if it does not match.
    fn accept(&mut self, expected: TokenType) -> Option<Token> {
        let peeked = self.tokens.peek();
        if peeked.is_some() && peeked.unwrap().ttype == expected {
            self.tokens.next()
        } else {
            None
        }
    }

    /// True if the next token in the stream matches the given type, and false if it doesn't.
    fn next_is(&mut self, expected: TokenType) -> bool {
        let peeked = self.tokens.peek();
        peeked.is_some() && peeked.unwrap().ttype == expected
    }

    /// Take a complete quantum argument from the token stream, if the next token is an identifier.
    /// This includes resolving any following subscript operation.  Returns an error variant if the
    /// next token _is_ an identifier, but the symbol represents something other than a quantum
    /// register, or isn't defined.  This can also be an error if the subscript is opened, but
    /// cannot be completely resolved due to a typing error or other invalid parse.  `Ok(None)` is
    /// returned if the next token in the stream does not match a possible quantum argument.
    fn accept_qarg(&mut self) -> Result<Option<Operand>, String> {
        let (name, name_token) = match self.accept(TokenType::Id) {
            None => return Ok(None),
            Some(token) => (token.id(&self.tokens.context), token),
        };
        let (register_size, register_start) = match self.symbols.get(&name) {
            Some(GlobalSymbol::Qreg { size, start }) => (*size, *start),
            Some(GlobalSymbol::Creg { .. }) => {
                return Err(message_from_token(
                    &name_token,
                    &format!("'{}' is a classical register, not a quantum register", name),
                    &self.tokens.filename,
                ))
            }
            Some(GlobalSymbol::Gate { .. }) => {
                return Err(message_from_token(
                    &name_token,
                    &format!("'{}' is a gate, not a quantum register", name),
                    &self.tokens.filename,
                ))
            }
            None => {
                return Err(message_from_token(
                    &name_token,
                    &format!("'{}' is not defined in this scope", name),
                    &self.tokens.filename,
                ))
            }
        };
        self.complete_operand(&name, register_size, register_start)
            .map(Some)
    }

    /// Take a complete quantum argument from the stream, if it matches.  This is for use within
    /// gates, and so the only valid type of quantum argument is a single qubit.
    fn accept_qarg_gate(&mut self) -> Result<Option<Operand>, String> {
        let (name, name_token) = match self.accept(TokenType::Id) {
            None => return Ok(None),
            Some(token) => (token.id(&self.tokens.context), token),
        };
        match self.gate_symbols.get(&name) {
            Some(GateSymbol::Qubit { index }) => Ok(Some(Operand::Single(*index))),
            Some(GateSymbol::Parameter { .. }) => Err(message_from_token(
                &name_token,
                &format!("'{}' is a parameter, not a qubit", name),
                &self.tokens.filename,
            )),
            None => {
                if self.symbols.contains_key(&name) {
                    let bad_type = match self.symbols[&name] {
                        GlobalSymbol::Qreg { .. } => "quantum register",
                        GlobalSymbol::Creg { .. } => "classical register",
                        GlobalSymbol::Gate { .. } => "gate",
                    };
                    Err(message_from_token(
                        &name_token,
                        &format!("'{}' is a {}, not a qubit", name, bad_type),
                        &self.tokens.filename,
                    ))
                } else {
                    Err(message_from_token(
                        &name_token,
                        &format!("'{}' is not defined in this scope", name),
                        &self.tokens.filename,
                    ))
                }
            }
        }
    }

    /// Take a complete quantum argument from the token stream, returning an error message if one
    /// is not present.
    fn require_qarg(&mut self, instruction: &Token) -> Result<Operand, String> {
        match self.tokens.peek().map(|tok| tok.ttype) {
            Some(TokenType::Id) => self.accept_qarg().map(Option::unwrap),
            Some(_) => {
                let token = self.tokens.next();
                Err(message_incorrect_requirement(
                    &self.tokens.filename,
                    "a quantum argument",
                    &token.unwrap(),
                ))
            }
            None => Err(message_bad_eof(
                &self.tokens.filename,
                "a quantum argument",
                instruction,
            )),
        }
    }

    /// Take a complete classical argument from the token stream, if the next token is an
    /// identifier.  This includes resolving any following subscript operation.  Returns an error
    /// variant if the next token _is_ an identifier, but the symbol represents something other
    /// than a classical register, or isn't defined.  This can also be an error if the subscript is
    /// opened, but cannot be completely resolved due to a typing error or other invalid parse.
    /// `Ok(None)` is returned if the next token in the stream does not match a possible classical
    /// argument.
    fn accept_carg(&mut self) -> Result<Option<Operand>, String> {
        let (name, name_token) = match self.accept(TokenType::Id) {
            None => return Ok(None),
            Some(token) => (token.id(&self.tokens.context), token),
        };
        let (register_size, register_start) = match self.symbols.get(&name) {
            Some(GlobalSymbol::Creg { size, start, .. }) => (*size, *start),
            Some(GlobalSymbol::Qreg { .. }) => {
                return Err(message_from_token(
                    &name_token,
                    &format!("'{}' is a quantum register, not a classical one", name),
                    &self.tokens.filename,
                ))
            }
            Some(GlobalSymbol::Gate { .. }) => {
                return Err(message_from_token(
                    &name_token,
                    &format!("'{}' is a gate, not a classical register", name),
                    &self.tokens.filename,
                ))
            }
            None => {
                return Err(message_from_token(
                    &name_token,
                    &format!("'{}' is not defined in this scope", name),
                    &self.tokens.filename,
                ))
            }
        };
        self.complete_operand(&name, register_size, register_start)
            .map(Some)
    }

    /// Take a complete classical argument from the token stream, returning an error message if one
    /// is not present.
    fn require_carg(&mut self, instruction: &Token) -> Result<Operand, String> {
        match self.tokens.peek().map(|tok| tok.ttype) {
            Some(TokenType::Id) => self.accept_carg().map(Option::unwrap),
            Some(_) => {
                let token = self.tokens.next();
                Err(message_incorrect_requirement(
                    &self.tokens.filename,
                    "a classical argument",
                    &token.unwrap(),
                ))
            }
            None => Err(message_bad_eof(
                &self.tokens.filename,
                "a classical argument",
                instruction,
            )),
        }
    }

    /// Evaluate a possible subscript on a register into a final [Operand], consuming the tokens
    /// (if present) from the stream.  Can return error variants if the subscript cannot be
    /// completed or if there is a parse error while reading the subscript.
    fn complete_operand(
        &mut self,
        name: &str,
        register_size: usize,
        register_start: usize,
    ) -> Result<Operand, String> {
        let lbracket_token = match self.accept(TokenType::LBracket) {
            Some(token) => token,
            None => return Ok(Operand::Range(register_size, register_start)),
        };
        let index_token = self.expect(TokenType::Integer, "an integer index", &lbracket_token)?;
        let index = index_token.int(&self.tokens.context);
        self.expect(TokenType::RBracket, "a closing bracket", &lbracket_token)?;
        if index < register_size {
            Ok(Operand::Single(register_start + index))
        } else {
            Err(message_from_token(
                &index_token,
                &format!(
                    "index {} is out-of-range for register '{}' of size {}",
                    index, name, register_size
                ),
                &self.tokens.filename,
            ))
        }
    }

    /// Parse an `OPENQASM <version>;` statement completely.  This function does not need to take
    /// the bytecode stream because the version information has no actionable effects for Qiskit
    /// to care about.  We simply error if the version supplied by the file is not the version of
    /// OpenQASM that we are able to support.  This assumes that the `OPENQASM` token is still in
    /// the stream.
    fn parse_version(&mut self) -> Result<usize, String> {
        let openqasm_token = self.expect_known(TokenType::OpenQASM);
        let version_token = self.expect(TokenType::Version, "version number", &openqasm_token)?;
        match version_token.version(&self.tokens.context) {
            Version {
                major: 2,
                minor: Some(0) | None,
            } => Ok(()),
            _ => Err(message_from_token(
                &version_token,
                &format!(
                    "can only handle OpenQASM 2.0, but given {}",
                    version_token.text(&self.tokens.context),
                ),
                &self.tokens.filename,
            )),
        }?;
        self.expect(TokenType::Semicolon, ";", &openqasm_token)?;
        Ok(0)
    }

    /// Parse a complete gate definition (including the body of the definition).  This assumes that
    /// the `gate` token is still in the scheme.  This function will likely result in many
    /// instructions being pushed onto the bytecode stream; one for the start and end of the gate
    /// definition, and then one instruction each for the gate applications in the body.
    fn parse_gate_definition(
        &mut self,
        bc: &mut Vec<Option<InternalBytecode>>,
    ) -> Result<usize, String> {
        let gate_token = self.expect_known(TokenType::Gate);
        let name_token = self.expect(TokenType::Id, "an identifier", &gate_token)?;
        let name = name_token.id(&self.tokens.context);
        // Parse the gate parameters (if any) into the symbol take.
        let mut n_params = 0usize;
        if let Some(lparen_token) = self.accept(TokenType::LParen) {
            while let Some(param_token) = self.accept(TokenType::Id) {
                let param_name = param_token.id(&self.tokens.context);
                match self.gate_symbols.insert(
                    param_name.to_owned(),
                    GateSymbol::Parameter { index: n_params },
                ) {
                    Some(GateSymbol::Parameter { .. }) => {
                        return Err(message_from_token(
                            &param_token,
                            &format!("'{}' is already defined as a parameter", param_name),
                            &self.tokens.filename,
                        ))
                    }
                    Some(GateSymbol::Qubit { .. }) => {
                        return Err(message_from_token(
                            &param_token,
                            &format!("'{}' is already defined as a qubit", param_name),
                            &self.tokens.filename,
                        ))
                    }
                    None => (),
                }
                n_params += 1;
                if self.accept(TokenType::Comma).is_none() {
                    break;
                }
            }
            self.expect(TokenType::RParen, "a closing parenthesis", &lparen_token)?;
        }
        // Parse the quantum parameters into the symbol table.
        let mut n_qubits = 0usize;
        while let Some(qubit_token) = self.accept(TokenType::Id) {
            let qubit_name = qubit_token.id(&self.tokens.context).to_owned();
            match self
                .gate_symbols
                .insert(qubit_name.to_owned(), GateSymbol::Qubit { index: n_qubits })
            {
                Some(GateSymbol::Parameter { .. }) => {
                    return Err(message_from_token(
                        &qubit_token,
                        &format!("'{}' is already defined as a parameter", qubit_name),
                        &self.tokens.filename,
                    ))
                }
                Some(GateSymbol::Qubit { .. }) => {
                    return Err(message_from_token(
                        &qubit_token,
                        &format!("'{}' is already defined as a qubit", qubit_name),
                        &self.tokens.filename,
                    ))
                }
                None => (),
            }
            n_qubits += 1;
            if self.accept(TokenType::Comma).is_none() {
                break;
            }
        }
        if n_qubits == 0 {
            return if self.tokens.peek().is_none() {
                Err(message_bad_eof(
                    &self.tokens.filename,
                    "a qubit identifier",
                    &gate_token,
                ))
            } else {
                Err(message_from_token(
                    &gate_token,
                    "gates must act on at least one qubit",
                    &self.tokens.filename,
                ))
            };
        }
        let lbrace_token = self.expect(TokenType::LBrace, "a gate body", &gate_token)?;
        bc.push(Some(InternalBytecode::DeclareGate {
            name: name.clone(),
            n_qubits,
        }));
        // The actual body of the gate.  Most of this is devolved to [Self::parse_gate_application]
        // to do the right thing.
        let mut statements = 0usize;
        loop {
            match self.tokens.peek().map(|tok| tok.ttype) {
                Some(TokenType::Id) => statements += self.parse_gate_application(bc, None, true)?,
                Some(TokenType::Barrier) => statements += self.parse_barrier(bc, Some(n_qubits))?,
                Some(TokenType::RBrace) => {
                    self.expect_known(TokenType::RBrace);
                    break;
                }
                Some(_) => {
                    let token = self.tokens.next().unwrap();
                    return Err(message_from_token(
                        &token,
                        &format!(
                            "only gate applications are valid within a 'gate' body, but saw {}",
                            token.text(&self.tokens.context)
                        ),
                        &self.tokens.filename,
                    ));
                }
                None => {
                    return Err(message_bad_eof(
                        &self.tokens.filename,
                        "a closing brace '}' of the gate body",
                        &lbrace_token,
                    ))
                }
            }
        }
        bc.push(Some(InternalBytecode::EndDeclareGate {}));
        self.gate_symbols.clear();
        self.define_gate(&gate_token, name, n_params, n_qubits)?;
        Ok(statements + 2)
    }

    /// Parse an `opaque` statement.  This assumes that the `opaque` token is still in the token
    /// stream we are reading from.
    fn parse_opaque_definition(
        &mut self,
        bc: &mut Vec<Option<InternalBytecode>>,
    ) -> Result<usize, String> {
        let opaque_token = self.expect_known(TokenType::Opaque);
        let name = self
            .expect(TokenType::Id, "an identifier", &opaque_token)?
            .text(&self.tokens.context)
            .to_owned();
        let mut n_params = 0usize;
        if let Some(lparen_token) = self.accept(TokenType::LParen) {
            while self.accept(TokenType::Id).is_some() {
                n_params += 1;
                if self.accept(TokenType::Comma).is_none() {
                    break;
                }
            }
            self.expect(TokenType::RParen, "closing parenthesis", &lparen_token)?;
        }
        let mut n_qubits = 0usize;
        while self.accept(TokenType::Id).is_some() {
            n_qubits += 1;
            if self.accept(TokenType::Comma).is_none() {
                break;
            }
        }
        self.expect(TokenType::Semicolon, ";", &opaque_token)?;
        if n_qubits == 0 {
            return Err(message_from_token(
                &opaque_token,
                "gates must act on at least one qubit",
                &self.tokens.filename,
            ));
        }
        bc.push(Some(InternalBytecode::DeclareOpaque {
            name: name.clone(),
            n_qubits,
        }));
        self.define_gate(&opaque_token, name, n_params, n_qubits)?;
        Ok(1)
    }

    /// Parse a gate application into the bytecode stream.  This resolves any broadcast over
    /// registers into a series of bytecode instructions, rather than leaving Qiskit to do it,
    /// which would involve much slower Python execution.  This assumes that the identifier token
    /// is still in the token stream.
    fn parse_gate_application(
        &mut self,
        bc: &mut Vec<Option<InternalBytecode>>,
        condition: Option<Condition>,
        in_gate: bool,
    ) -> Result<usize, String> {
        let name_token = self.expect_known(TokenType::Id);
        let name = name_token.id(&self.tokens.context);
        let (index, n_params, n_qubits) = match self.symbols.get(&name) {
            Some(GlobalSymbol::Gate {
                n_params,
                n_qubits,
                index,
            }) => Ok((*index, *n_params, *n_qubits)),
            Some(symbol) => Err(message_from_token(
                &name_token,
                &format!(
                    "'{}' is a {} register, not a gate",
                    name,
                    if matches!(symbol, GlobalSymbol::Creg { .. }) {
                        "classical"
                    } else {
                        "quantum"
                    }
                ),
                &self.tokens.filename,
            )),
            None => Err(message_from_token(
                &name_token,
                &format!("'{}' is not defined in this scope", name),
                &self.tokens.filename,
            )),
        }?;
        let parameters = self.expect_gate_parameters(&name_token, n_params, in_gate)?;
        let mut qargs = Vec::<Operand>::with_capacity(n_qubits);
        if in_gate {
            while let Some(qarg) = self.accept_qarg_gate()? {
                qargs.push(qarg);
                if self.accept(TokenType::Comma).is_none() {
                    break;
                }
            }
        } else {
            while let Some(qarg) = self.accept_qarg()? {
                qargs.push(qarg);
                if self.accept(TokenType::Comma).is_none() {
                    break;
                }
            }
        }
        if qargs.len() != n_qubits {
            return match self.tokens.peek().map(|tok| tok.ttype) {
                Some(TokenType::Semicolon) => Err(message_from_token(
                    &name_token,
                    &format!(
                        "'{}' takes {} quantum argument{}, but got {}",
                        name,
                        n_qubits,
                        if n_qubits == 1 { "" } else { "s" },
                        qargs.len()
                    ),
                    &self.tokens.filename,
                )),
                Some(_) => Err(message_incorrect_requirement(
                    &self.tokens.filename,
                    "the end of the argument list",
                    &name_token,
                )),
                None => Err(message_bad_eof(
                    &self.tokens.filename,
                    "the end of the argument list",
                    &name_token,
                )),
            };
        }
        self.expect(TokenType::Semicolon, "';'", &name_token)?;
        self.emit_gate_application(bc, &name_token, index, parameters, &qargs, condition)
    }

    /// Parse the parameters (if any) from a gate application.
    fn expect_gate_parameters(
        &mut self,
        name_token: &Token,
        n_params: usize,
        in_gate: bool,
    ) -> Result<GateParameters, String> {
        let lparen_token = match self.accept(TokenType::LParen) {
            Some(lparen_token) => lparen_token,
            None => {
                return Ok(if in_gate {
                    GateParameters::Expression(vec![])
                } else {
                    GateParameters::Constant(vec![])
                });
            }
        };
        let mut seen_params = 0usize;
        // This code duplication is to avoid duplication of allocation when parsing the far more
        // common case of expecting constant parameters for a gate application in the body of the
        // OQ2 file.
        let parameters = if in_gate {
            let mut parameters = Vec::<Expr>::with_capacity(n_params);
            while !self.next_is(TokenType::RParen) {
                let mut expr_parser = ExprParser {
                    tokens: &mut self.tokens,
                    gate_symbols: &self.gate_symbols,
                };
                parameters.push(expr_parser.parse_expression(&lparen_token)?);
                seen_params += 1;
                if self.accept(TokenType::Comma).is_none() {
                    break;
                }
            }
            self.expect(TokenType::RParen, "')'", &lparen_token)?;
            GateParameters::Expression(parameters)
        } else {
            let mut parameters = Vec::<f64>::with_capacity(n_params);
            while !self.next_is(TokenType::RParen) {
                let mut expr_parser = ExprParser {
                    tokens: &mut self.tokens,
                    gate_symbols: &self.gate_symbols,
                };
                match expr_parser.parse_expression(&lparen_token)? {
                    Expr::Constant(value) => parameters.push(value),
                    _ => {
                        return Err(message_from_token(
                            &lparen_token,
                            "non-constant expression in program body",
                            &self.tokens.filename,
                        ))
                    }
                }
                seen_params += 1;
                if self.accept(TokenType::Comma).is_none() {
                    break;
                }
            }
            self.expect(TokenType::RParen, "')'", &lparen_token)?;
            GateParameters::Constant(parameters)
        };
        if seen_params != n_params {
            return Err(message_from_token(
                name_token,
                &format!(
                    "'{}' takes {} parameter{}, but got {}",
                    &name_token.text(&self.tokens.context),
                    n_params,
                    if n_params == 1 { "" } else { "s" },
                    seen_params
                ),
                &self.tokens.filename,
            ));
        }
        Ok(parameters)
    }

    /// Emit the bytecode for the application of a gate.  This involves resolving any broadcasts
    /// in the operands of the gate (i.e. if one or more of them is a register).
    fn emit_gate_application(
        &self,
        bc: &mut Vec<Option<InternalBytecode>>,
        instruction: &Token,
        gate_id: usize,
        parameters: GateParameters,
        qargs: &[Operand],
        condition: Option<Condition>,
    ) -> Result<usize, String> {
        // Fast path for most common gate patterns that don't need broadcasting.
        if let Some(qubits) = match qargs {
            [Operand::Single(index)] => Some(vec![*index]),
            [Operand::Single(left), Operand::Single(right)] => {
                if *left == *right {
                    return Err(message_from_token(
                        instruction,
                        "duplicate qubits in gate application",
                        &self.tokens.filename,
                    ));
                }
                Some(vec![*left, *right])
            }
            [] => Some(vec![]),
            _ => None,
        } {
            return match parameters {
                GateParameters::Constant(parameters) => {
                    self.emit_single_global_gate(bc, gate_id, parameters, qubits, &condition)
                }
                GateParameters::Expression(parameters) => {
                    self.emit_single_gate_gate(bc, gate_id, parameters, qubits)
                }
            };
        };
        // If we're here we either have to broadcast or it's a 3+q gate - either way, we're not as
        // worried about performance.
        let mut qubits = HashSet::<usize>::with_capacity(qargs.len());
        let mut broadcast_length = 0usize;
        for qarg in qargs {
            match qarg {
                Operand::Single(index) => {
                    if !qubits.insert(*index) {
                        return Err(message_from_token(
                            instruction,
                            "duplicate qubits in gate application",
                            &self.tokens.filename,
                        ));
                    }
                }
                Operand::Range(size, start) => {
                    if broadcast_length != 0 && broadcast_length != *size {
                        return Err(message_from_token(
                            instruction,
                            "cannot resolve broadcast in gate application",
                            &self.tokens.filename,
                        ));
                    }
                    for index in *start..*start + *size {
                        if !qubits.insert(index) {
                            return Err(message_from_token(
                                instruction,
                                "duplicate qubits in gate application",
                                &self.tokens.filename,
                            ));
                        }
                    }
                    broadcast_length = *size;
                }
            }
        }
        if broadcast_length == 0 {
            let qubits = qargs
                .iter()
                .filter_map(|qarg| {
                    if let Operand::Single(index) = qarg {
                        Some(*index)
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();
            if qubits.len() < qargs.len() {
                // We're broadcasting against at least one empty register.
                return Ok(0);
            }
            return match parameters {
                GateParameters::Constant(parameters) => {
                    self.emit_single_global_gate(bc, gate_id, parameters, qubits, &condition)
                }
                GateParameters::Expression(parameters) => {
                    self.emit_single_gate_gate(bc, gate_id, parameters, qubits)
                }
            };
        }
        for i in 0..broadcast_length {
            let qubits = qargs
                .iter()
                .map(|qarg| match qarg {
                    Operand::Single(index) => *index,
                    Operand::Range(_, start) => *start + i,
                })
                .collect::<Vec<_>>();
            match parameters {
                GateParameters::Constant(ref parameters) => {
                    self.emit_single_global_gate(
                        bc,
                        gate_id,
                        parameters.clone(),
                        qubits,
                        &condition,
                    )?;
                }
                // Gates used in gate-body definitions can't ever broadcast, because their only
                // operands are single qubits.
                _ => unreachable!(),
            }
        }
        Ok(broadcast_length)
    }

    /// Emit the bytecode for a single gate application in the global scope.  This could
    /// potentially have a classical condition.
    fn emit_single_global_gate(
        &self,
        bc: &mut Vec<Option<InternalBytecode>>,
        gate_id: usize,
        arguments: Vec<f64>,
        qubits: Vec<usize>,
        condition: &Option<Condition>,
    ) -> Result<usize, String> {
        if let Some(condition) = condition {
            bc.push(Some(InternalBytecode::ConditionedGate {
                id: gate_id,
                arguments,
                qubits,
                creg: condition.creg,
                value: condition.value,
            }));
        } else {
            bc.push(Some(InternalBytecode::Gate {
                id: gate_id,
                arguments,
                qubits,
            }));
        }
        Ok(1)
    }

    /// Emit the bytecode for a single gate application in a gate-definition body.  These are not
    /// allowed to be conditioned, because otherwise the containing `gate` would not be unitary.
    fn emit_single_gate_gate(
        &self,
        bc: &mut Vec<Option<InternalBytecode>>,
        gate_id: usize,
        arguments: Vec<Expr>,
        qubits: Vec<usize>,
    ) -> Result<usize, String> {
        bc.push(Some(InternalBytecode::GateInBody {
            id: gate_id,
            arguments,
            qubits,
        }));
        Ok(1)
    }

    /// Parse a complete conditional statement, including the operation that follows the condition
    /// (though this work is delegated to the requisite other grammar rule).  This assumes that the
    /// `if` token is still on the token stream.
    fn parse_conditional(
        &mut self,
        bc: &mut Vec<Option<InternalBytecode>>,
    ) -> Result<usize, String> {
        let if_token = self.expect_known(TokenType::If);
        let lparen_token = self.expect(TokenType::LParen, "'('", &if_token)?;
        let name_token = self.expect(TokenType::Id, "classical register", &if_token)?;
        self.expect(TokenType::Equals, "'=='", &if_token)?;
        let value = self
            .expect(TokenType::Integer, "an integer", &if_token)?
            .int(&self.tokens.context);
        self.expect(TokenType::RParen, "')'", &lparen_token)?;
        let name = name_token.id(&self.tokens.context);
        let creg = match self.symbols.get(&name) {
            Some(GlobalSymbol::Creg { index, .. }) => Ok(*index),
            Some(GlobalSymbol::Qreg { .. }) => Err(message_from_token(
                &name_token,
                &format!("'{}' is a quantum register, not a classical register", name),
                &self.tokens.filename,
            )),
            Some(GlobalSymbol::Gate { .. }) => Err(message_from_token(
                &name_token,
                &format!("'{}' is a gate, not a classical register", name),
                &self.tokens.filename,
            )),
            None => Err(message_from_token(
                &name_token,
                &format!("'{}' is not defined in this scope", name),
                &self.tokens.filename,
            )),
        }?;
        let condition = Some(Condition { creg, value });
        match self.tokens.peek().map(|tok| tok.ttype) {
            Some(TokenType::Id) => self.parse_gate_application(bc, condition, false),
            Some(TokenType::Measure) => self.parse_measure(bc, condition),
            Some(TokenType::Reset) => self.parse_reset(bc, condition),
            Some(_) => {
                let token = self.tokens.next();
                Err(message_incorrect_requirement(
                    &self.tokens.filename,
                    "a gate application, measurement or reset",
                    &token.unwrap(),
                ))
            }
            None => Err(message_bad_eof(
                &self.tokens.filename,
                "a gate, measurement or reset to condition",
                &if_token,
            )),
        }
    }

    /// Parse a barrier statement.  This assumes that the `barrier` token is still in the token
    /// stream.
    fn parse_barrier(
        &mut self,
        bc: &mut Vec<Option<InternalBytecode>>,
        n_gate_qubits: Option<usize>,
    ) -> Result<usize, String> {
        let barrier_token = self.expect_known(TokenType::Barrier);
        let qubits = if !self.next_is(TokenType::Semicolon) {
            let mut qubits = Vec::new();
            let mut used = HashSet::<usize>::new();
            while let Some(qarg) = if n_gate_qubits.is_some() {
                self.accept_qarg_gate()?
            } else {
                self.accept_qarg()?
            } {
                match qarg {
                    Operand::Single(index) => {
                        if used.insert(index) {
                            qubits.push(index)
                        }
                    }
                    Operand::Range(size, start) => {
                        qubits.extend((start..start + size).filter(|value| used.insert(*value)))
                    }
                }
                if self.accept(TokenType::Comma).is_none() {
                    break;
                }
            }
            qubits
        } else if let Some(n_gate_qubits) = n_gate_qubits {
            (0..n_gate_qubits).collect::<Vec<usize>>()
        } else {
            (0..self.n_qubits).collect::<Vec<usize>>()
        };
        self.expect(TokenType::Semicolon, "';'", &barrier_token)?;
        if qubits.is_empty() {
            Ok(0)
        } else {
            // The qubits are empty iff the only operands are zero-sized registers.  If there's no
            // quantum arguments at all, then `qubits` will contain everything.
            bc.push(Some(InternalBytecode::Barrier { qubits }));
            Ok(1)
        }
    }

    /// Parse a measurement operation into bytecode.  This resolves any broadcast in the
    /// measurement statement into a series of bytecode instructions, so we do more of the work in
    /// Rust space than in Python space.
    fn parse_measure(
        &mut self,
        bc: &mut Vec<Option<InternalBytecode>>,
        condition: Option<Condition>,
    ) -> Result<usize, String> {
        let measure_token = self.expect_known(TokenType::Measure);
        let qarg = self.require_qarg(&measure_token)?;
        self.expect(TokenType::Arrow, "'->'", &measure_token)?;
        let carg = self.require_carg(&measure_token)?;
        self.expect(TokenType::Semicolon, "';'", &measure_token)?;
        if let Some(Condition { creg, value }) = condition {
            match (qarg, carg) {
                (Operand::Single(qubit), Operand::Single(clbit)) => {
                    bc.push(Some(InternalBytecode::ConditionedMeasure {
                        qubit,
                        clbit,
                        creg,
                        value,
                    }));
                    Ok(1)
                }
                (Operand::Range(q_size, q_start), Operand::Range(c_size, c_start))
                    if q_size == c_size =>
                {
                    bc.extend((0..q_size).map(|i| {
                        Some(InternalBytecode::ConditionedMeasure {
                            qubit: q_start + i,
                            clbit: c_start + i,
                            creg,
                            value,
                        })
                    }));
                    Ok(q_size)
                }
                _ => Err(message_from_token(
                    &measure_token,
                    "cannot resolve broadcast in measurement",
                    &self.tokens.filename,
                )),
            }
        } else {
            match (qarg, carg) {
                (Operand::Single(qubit), Operand::Single(clbit)) => {
                    bc.push(Some(InternalBytecode::Measure { qubit, clbit }));
                    Ok(1)
                }
                (Operand::Range(q_size, q_start), Operand::Range(c_size, c_start))
                    if q_size == c_size =>
                {
                    bc.extend((0..q_size).map(|i| {
                        Some(InternalBytecode::Measure {
                            qubit: q_start + i,
                            clbit: c_start + i,
                        })
                    }));
                    Ok(q_size)
                }
                _ => Err(message_from_token(
                    &measure_token,
                    "cannot resolve broadcast in measurement",
                    &self.tokens.filename,
                )),
            }
        }
    }

    /// Parse a single reset statement.  This resolves any broadcast in the statement, i.e. if the
    /// target is a register rather than a single qubit.  This assumes that the `reset` token is
    /// still in the token stream.
    fn parse_reset(
        &mut self,
        bc: &mut Vec<Option<InternalBytecode>>,
        condition: Option<Condition>,
    ) -> Result<usize, String> {
        let reset_token = self.expect_known(TokenType::Reset);
        let qarg = self.require_qarg(&reset_token)?;
        self.expect(TokenType::Semicolon, "';'", &reset_token)?;
        if let Some(Condition { creg, value }) = condition {
            match qarg {
                Operand::Single(qubit) => {
                    bc.push(Some(InternalBytecode::ConditionedReset {
                        qubit,
                        creg,
                        value,
                    }));
                    Ok(1)
                }
                Operand::Range(size, start) => {
                    bc.extend((start..start + size).map(|qubit| {
                        Some(InternalBytecode::ConditionedReset { qubit, creg, value })
                    }));
                    Ok(size)
                }
            }
        } else {
            match qarg {
                Operand::Single(qubit) => {
                    bc.push(Some(InternalBytecode::Reset { qubit }));
                    Ok(0)
                }
                Operand::Range(size, start) => {
                    bc.extend(
                        (start..start + size).map(|qubit| Some(InternalBytecode::Reset { qubit })),
                    );
                    Ok(size)
                }
            }
        }
    }

    /// Parse a declaration of a classical register, emitting the relevant bytecode and adding the
    /// definition to the relevant parts of the internal symbol tables in the parser state.  This
    /// assumes that the `creg` token is still in the token stream.
    fn parse_creg(&mut self, bc: &mut Vec<Option<InternalBytecode>>) -> Result<usize, String> {
        let creg_token = self.expect_known(TokenType::Creg);
        let name_token = self.expect(TokenType::Id, "a valid identifier", &creg_token)?;
        let name = name_token.id(&self.tokens.context);
        let lbracket_token = self.expect(TokenType::LBracket, "'['", &creg_token)?;
        let size = self
            .expect(TokenType::Integer, "an integer", &lbracket_token)?
            .int(&self.tokens.context);
        self.expect(TokenType::RBracket, "']'", &lbracket_token)?;
        self.expect(TokenType::Semicolon, "';'", &creg_token)?;
        bc.push(Some(InternalBytecode::DeclareCreg {
            name: name.clone(),
            size,
        }));
        if self
            .symbols
            .insert(
                name,
                GlobalSymbol::Creg {
                    size,
                    start: self.n_clbits,
                    index: self.n_cregs,
                },
            )
            .is_some()
        {
            return Err(message_from_token(
                &name_token,
                &format!(
                    "'{}' is already defined",
                    name_token.id(&self.tokens.context)
                ),
                &self.tokens.filename,
            ));
        }
        self.n_clbits += size;
        self.n_cregs += 1;
        Ok(1)
    }

    /// Parse a declaration of a quantum register, emitting the relevant bytecode and adding the
    /// definition to the relevant parts of the internal symbol tables in the parser state.  This
    /// assumes that the `qreg` token is still in the token stream.
    fn parse_qreg(&mut self, bc: &mut Vec<Option<InternalBytecode>>) -> Result<usize, String> {
        let qreg_token = self.expect_known(TokenType::Qreg);
        let name_token = self.expect(TokenType::Id, "a valid identifier", &qreg_token)?;
        let name = name_token.id(&self.tokens.context);
        let lbracket_token = self.expect(TokenType::LBracket, "'['", &qreg_token)?;
        let size = self
            .expect(TokenType::Integer, "an integer", &lbracket_token)?
            .int(&self.tokens.context);
        self.expect(TokenType::RBracket, "']'", &lbracket_token)?;
        self.expect(TokenType::Semicolon, "';'", &qreg_token)?;
        bc.push(Some(InternalBytecode::DeclareQreg {
            name: name.clone(),
            size,
        }));
        if self
            .symbols
            .insert(
                name,
                GlobalSymbol::Qreg {
                    size,
                    start: self.n_qubits,
                },
            )
            .is_some()
        {
            return Err(message_from_token(
                &name_token,
                &format!(
                    "'{}' is already defined",
                    name_token.id(&self.tokens.context)
                ),
                &self.tokens.filename,
            ));
        }
        self.n_qubits += size;
        Ok(1)
    }

    /// Parse an include statement.  This currently only actually handles includes of `qelib1.inc`,
    /// which aren't actually parsed; the parser has a built-in version of the file that it simply
    /// updates its state with (and the Python side of the parser does the same) rather than
    /// re-parsing the same file every time.  This assumes that the `include` token is still in the
    /// token stream.
    fn parse_include(&mut self, bc: &mut Vec<Option<InternalBytecode>>) -> Result<usize, String> {
        let include_token = self.expect_known(TokenType::Include);
        let filename_token =
            self.expect(TokenType::Filename, "a filename string", &include_token)?;
        self.expect(TokenType::Semicolon, "';'", &include_token)?;
        let filename = filename_token.filename(&self.tokens.context);
        if filename == "qelib1.inc" {
            self.include_qelib1(&include_token)?;
            bc.push(Some(InternalBytecode::SpecialInclude { name: filename }));
            Ok(1)
        } else {
            Err(message_from_token(
                &filename_token,
                &format!("can only currently handle 'qelib1.inc', not '{}'", filename),
                &self.tokens.filename,
            ))
        }
    }

    /// Update the parser state with the built-in version of the `qelib1.inc` include file.  This
    /// is precisely as the file is described in the arXiv paper.
    fn include_qelib1(&mut self, include: &Token) -> Result<(), String> {
        self.symbols.reserve(N_QELIB1_GATES);
        self.define_gate(include, "u3".to_owned(), 3, 1)?;
        self.define_gate(include, "u2".to_owned(), 2, 1)?;
        self.define_gate(include, "u1".to_owned(), 1, 1)?;
        self.define_gate(include, "cx".to_owned(), 0, 2)?;
        self.define_gate(include, "id".to_owned(), 0, 1)?;
        self.define_gate(include, "x".to_owned(), 0, 1)?;
        self.define_gate(include, "y".to_owned(), 0, 1)?;
        self.define_gate(include, "z".to_owned(), 0, 1)?;
        self.define_gate(include, "h".to_owned(), 0, 1)?;
        self.define_gate(include, "s".to_owned(), 0, 1)?;
        self.define_gate(include, "sdg".to_owned(), 0, 1)?;
        self.define_gate(include, "t".to_owned(), 0, 1)?;
        self.define_gate(include, "tdg".to_owned(), 0, 1)?;
        self.define_gate(include, "rx".to_owned(), 1, 1)?;
        self.define_gate(include, "ry".to_owned(), 1, 1)?;
        self.define_gate(include, "rz".to_owned(), 1, 1)?;
        self.define_gate(include, "cz".to_owned(), 0, 2)?;
        self.define_gate(include, "cy".to_owned(), 0, 2)?;
        self.define_gate(include, "ch".to_owned(), 0, 2)?;
        self.define_gate(include, "ccx".to_owned(), 0, 3)?;
        self.define_gate(include, "crz".to_owned(), 1, 2)?;
        self.define_gate(include, "cu1".to_owned(), 1, 2)?;
        self.define_gate(include, "cu3".to_owned(), 3, 2)?;
        Ok(())
    }

    /// Update the parser state with the definition of a particular gate.  This does not emit any
    /// bytecode because not all gate definitions need something passing to Python.  For example,
    /// the Python parser initialises its state including the built-in gates `U` and `CX`, and
    /// handles the `qelib1.inc` include specially as well.
    fn define_gate(
        &mut self,
        owner: &Token,
        name: String,
        n_params: usize,
        n_qubits: usize,
    ) -> Result<(), String> {
        if self.symbols.contains_key(&name) {
            Err(message_from_token(
                owner,
                &format!("name '{}' is already defined", name),
                &self.tokens.filename,
            ))
        } else {
            self.symbols.insert(
                name,
                GlobalSymbol::Gate {
                    n_params,
                    n_qubits,
                    index: self.n_gates,
                },
            );
            self.n_gates += 1;
            Ok(())
        }
    }

    /// Parse the next OpenQASM 2 statement in the program into a series of bytecode instructions.
    ///
    /// This is the principal public worker function of the parser.  One call to this function
    /// parses a single OpenQASM 2 statement, which may expand to several bytecode instructions if
    /// there is any broadcasting to resolve, or if the statement is a gate definition.  A call to
    /// this function that returns `Some` will always have pushed at least one instruction to the
    /// bytecode stream (the number is included).  A return of `None` signals the end of the
    /// iterator.
    pub fn parse_next(
        &mut self,
        bc: &mut Vec<Option<InternalBytecode>>,
    ) -> Result<Option<usize>, String> {
        let called = self.called;
        self.called = true;
        if let Some(ttype) = self.tokens.peek().map(|tok| tok.ttype) {
            match match ttype {
                TokenType::Id => self.parse_gate_application(bc, None, false),
                TokenType::Creg => self.parse_creg(bc),
                TokenType::Qreg => self.parse_qreg(bc),
                TokenType::Include => self.parse_include(bc),
                TokenType::Measure => self.parse_measure(bc, None),
                TokenType::Reset => self.parse_reset(bc, None),
                TokenType::Barrier => self.parse_barrier(bc, None),
                TokenType::If => self.parse_conditional(bc),
                TokenType::Opaque => self.parse_opaque_definition(bc),
                TokenType::Gate => self.parse_gate_definition(bc),
                TokenType::OpenQASM => {
                    if called {
                        let token = self.tokens.next().unwrap();
                        Err(message_from_token(
                            &token,
                            "only the first statement may be a version declaration",
                            &self.tokens.filename,
                        ))
                    } else {
                        self.parse_version()
                    }
                }
                _ => {
                    let token = self.tokens.next().unwrap();
                    Err(message_from_token(
                        &token,
                        &format!(
                            "needed a start-of-statement token, but instead got {}",
                            token.text(&self.tokens.context)
                        ),
                        &self.tokens.filename,
                    ))
                }
            }? {
                0 => self.parse_next(bc),
                emitted => Ok(Some(emitted)),
            }
        } else {
            Ok(None)
        }
    }
}
