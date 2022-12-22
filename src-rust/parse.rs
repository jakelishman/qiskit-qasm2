use hashbrown::{HashMap, HashSet};

use crate::bytecode::InternalByteCode;
use crate::error::{message_bad_eof, message_from_token, message_incorrect_requirement};
use crate::expr::{Expr, ExprArena, ExprParser};
use crate::lex::{Token, TokenStream, TokenType, Version};

const N_BUILTIN_GATES: usize = 2;
const N_QELIB1_GATES: usize = 23;

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

pub enum GateSymbol {
    Qubit { index: usize },
    Parameter { index: usize },
}

enum Operand {
    Single(usize),
    Range(usize, usize),
}

enum GateParameters {
    Constant(Vec<f64>),
    Expression(Vec<(Expr, ExprArena)>),
}

struct Condition {
    creg: usize,
    value: usize,
}

pub struct State<T: std::io::BufRead> {
    tokens: TokenStream<T>,
    symbols: HashMap<String, GlobalSymbol>,
    gate_symbols: HashMap<String, GateSymbol>,
    n_qubits: usize,
    n_clbits: usize,
    n_cregs: usize,
    n_gates: usize,
}

impl<T: std::io::BufRead> State<T> {
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
        };
        let dummy_token = Token::dummy();
        state.define_gate(&dummy_token, "U".into(), 3, 1).unwrap();
        state.define_gate(&dummy_token, "CX".into(), 0, 2).unwrap();
        state
    }

    fn expect_known(&mut self, expected: TokenType) -> Token {
        let out = self.tokens.next().unwrap();
        if out.ttype != expected {
            panic!()
        }
        out
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

    fn accept(&mut self, expected: TokenType) -> Option<Token> {
        let peeked = self.tokens.peek();
        if peeked.is_some() && peeked.unwrap().ttype == expected {
            self.tokens.next()
        } else {
            None
        }
    }

    fn next_is(&mut self, expected: TokenType) -> bool {
        let peeked = self.tokens.peek();
        peeked.is_some() && peeked.unwrap().ttype == expected
    }

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
                    &format!("'{}' is a classical register, not a quantum one", name),
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
                        &format!("'{}' is a {}, not a qubit", bad_type, name),
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

    fn parse_version(&mut self) -> Result<(), String> {
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
        Ok(())
    }

    fn parse_gate_definition(&mut self, bc: &mut Vec<InternalByteCode>) -> Result<(), String> {
        let gate_token = self.expect_known(TokenType::Gate);
        let name_token = self.expect(TokenType::Id, "an identifier", &gate_token)?;
        let name = name_token.id(&self.tokens.context);
        let mut params = Vec::new();
        if let Some(lparen_token) = self.accept(TokenType::LParen) {
            while let Some(param_token) = self.accept(TokenType::Id) {
                let param_name = param_token.id(&self.tokens.context);
                match self.gate_symbols.insert(
                    param_name.to_owned(),
                    GateSymbol::Parameter {
                        index: params.len(),
                    },
                ) {
                    Some(GateSymbol::Parameter { .. }) => {
                        return Err(message_from_token(
                            &param_token,
                            &format!("'{}' is already a defined parameter", param_name),
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
                params.push(param_name.to_owned());
                if self.accept(TokenType::Comma).is_none() {
                    break;
                }
            }
            self.expect(TokenType::RParen, "a closing parenthesis", &lparen_token)?;
        }
        let n_params = params.len();
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
                        &format!("'{}' is already a defined parameter", qubit_name),
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
        let lbrace_token = self.expect(TokenType::LBrace, "a gate body", &gate_token)?;
        bc.push(InternalByteCode::DeclareGate {
            name: name.clone(),
            params,
            n_qubits,
        });
        loop {
            match self.tokens.peek().map(|tok| tok.ttype) {
                Some(TokenType::Id) => self.parse_gate_application(bc, None, true)?,
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
        bc.push(InternalByteCode::EndDeclareGate {});
        self.define_gate(&gate_token, name, n_params, n_qubits)?;
        Ok(())
    }

    fn parse_opaque_definition(&mut self, bc: &mut Vec<InternalByteCode>) -> Result<(), String> {
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
        bc.push(InternalByteCode::DeclareOpaque {
            name: name.clone(),
            n_params,
            n_qubits,
        });
        self.define_gate(&opaque_token, name, n_params, n_qubits)?;
        Ok(())
    }

    fn parse_gate_application(
        &mut self,
        bc: &mut Vec<InternalByteCode>,
        condition: Option<Condition>,
        in_gate: bool,
    ) -> Result<(), String> {
        let name_token = self.expect_known(TokenType::Id);
        let name = name_token.id(&self.tokens.context);
        let (index, n_params, n_qubits) = match self.symbols.get(&name) {
            Some(GlobalSymbol::Gate {
                n_params,
                n_qubits,
                index,
            }) => Ok((*index, *n_params, *n_qubits)),
            Some(_) => Err(message_from_token(
                &name_token,
                &format!("'{}' was declared as a register, not a gate", name),
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
            return Err(message_from_token(
                &name_token,
                &format!(
                    "'{}' takes {} quantum argument{}, but got {}",
                    name,
                    n_qubits,
                    if n_qubits == 1 { "" } else { "s" },
                    qargs.len()
                ),
                &self.tokens.filename,
            ));
        }
        self.expect(TokenType::Semicolon, "';'", &name_token)?;
        self.emit_gate_application(bc, &name_token, index, parameters, &qargs, condition)
    }

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
            let mut parameters = Vec::<(Expr, ExprArena)>::with_capacity(n_params);
            while !self.next_is(TokenType::RParen) {
                let mut expr_parser = ExprParser::new(&mut self.tokens, &self.gate_symbols);
                parameters.push((
                    expr_parser.parse_expression(&lparen_token)?,
                    expr_parser.arena,
                ));
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
                let mut expr_parser = ExprParser::new(&mut self.tokens, &self.gate_symbols);
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

    fn emit_gate_application(
        &self,
        bc: &mut Vec<InternalByteCode>,
        instruction: &Token,
        gate_id: usize,
        parameters: GateParameters,
        qargs: &[Operand],
        condition: Option<Condition>,
    ) -> Result<(), String> {
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
                .map(|qarg| {
                    if let Operand::Single(index) = qarg {
                        *index
                    } else {
                        unreachable!()
                    }
                })
                .collect::<Vec<_>>();
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
                GateParameters::Constant(ref parameters) => self.emit_single_global_gate(
                    bc,
                    gate_id,
                    parameters.clone(),
                    qubits,
                    &condition,
                )?,
                // Gates used in gate-body definitions can't ever broadcast, because their only
                // operands are single qubits.
                _ => unreachable!(),
            }
        }
        Ok(())
    }

    fn emit_single_global_gate(
        &self,
        bc: &mut Vec<InternalByteCode>,
        gate_id: usize,
        parameters: Vec<f64>,
        qubits: Vec<usize>,
        condition: &Option<Condition>,
    ) -> Result<(), String> {
        if let Some(condition) = condition {
            bc.push(InternalByteCode::ConditionedGate {
                id: gate_id,
                parameters,
                qubits,
                creg: condition.creg,
                value: condition.value,
            });
        } else {
            bc.push(InternalByteCode::Gate {
                id: gate_id,
                parameters,
                qubits,
            });
        }
        Ok(())
    }

    fn emit_single_gate_gate(
        &self,
        bc: &mut Vec<InternalByteCode>,
        gate_id: usize,
        parameters: Vec<(Expr, ExprArena)>,
        qubits: Vec<usize>,
    ) -> Result<(), String> {
        bc.push(InternalByteCode::GateInBody {
            id: gate_id,
            parameters,
            qubits,
        });
        Ok(())
    }

    fn parse_conditional(&mut self, bc: &mut Vec<InternalByteCode>) -> Result<(), String> {
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
            Some(_) => Err(message_from_token(
                &name_token,
                &format!("'{}' is not a classical register", name),
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

    fn parse_barrier(&mut self, bc: &mut Vec<InternalByteCode>) -> Result<(), String> {
        let barrier_token = self.expect_known(TokenType::Barrier);
        let qubits = if !self.next_is(TokenType::Semicolon) {
            let mut qubits = Vec::new();
            while let Some(qarg) = self.accept_qarg()? {
                match qarg {
                    Operand::Single(index) => qubits.push(index),
                    Operand::Range(size, start) => qubits.extend(start..start + size),
                }
                if self.accept(TokenType::Comma).is_none() {
                    break;
                }
            }
            qubits
        } else {
            (0..self.n_qubits).collect::<Vec<usize>>()
        };
        self.expect(TokenType::Semicolon, "';'", &barrier_token)?;
        bc.push(InternalByteCode::Barrier { qubits });
        Ok(())
    }

    fn parse_measure(
        &mut self,
        bc: &mut Vec<InternalByteCode>,
        condition: Option<Condition>,
    ) -> Result<(), String> {
        let measure_token = self.expect_known(TokenType::Measure);
        let qarg = self.require_qarg(&measure_token)?;
        self.expect(TokenType::Arrow, "'->'", &measure_token)?;
        let carg = self.require_carg(&measure_token)?;
        self.expect(TokenType::Semicolon, "';'", &measure_token)?;
        if let Some(Condition { creg, value }) = condition {
            match (qarg, carg) {
                (Operand::Single(qubit), Operand::Single(clbit)) => {
                    bc.push(InternalByteCode::ConditionedMeasure {
                        qubit,
                        clbit,
                        creg,
                        value,
                    });
                    Ok(())
                }
                (Operand::Range(q_size, q_start), Operand::Range(c_size, c_start))
                    if q_size == c_size =>
                {
                    bc.extend((0..q_size).map(|i| InternalByteCode::ConditionedMeasure {
                        qubit: q_start + i,
                        clbit: c_start + i,
                        creg,
                        value,
                    }));
                    Ok(())
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
                    bc.push(InternalByteCode::Measure { qubit, clbit });
                    Ok(())
                }
                (Operand::Range(q_size, q_start), Operand::Range(c_size, c_start))
                    if q_size == c_size =>
                {
                    bc.extend((0..q_size).map(|i| InternalByteCode::Measure {
                        qubit: q_start + i,
                        clbit: c_start + i,
                    }));
                    Ok(())
                }
                _ => Err(message_from_token(
                    &measure_token,
                    "cannot resolve broadcast in measurement",
                    &self.tokens.filename,
                )),
            }
        }
    }

    fn parse_reset(
        &mut self,
        bc: &mut Vec<InternalByteCode>,
        condition: Option<Condition>,
    ) -> Result<(), String> {
        let reset_token = self.expect_known(TokenType::Reset);
        let qarg = self.require_qarg(&reset_token)?;
        self.expect(TokenType::Semicolon, "';'", &reset_token)?;
        if let Some(Condition { creg, value }) = condition {
            match qarg {
                Operand::Single(qubit) => {
                    bc.push(InternalByteCode::ConditionedReset { qubit, creg, value })
                }
                Operand::Range(size, start) => bc.extend(
                    (start..start + size).map(|qubit| InternalByteCode::ConditionedReset {
                        qubit,
                        creg,
                        value,
                    }),
                ),
            };
        } else {
            match qarg {
                Operand::Single(qubit) => bc.push(InternalByteCode::Reset { qubit }),
                Operand::Range(size, start) => {
                    bc.extend((start..start + size).map(|qubit| InternalByteCode::Reset { qubit }))
                }
            };
        }
        Ok(())
    }

    fn parse_creg(&mut self, bc: &mut Vec<InternalByteCode>) -> Result<(), String> {
        let creg_token = self.expect_known(TokenType::Creg);
        let name_token = self.expect(TokenType::Id, "a classical register", &creg_token)?;
        let name = name_token.id(&self.tokens.context);
        let lbracket_token = self.expect(TokenType::LBracket, "'['", &creg_token)?;
        let size = self
            .expect(TokenType::Integer, "an integer", &lbracket_token)?
            .int(&self.tokens.context);
        self.expect(TokenType::RBracket, "']'", &lbracket_token)?;
        self.expect(TokenType::Semicolon, "';'", &creg_token)?;
        bc.push(InternalByteCode::DeclareCreg {
            name: name.clone(),
            size,
        });
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
        Ok(())
    }

    fn parse_qreg(&mut self, bc: &mut Vec<InternalByteCode>) -> Result<(), String> {
        let qreg_token = self.expect_known(TokenType::Qreg);
        let name_token = self.expect(TokenType::Id, "a quantum register", &qreg_token)?;
        let name = name_token.id(&self.tokens.context);
        let lbracket_token = self.expect(TokenType::LBracket, "'['", &qreg_token)?;
        let size = self
            .expect(TokenType::Integer, "an integer", &lbracket_token)?
            .int(&self.tokens.context);
        self.expect(TokenType::RBracket, "']'", &lbracket_token)?;
        self.expect(TokenType::Semicolon, "';'", &qreg_token)?;
        bc.push(InternalByteCode::DeclareQreg {
            name: name.clone(),
            size,
        });
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
        Ok(())
    }

    fn parse_include(&mut self, bc: &mut Vec<InternalByteCode>) -> Result<(), String> {
        let include_token = self.expect_known(TokenType::Include);
        let filename_token =
            self.expect(TokenType::Filename, "a filename string", &include_token)?;
        self.expect(TokenType::Semicolon, "';'", &include_token)?;
        let filename = filename_token.filename(&self.tokens.context);
        if filename == "qelib1.inc" {
            self.include_qelib1(&include_token)?;
            bc.push(InternalByteCode::SpecialInclude { name: filename });
            Ok(())
        } else {
            Err(message_from_token(
                &filename_token,
                &format!("can only currently handle 'qelib1.inc', not '{}'", filename),
                &self.tokens.filename,
            ))
        }
    }

    fn include_qelib1(&mut self, include: &Token) -> Result<(), String> {
        self.symbols.reserve(N_QELIB1_GATES);
        self.define_gate(include, "u3".into(), 3, 1)?;
        self.define_gate(include, "u2".into(), 2, 1)?;
        self.define_gate(include, "u1".into(), 1, 1)?;
        self.define_gate(include, "cx".into(), 0, 2)?;
        self.define_gate(include, "id".into(), 0, 1)?;
        self.define_gate(include, "x".into(), 0, 1)?;
        self.define_gate(include, "y".into(), 0, 1)?;
        self.define_gate(include, "z".into(), 0, 1)?;
        self.define_gate(include, "h".into(), 0, 1)?;
        self.define_gate(include, "s".into(), 0, 1)?;
        self.define_gate(include, "sdg".into(), 0, 1)?;
        self.define_gate(include, "t".into(), 0, 1)?;
        self.define_gate(include, "tdg".into(), 0, 1)?;
        self.define_gate(include, "rx".into(), 1, 1)?;
        self.define_gate(include, "ry".into(), 1, 1)?;
        self.define_gate(include, "rz".into(), 1, 1)?;
        self.define_gate(include, "cz".into(), 0, 2)?;
        self.define_gate(include, "cy".into(), 0, 2)?;
        self.define_gate(include, "ch".into(), 0, 2)?;
        self.define_gate(include, "ccx".into(), 0, 3)?;
        self.define_gate(include, "crz".into(), 1, 2)?;
        self.define_gate(include, "cu1".into(), 1, 2)?;
        self.define_gate(include, "cu3".into(), 3, 2)?;
        Ok(())
    }

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

    pub fn parse_next(
        &mut self,
        bc: &mut Vec<InternalByteCode>,
        allow_version: bool,
    ) -> Result<(), String> {
        if allow_version {
            if let Some(TokenType::OpenQASM) = self.tokens.peek().map(|tok| tok.ttype) {
                self.parse_version()?;
            }
        }
        if let Some(ttype) = self.tokens.peek().map(|tok| tok.ttype) {
            match ttype {
                TokenType::Id => self.parse_gate_application(bc, None, false),
                TokenType::Creg => self.parse_creg(bc),
                TokenType::Qreg => self.parse_qreg(bc),
                TokenType::Include => self.parse_include(bc),
                TokenType::Measure => self.parse_measure(bc, None),
                TokenType::Reset => self.parse_reset(bc, None),
                TokenType::Barrier => self.parse_barrier(bc),
                TokenType::If => self.parse_conditional(bc),
                TokenType::Opaque => self.parse_opaque_definition(bc),
                TokenType::Gate => self.parse_gate_definition(bc),
                _ => {
                    let token = self.tokens.next().unwrap();
                    Err(message_from_token(
                        &token,
                        &format!(
                            "needed a start-of-statement token, but got {}",
                            token.text(&self.tokens.context)
                        ),
                        &self.tokens.filename,
                    ))
                }
            }?;
        }
        Ok(())
    }
}
