use core::f64;
use core::iter::Peekable;

use hashbrown::HashMap;
use pyo3::prelude::*;

use crate::lex::{Token, TokenStream, TokenType, Version};

const N_BUILTIN_GATES: usize = 2;
const N_QELIB1_GATES: usize = 23;

#[pyclass(module = "qiskit_qasm2.core", frozen)]
#[derive(Copy, Clone, Debug)]
pub enum OpCode {
    Gate,
    ConditionedGate,
    Measure,
    ConditionedMeasure,
    Reset,
    ConditionedReset,
    Barrier,
    DeclareQreg,
    DeclareCreg,
    DeclareGate,
    GateInBody,
    PushInteger,
    PushFloat,
    PushPi,
    PushParameter,
    PushOperator,
    EvaluateExpression,
    EndDeclareGate,
    DeclareOpaque,
    SpecialInclude,
    Error,
}

#[pyclass(module = "qiskit_qasm2.core", frozen)]
#[derive(Copy, Clone, Debug)]
pub enum BinaryOp {
    Plus,
    Minus,
    Multiply,
    Divide,
    Power,
}

#[pyclass(module = "qiskit_qasm2.core", frozen)]
#[derive(Copy, Clone, Debug)]
pub enum UnaryOp {
    Negate,
    Cos,
    Exp,
    Ln,
    Sin,
    Sqrt,
    Tan,
}

#[pyclass(module = "qiskit_qasm2.core", frozen)]
pub struct ByteCode {
    #[pyo3(get)]
    opcode: OpCode,
    #[pyo3(get)]
    operands: PyObject,
}

pub enum InternalByteCode {
    Gate {
        id: usize,
        parameters: Vec<f64>,
        qubits: Vec<usize>,
    },
    ConditionedGate {
        id: usize,
        parameters: Vec<f64>,
        qubits: Vec<usize>,
        creg: usize,
        value: usize,
    },
    Measure {
        qubit: usize,
        clbit: usize,
    },
    ConditionedMeasure {
        qubit: usize,
        clbit: usize,
        creg: usize,
        value: usize,
    },
    Reset {
        qubit: usize,
    },
    ConditionedReset {
        qubit: usize,
        creg: usize,
        value: usize,
    },
    Barrier {
        qubits: Vec<usize>,
    },
    DeclareQreg {
        name: String,
        size: usize,
    },
    DeclareCreg {
        name: String,
        size: usize,
    },
    DeclareGate {
        name: String,
        n_params: usize,
        n_qubits: usize,
    },
    GateInBody {
        id: usize,
        n_params: usize,
        qubits: Vec<usize>,
    },
    PushInteger {
        value: usize,
    },
    PushFloat {
        value: f64,
    },
    PushPi {},
    PushParameter {
        index: usize,
    },
    PushBinaryOperator {
        op: BinaryOp,
    },
    PushUnaryOperator {
        op: UnaryOp,
    },
    EvaluateExpression {},
    EndDeclareGate {},
    DeclareOpaque {
        name: String,
        n_params: usize,
        n_qubits: usize,
    },
    SpecialInclude {
        name: String,
    },
}

impl InternalByteCode {
    pub fn to_python(&self, py: Python) -> ByteCode {
        match self {
            InternalByteCode::Gate {
                id,
                parameters,
                qubits,
            } => ByteCode {
                opcode: OpCode::Gate,
                operands: (id, parameters, qubits).to_object(py),
            },
            InternalByteCode::ConditionedGate {
                id,
                parameters,
                qubits,
                creg,
                value,
            } => ByteCode {
                opcode: OpCode::ConditionedGate,
                operands: (id, parameters, qubits, creg, value).to_object(py),
            },
            InternalByteCode::Measure { qubit, clbit } => ByteCode {
                opcode: OpCode::Measure,
                operands: (qubit, clbit).to_object(py),
            },
            InternalByteCode::ConditionedMeasure {
                qubit,
                clbit,
                creg,
                value,
            } => ByteCode {
                opcode: OpCode::ConditionedMeasure,
                operands: (qubit, clbit, creg, value).to_object(py),
            },
            InternalByteCode::Reset { qubit } => ByteCode {
                opcode: OpCode::Reset,
                operands: (qubit,).to_object(py),
            },
            InternalByteCode::ConditionedReset { qubit, creg, value } => ByteCode {
                opcode: OpCode::ConditionedReset,
                operands: (qubit, creg, value).to_object(py),
            },
            InternalByteCode::Barrier { qubits } => ByteCode {
                opcode: OpCode::Reset,
                operands: (qubits,).to_object(py),
            },
            InternalByteCode::DeclareQreg { name, size } => ByteCode {
                opcode: OpCode::DeclareQreg,
                operands: (name, size).to_object(py),
            },
            InternalByteCode::DeclareCreg { name, size } => ByteCode {
                opcode: OpCode::DeclareCreg,
                operands: (name, size).to_object(py),
            },
            InternalByteCode::DeclareGate {
                name,
                n_params,
                n_qubits,
            } => ByteCode {
                opcode: OpCode::DeclareGate,
                operands: (name, n_params, n_qubits).to_object(py),
            },
            InternalByteCode::GateInBody {
                id,
                n_params,
                qubits,
            } => ByteCode {
                opcode: OpCode::GateInBody,
                operands: (id, n_params, qubits).to_object(py),
            },
            InternalByteCode::PushInteger { value } => ByteCode {
                opcode: OpCode::PushInteger,
                operands: (value,).to_object(py),
            },
            InternalByteCode::PushFloat { value } => ByteCode {
                opcode: OpCode::PushFloat,
                operands: (value,).to_object(py),
            },
            InternalByteCode::PushPi {} => ByteCode {
                opcode: OpCode::PushPi,
                operands: ().to_object(py),
            },
            InternalByteCode::PushParameter { index } => ByteCode {
                opcode: OpCode::PushParameter,
                operands: (index,).to_object(py),
            },
            InternalByteCode::PushBinaryOperator { op } => ByteCode {
                opcode: OpCode::PushOperator,
                operands: (op.into_py(py),).to_object(py),
            },
            InternalByteCode::PushUnaryOperator { op } => ByteCode {
                opcode: OpCode::PushOperator,
                operands: (op.into_py(py),).to_object(py),
            },
            InternalByteCode::EvaluateExpression {} => ByteCode {
                opcode: OpCode::EvaluateExpression,
                operands: ().to_object(py),
            },
            InternalByteCode::EndDeclareGate {} => ByteCode {
                opcode: OpCode::EndDeclareGate,
                operands: ().to_object(py),
            },
            InternalByteCode::DeclareOpaque {
                name,
                n_params,
                n_qubits,
            } => ByteCode {
                opcode: OpCode::DeclareOpaque,
                operands: (name, n_params, n_qubits).to_object(py),
            },
            InternalByteCode::SpecialInclude { name } => ByteCode {
                opcode: OpCode::SpecialInclude,
                operands: (name,).to_object(py),
            },
        }
    }
}

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

enum GateSymbol {
    Qubit { index: usize },
    Parameter { index: usize },
}

enum Operand {
    Single(usize),
    Range(usize, usize),
}

struct Condition {
    creg: usize,
    value: usize,
}

macro_rules! require_token {
    ($token_stream:expr, $expected:pat) => {{
        let token = $token_stream.next();
        match &token.as_ref().map(|tok| &tok.ttype) {
            Some($expected) => Ok(token.unwrap()),
            _ => Err(token),
        }
    }};
}

macro_rules! try_token {
    ($token_stream:expr, $token:pat) => {{
        let peeked = $token_stream.peek();
        matches!(peeked.map(|tok| &tok.ttype), Some($token))
    }};
}

macro_rules! accept_token {
    ($token_stream:expr, $token:pat) => {{
        if try_token!($token_stream, $token) {
            require_token!($token_stream, $token).unwrap();
            true
        } else {
            false
        }
    }};
}

struct State<'a> {
    tokens: Peekable<TokenStream<'a>>,
    filename: String,
    bc: Vec<InternalByteCode>,
    symbols: HashMap<String, GlobalSymbol>,
    gate_symbols: HashMap<String, GateSymbol>,
    n_qubits: usize,
    n_clbits: usize,
    n_cregs: usize,
    n_gates: usize,
}

impl<'a> State<'a> {
    pub fn new(tokens: TokenStream<'a>) -> State {
        let filename = tokens.filename.clone();
        let mut state = State {
            tokens: tokens.peekable(),
            filename,
            bc: Vec::new(),
            // For Qiskit-created circuits, all files will have the builtin gates and `qelib1.inc`,
            // so we allocate with that in mind.
            symbols: HashMap::with_capacity(N_BUILTIN_GATES + N_QELIB1_GATES),
            gate_symbols: HashMap::new(),
            n_qubits: 0,
            n_clbits: 0,
            n_cregs: 0,
            n_gates: 0,
        };
        let dummy_token = Token {
            ttype: TokenType::Error,
            line: 0,
            start_col: 0,
            end_col: 0,
        };
        state.define_gate(&dummy_token, "U".into(), 3, 1).unwrap();
        state.define_gate(&dummy_token, "CX".into(), 0, 2).unwrap();
        state
    }

    pub fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    #[inline(always)]
    fn expect_identifier(&mut self, cause: &Token) -> Result<(String, Token), String> {
        let token = self.tokens.next();
        match token.as_ref().map(|tok| &tok.ttype) {
            Some(TokenType::Id(name)) => Ok((name.clone(), token.unwrap())),
            Some(_) => Err(message_incorrect_requirement(
                &self.filename,
                "an identifier",
                token.as_ref().unwrap(),
            )),
            None => Err(message_bad_eof(&self.filename, "an identifier", cause)),
        }
    }

    #[inline(always)]
    fn expect_float(&mut self) -> Result<(f64, Token), Option<Token>> {
        let token = self.tokens.next();
        match token.as_ref().map(|tok| &tok.ttype) {
            Some(TokenType::Real(val)) => Ok((*val, token.unwrap())),
            Some(TokenType::Integer(val)) => Ok((*val as f64, token.unwrap())),
            Some(TokenType::Pi) => Ok((f64::consts::PI, token.unwrap())),
            _ => Err(token),
        }
    }

    #[inline(always)]
    fn expect_integer(&mut self) -> Result<(usize, Token), Option<Token>> {
        let token = self.tokens.next();
        match token.as_ref().map(|tok| &tok.ttype) {
            Some(TokenType::Integer(val)) => Ok((*val, token.unwrap())),
            _ => Err(token),
        }
    }

    fn accept_qarg_gate(&mut self, gate_token: &Token) -> Result<Option<usize>, String> {
        if !try_token!(self.tokens, TokenType::Id(_)) {
            return Ok(None);
        }
        let (name, name_token) = self.expect_identifier(gate_token)?;
        match self.gate_symbols.get(&name) {
            Some(GateSymbol::Qubit { index }) => Ok(Some(*index)),
            _ => Err(message_from_token(
                &self.filename,
                &name_token,
                &format!("name '{}' is not a qubit defined by this gate", name),
            )),
        }
    }

    fn accept_qarg(&mut self, instruction: &Token) -> Result<Option<Operand>, String> {
        if !try_token!(self.tokens, TokenType::Id(_)) {
            return Ok(None);
        }
        let (name, name_token) = self.expect_identifier(instruction)?;
        let (register_size, register_start) = match self.symbols.get(&name) {
            Some(GlobalSymbol::Qreg { size, start }) => (*size, *start),
            Some(GlobalSymbol::Creg { .. }) => {
                return Err(message_from_token(
                    &self.filename,
                    &name_token,
                    &format!("'{}' is a classical register, not a quantum one", name),
                ))
            }
            Some(GlobalSymbol::Gate { .. }) => {
                return Err(message_from_token(
                    &self.filename,
                    &name_token,
                    &format!("'{}' is a gate, not a quantum register", name),
                ))
            }
            None => {
                return Err(message_from_token(
                    &self.filename,
                    &name_token,
                    &format!("'{}' is not defined in this scope", name),
                ))
            }
        };
        self.complete_operand(&name, register_size, register_start)
            .map(Some)
    }

    fn require_qarg(&mut self, instruction: &Token) -> Result<Operand, String> {
        let peeked = self.tokens.peek();
        match peeked.as_ref().map(|tok| &tok.ttype) {
            Some(TokenType::Id(_)) => self.accept_qarg(instruction).map(Option::unwrap),
            Some(_) => Err(message_incorrect_requirement(
                &self.filename,
                "a quantum argument",
                peeked.unwrap(),
            )),
            None => Err(message_bad_eof(
                &self.filename,
                "a quantum argument",
                instruction,
            )),
        }
    }

    fn accept_carg(&mut self, instruction: &Token) -> Result<Option<Operand>, String> {
        if !try_token!(self.tokens, TokenType::Id(_)) {
            return Ok(None);
        }
        let (name, name_token) = self.expect_identifier(instruction)?;
        let (register_size, register_start) = match self.symbols.get(&name) {
            Some(GlobalSymbol::Creg { size, start, .. }) => (*size, *start),
            Some(GlobalSymbol::Qreg { .. }) => {
                return Err(message_from_token(
                    &self.filename,
                    &name_token,
                    &format!("'{}' is a quantum register, not a classical one", name),
                ))
            }
            Some(GlobalSymbol::Gate { .. }) => {
                return Err(message_from_token(
                    &self.filename,
                    &name_token,
                    &format!("'{}' is a gate, not a classical register", name),
                ))
            }
            None => {
                return Err(message_from_token(
                    &self.filename,
                    &name_token,
                    &format!("'{}' is not defined in this scope", name),
                ))
            }
        };
        self.complete_operand(&name, register_size, register_start)
            .map(Some)
    }

    fn require_carg(&mut self, instruction: &Token) -> Result<Operand, String> {
        let peeked = self.tokens.peek();
        match peeked.as_ref().map(|tok| &tok.ttype) {
            Some(TokenType::Id(_)) => self.accept_carg(instruction).map(Option::unwrap),
            Some(_) => Err(message_incorrect_requirement(
                &self.filename,
                "a classical argument",
                peeked.unwrap(),
            )),
            None => Err(message_bad_eof(
                &self.filename,
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
        if !try_token!(self.tokens, TokenType::LBracket) {
            return Ok(Operand::Range(register_size, register_start));
        }
        let lbracket_token = require_token!(self.tokens, TokenType::LBracket).unwrap();
        let (index, index_token) = match self.expect_integer() {
            Ok((index, index_token)) => (index, index_token),
            Err(Some(other_token)) => {
                return Err(message_from_token(
                    &self.filename,
                    &other_token,
                    &format!(
                        "expected an integer index, but received '{}'",
                        other_token.text()
                    ),
                ))
            }
            Err(None) => {
                return Err(message_from_token(
                    &self.filename,
                    &lbracket_token,
                    "unexpected end-of-file while trying to find a register index",
                ))
            }
        };
        check_requirement(
            &require_token!(self.tokens, TokenType::RBracket),
            &TokenType::RBracket.text(),
            &lbracket_token,
            &self.filename,
        )?;
        if index < register_size {
            Ok(Operand::Single(register_start + index))
        } else {
            Err(message_from_token(
                &self.filename,
                &index_token,
                &format!(
                    "index {} is out-of-range for register '{}' of size {}",
                    index, name, register_size
                ),
            ))
        }
    }

    pub fn parse_version(&mut self) -> Result<(), String> {
        let token = self.tokens.next().unwrap();
        let (major, minor) = match token.ttype {
            TokenType::OpenQASM(Version { major, minor }) => (major, minor.unwrap_or(0)),
            _ => unreachable!(),
        };
        match (major, minor) {
            (2, 0) => Ok(()),
            (major, minor) => Err(message_from_token(
                &self.filename,
                &token,
                &format!(
                    "can only handle OpenQASM 2.0, but given {}.{}",
                    major, minor
                ),
            )),
        }?;
        check_requirement(
            &require_token!(self.tokens, TokenType::Semicolon),
            &TokenType::Semicolon.text(),
            &token,
            &self.filename,
        )
    }

    pub fn parse_gate_definition(&mut self) -> Result<(), String> {
        // TODO.
        Err("cannot yet handle gate definitions".into())
    }

    pub fn parse_opaque_definition(&mut self) -> Result<(), String> {
        let opaque_token = require_token!(self.tokens, TokenType::Opaque).unwrap();
        let (name, _) = self.expect_identifier(&opaque_token)?;
        let mut n_params = 0usize;
        if try_token!(self.tokens, TokenType::LParen) {
            let lparen_token = require_token!(self.tokens, TokenType::LParen).unwrap();
            while accept_token!(self.tokens, TokenType::Id(_)) {
                n_params += 1;
                if !accept_token!(self.tokens, TokenType::Comma) {
                    break;
                }
            }
            check_requirement(
                &require_token!(self.tokens, TokenType::RParen),
                &TokenType::RParen.text(),
                &lparen_token,
                &self.filename,
            )?;
        }
        let mut n_qubits = 0usize;
        while accept_token!(self.tokens, TokenType::Id(_)) {
            n_qubits += 1;
            if !accept_token!(self.tokens, TokenType::Comma) {
                break;
            }
        }
        check_requirement(
            &require_token!(self.tokens, TokenType::Semicolon),
            &TokenType::Semicolon.text(),
            &opaque_token,
            &self.filename,
        )?;
        self.bc.push(InternalByteCode::DeclareOpaque {
            name: name.clone(),
            n_params,
            n_qubits,
        });
        self.define_gate(&opaque_token, name, n_params, n_qubits)?;
        Ok(())
    }

    pub fn parse_gate_application(&mut self, condition: Option<Condition>) -> Result<(), String> {
        // We only enter this function if the caller has _seen_ that there's an identifier.
        let (name, name_token) = {
            let token = self.tokens.next().unwrap();
            match &token.ttype {
                TokenType::Id(name) => (name.clone(), token),
                _ => unreachable!(),
            }
        };
        let (index, n_params, n_qubits) = match self.symbols.get(&name) {
            Some(GlobalSymbol::Gate {
                n_params,
                n_qubits,
                index,
            }) => Ok((*index, *n_params, *n_qubits)),
            Some(_) => Err(message_from_token(
                &self.filename,
                &name_token,
                &format!("'{}' was declared as a register, not a gate", name),
            )),
            None => Err(message_from_token(
                &self.filename,
                &name_token,
                &format!("'{}' is not defined in this scope", name),
            )),
        }?;
        let mut parameters = Vec::<f64>::with_capacity(n_params);
        if try_token!(self.tokens, TokenType::LParen) {
            let lparen_token = require_token!(self.tokens, TokenType::LParen).unwrap();
            // TODO: support constant folding.
            while !try_token!(self.tokens, TokenType::RParen) {
                let parameter = match self.expect_float() {
                    Ok((parameter, _)) => Ok(parameter),
                    Err(None) => Err(message_bad_eof(
                        &self.filename,
                        "a parameter or a closing parenthesis",
                        &lparen_token,
                    )),
                    Err(Some(other)) => Err(message_incorrect_requirement(
                        &self.filename,
                        "a parameter or a closing parenthesis",
                        &other,
                    )),
                }?;
                parameters.push(parameter);
                if !accept_token!(self.tokens, TokenType::Comma) {
                    break;
                }
            }
            check_requirement(
                &require_token!(self.tokens, TokenType::RParen),
                &TokenType::RParen.text(),
                &lparen_token,
                &self.filename,
            )?;
        }
        if parameters.len() != n_params {
            return Err(message_from_token(
                &self.filename,
                &name_token,
                &format!(
                    "'{}' takes {} parameter{}, but got {}",
                    name,
                    n_params,
                    if n_params == 1 { "" } else { "s" },
                    parameters.len()
                ),
            ));
        }
        let mut qargs = Vec::<Operand>::with_capacity(n_qubits);
        while let Some(qarg) = self.accept_qarg(&name_token)? {
            qargs.push(qarg);
            if !accept_token!(self.tokens, TokenType::Comma) {
                break;
            }
        }
        if qargs.len() != n_qubits {
            return Err(message_from_token(
                &self.filename,
                &name_token,
                &format!(
                    "'{}' takes {} quantum argument{}, but got {}",
                    name,
                    n_qubits,
                    if n_qubits == 1 { "" } else { "s" },
                    qargs.len()
                ),
            ));
        }
        check_requirement(
            &require_token!(self.tokens, TokenType::Semicolon),
            &TokenType::Semicolon.text(),
            &name_token,
            &self.filename,
        )?;
        if let Some(condition) = condition {
            self.emit_conditional_gate_application(
                &name_token,
                index,
                parameters,
                &qargs,
                condition,
            )
        } else {
            self.emit_gate_application(&name_token, index, parameters, &qargs)
        }
    }

    fn emit_gate_application(
        &mut self,
        instruction: &Token,
        gate_id: usize,
        parameters: Vec<f64>,
        qargs: &[Operand],
    ) -> Result<(), String> {
        // Fast path for the most common gate patterns, which don't need broadcasting.
        if let Some(qubits) = match qargs {
            [Operand::Single(index)] => Some(vec![*index]),
            [Operand::Single(left), Operand::Single(right)] => Some(vec![*left, *right]),
            [] => Some(vec![]),
            _ => None,
        } {
            self.bc.push(InternalByteCode::Gate {
                id: gate_id,
                parameters,
                qubits,
            });
            return Ok(());
        };
        // If we're here we either have to broadcast or it's a 3+q gate - either way, we're not as
        // worried about performance.
        let mut broadcast_length = 0usize;
        for qarg in qargs {
            match qarg {
                Operand::Single(_) => (),
                Operand::Range(size, _) => {
                    if broadcast_length != 0 && broadcast_length != *size {
                        return Err(message_from_token(
                            &self.filename,
                            instruction,
                            "cannot resolve broadcast in gate application",
                        ));
                    }
                    broadcast_length = *size;
                }
            }
        }
        if broadcast_length == 0 {
            self.bc.push(InternalByteCode::Gate {
                id: gate_id,
                parameters,
                qubits: qargs
                    .iter()
                    .map(|qarg| {
                        if let Operand::Single(index) = qarg {
                            *index
                        } else {
                            unreachable!()
                        }
                    })
                    .collect(),
            });
        } else {
            for i in 0..broadcast_length {
                self.bc.push(InternalByteCode::Gate {
                    id: gate_id,
                    parameters: parameters.clone(),
                    qubits: qargs
                        .iter()
                        .map(|qarg| match qarg {
                            Operand::Single(index) => *index,
                            Operand::Range(_, start) => *start + i,
                        })
                        .collect(),
                });
            }
        }
        Ok(())
    }

    fn emit_conditional_gate_application(
        &mut self,
        instruction: &Token,
        gate_id: usize,
        parameters: Vec<f64>,
        qargs: &[Operand],
        condition: Condition,
    ) -> Result<(), String> {
        // Fast path for most common gate patterns that don't need broadcasting.
        if let Some(qubits) = match qargs {
            [Operand::Single(index)] => Some(vec![*index]),
            [Operand::Single(left), Operand::Single(right)] => Some(vec![*left, *right]),
            [] => Some(vec![]),
            _ => None,
        } {
            self.bc.push(InternalByteCode::ConditionedGate {
                id: gate_id,
                parameters,
                qubits,
                creg: condition.creg,
                value: condition.value,
            });
            return Ok(());
        };
        // If we're here we either have to broadcast or it's a 3+q gate - either way, we're not as
        // worried about performance.
        let mut broadcast_length = 0usize;
        for qarg in qargs {
            match qarg {
                Operand::Single(_) => (),
                Operand::Range(size, _) => {
                    if broadcast_length != 0 && broadcast_length != *size {
                        return Err(message_from_token(
                            &self.filename,
                            instruction,
                            "cannot resolve broadcast in gate application",
                        ));
                    }
                    broadcast_length = *size;
                }
            }
        }
        if broadcast_length == 0 {
            self.bc.push(InternalByteCode::ConditionedGate {
                id: gate_id,
                parameters,
                qubits: qargs
                    .iter()
                    .map(|qarg| {
                        if let Operand::Single(index) = qarg {
                            *index
                        } else {
                            unreachable!()
                        }
                    })
                    .collect(),
                creg: condition.creg,
                value: condition.value,
            });
        } else {
            for i in 0..broadcast_length {
                self.bc.push(InternalByteCode::ConditionedGate {
                    id: gate_id,
                    parameters: parameters.clone(),
                    qubits: qargs
                        .iter()
                        .map(|qarg| match qarg {
                            Operand::Single(index) => *index,
                            Operand::Range(_, start) => *start + i,
                        })
                        .collect(),
                    creg: condition.creg,
                    value: condition.value,
                });
            }
        }
        Ok(())
    }

    pub fn parse_conditional(&mut self) -> Result<(), String> {
        let if_token = require_token!(self.tokens, TokenType::If).unwrap();
        check_requirement(
            &require_token!(self.tokens, TokenType::LParen),
            &TokenType::LParen.text(),
            &if_token,
            &self.filename,
        )?;
        let (name, name_token) = self.expect_identifier(&if_token)?;
        let creg = match self.symbols.get(&name) {
            Some(GlobalSymbol::Creg { index, .. }) => Ok(*index),
            Some(_) => Err(message_from_token(
                &self.filename,
                &name_token,
                &format!("'{}' is not a classical register", name),
            )),
            None => Err(message_from_token(
                &self.filename,
                &name_token,
                &format!("'{}' is not defined in this scope", name),
            )),
        }?;
        check_requirement(
            &require_token!(self.tokens, TokenType::Equals),
            &TokenType::Equals.text(),
            &if_token,
            &self.filename,
        )?;
        let value = match self.expect_integer() {
            Ok((value, _)) => Ok(value),
            Err(None) => Err(message_bad_eof(&self.filename, "an integer", &if_token)),
            Err(Some(other)) => Err(message_incorrect_requirement(
                &self.filename,
                "an integer",
                &other,
            )),
        }?;
        let condition = Some(Condition { creg, value });
        check_requirement(
            &require_token!(self.tokens, TokenType::RParen),
            &TokenType::RParen.text(),
            &if_token,
            &self.filename,
        )?;
        let peeked = self.tokens.peek();
        match peeked.as_ref().map(|tok| &tok.ttype) {
            Some(TokenType::Id(_)) => self.parse_gate_application(condition),
            Some(TokenType::Measure) => self.parse_measure(condition),
            Some(TokenType::Reset) => self.parse_reset(condition),
            Some(ttype) => Err(message_from_token(&self.filename, peeked.unwrap(), &format!("expected a gate application, measurement or reset for the conditional, not '{}", ttype.text()))),
            None => Err(message_bad_eof(&self.filename, "a gate, measurement or reset to condition", &if_token)),
        }
    }

    pub fn parse_barrier(&mut self) -> Result<(), String> {
        let barrier_token = require_token!(self.tokens, TokenType::Barrier).unwrap();
        let qubits = if accept_token!(self.tokens, TokenType::Id(_)) {
            let mut qubits = Vec::new();
            while let Some(qarg) = self.accept_qarg(&barrier_token)? {
                match qarg {
                    Operand::Single(index) => qubits.push(index),
                    Operand::Range(size, start) => qubits.extend(start..start + size),
                }
                if !accept_token!(self.tokens, TokenType::Comma) {
                    break;
                }
            }
            qubits
        } else {
            (0..self.n_qubits).collect::<Vec<usize>>()
        };
        check_requirement(
            &require_token!(self.tokens, TokenType::Semicolon),
            &TokenType::Semicolon.text(),
            &barrier_token,
            &self.filename,
        )?;
        self.bc.push(InternalByteCode::Barrier { qubits });
        Ok(())
    }

    pub fn parse_measure(&mut self, condition: Option<Condition>) -> Result<(), String> {
        let measure_token = require_token!(self.tokens, TokenType::Measure).unwrap();
        let qarg = self.require_qarg(&measure_token)?;
        check_requirement(
            &require_token!(self.tokens, TokenType::Arrow),
            &TokenType::Arrow.text(),
            &measure_token,
            &self.filename,
        )?;
        let carg = self.require_carg(&measure_token)?;
        check_requirement(
            &require_token!(self.tokens, TokenType::Semicolon),
            &TokenType::Semicolon.text(),
            &measure_token,
            &self.filename,
        )?;
        if let Some(Condition { creg, value }) = condition {
            match (qarg, carg) {
                (Operand::Single(qubit), Operand::Single(clbit)) => {
                    self.bc.push(InternalByteCode::ConditionedMeasure {
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
                    self.bc
                        .extend((0..q_size).map(|i| InternalByteCode::ConditionedMeasure {
                            qubit: q_start + i,
                            clbit: c_start + i,
                            creg,
                            value,
                        }));
                    Ok(())
                }
                _ => Err(message_from_token(
                    &self.filename,
                    &measure_token,
                    "cannot resolve broadcast in measurement",
                )),
            }
        } else {
            match (qarg, carg) {
                (Operand::Single(qubit), Operand::Single(clbit)) => {
                    self.bc.push(InternalByteCode::Measure { qubit, clbit });
                    Ok(())
                }
                (Operand::Range(q_size, q_start), Operand::Range(c_size, c_start))
                    if q_size == c_size =>
                {
                    self.bc
                        .extend((0..q_size).map(|i| InternalByteCode::Measure {
                            qubit: q_start + i,
                            clbit: c_start + i,
                        }));
                    Ok(())
                }
                _ => Err(message_from_token(
                    &self.filename,
                    &measure_token,
                    "cannot resolve broadcast in measurement",
                )),
            }
        }
    }

    pub fn parse_reset(&mut self, condition: Option<Condition>) -> Result<(), String> {
        let reset_token = require_token!(self.tokens, TokenType::Reset).unwrap();
        let qarg = self.require_qarg(&reset_token)?;
        check_requirement(
            &require_token!(self.tokens, TokenType::Semicolon),
            &TokenType::Semicolon.text(),
            &reset_token,
            &self.filename,
        )?;
        if let Some(Condition { creg, value }) = condition {
            match qarg {
                Operand::Single(qubit) => {
                    self.bc
                        .push(InternalByteCode::ConditionedReset { qubit, creg, value })
                }
                Operand::Range(size, start) => self.bc.extend(
                    (start..start + size).map(|qubit| InternalByteCode::ConditionedReset {
                        qubit,
                        creg,
                        value,
                    }),
                ),
            };
        } else {
            match qarg {
                Operand::Single(qubit) => self.bc.push(InternalByteCode::Reset { qubit }),
                Operand::Range(size, start) => self
                    .bc
                    .extend((start..start + size).map(|qubit| InternalByteCode::Reset { qubit })),
            };
        }
        Ok(())
    }

    pub fn parse_creg(&mut self) -> Result<(), String> {
        let creg_token = require_token!(self.tokens, TokenType::Creg).unwrap();
        let (name, _) = self.expect_identifier(&creg_token)?;
        check_requirement(
            &require_token!(self.tokens, TokenType::LBracket),
            &TokenType::LBracket.text(),
            &creg_token,
            &self.filename,
        )?;
        let size = match self.expect_integer() {
            Ok((value, _)) => Ok(value),
            Err(None) => Err(message_bad_eof(&self.filename, "an integer", &creg_token)),
            Err(Some(other)) => Err(message_incorrect_requirement(
                &self.filename,
                "an integer",
                &other,
            )),
        }?;
        check_requirement(
            &require_token!(self.tokens, TokenType::RBracket),
            &TokenType::RBracket.text(),
            &creg_token,
            &self.filename,
        )?;
        check_requirement(
            &require_token!(self.tokens, TokenType::Semicolon),
            &TokenType::Semicolon.text(),
            &creg_token,
            &self.filename,
        )?;
        self.bc.push(InternalByteCode::DeclareCreg {
            name: name.clone(),
            size,
        });
        self.symbols.insert(
            name,
            GlobalSymbol::Creg {
                size,
                start: self.n_clbits,
                index: self.n_cregs,
            },
        );
        self.n_clbits += size;
        self.n_cregs += 1;
        Ok(())
    }

    pub fn parse_qreg(&mut self) -> Result<(), String> {
        let qreg_token = require_token!(self.tokens, TokenType::Qreg).unwrap();
        let (name, _) = self.expect_identifier(&qreg_token)?;
        check_requirement(
            &require_token!(self.tokens, TokenType::LBracket),
            &TokenType::LBracket.text(),
            &qreg_token,
            &self.filename,
        )?;
        let size = match self.expect_integer() {
            Ok((value, _)) => Ok(value),
            Err(None) => Err(message_bad_eof(&self.filename, "an integer", &qreg_token)),
            Err(Some(other)) => Err(message_incorrect_requirement(
                &self.filename,
                "an integer",
                &other,
            )),
        }?;
        check_requirement(
            &require_token!(self.tokens, TokenType::RBracket),
            &TokenType::RBracket.text(),
            &qreg_token,
            &self.filename,
        )?;
        check_requirement(
            &require_token!(self.tokens, TokenType::Semicolon),
            &TokenType::Semicolon.text(),
            &qreg_token,
            &self.filename,
        )?;
        self.bc.push(InternalByteCode::DeclareQreg {
            name: name.clone(),
            size,
        });
        self.symbols.insert(
            name,
            GlobalSymbol::Qreg {
                size,
                start: self.n_qubits,
            },
        );
        self.n_qubits += size;
        Ok(())
    }

    pub fn parse_include(&mut self) -> Result<(), String> {
        let include_token = require_token!(self.tokens, TokenType::Include).unwrap();
        let name_token = self.tokens.next();
        let name = match &name_token.as_ref().map(|tok| &tok.ttype) {
            Some(TokenType::Filename(name)) => Ok(name.clone()),
            Some(_) => Err(message_incorrect_requirement(
                &self.filename,
                "a filename",
                name_token.as_ref().unwrap(),
            )),
            None => Err(message_bad_eof(
                &self.filename,
                "a filename",
                &include_token,
            )),
        }?;
        check_requirement(
            &require_token!(self.tokens, TokenType::Semicolon),
            &TokenType::Semicolon.text(),
            &include_token,
            &self.filename,
        )?;
        if name == "qelib1.inc" {
            self.include_qelib1(&include_token)?;
            self.bc.push(InternalByteCode::SpecialInclude { name });
            Ok(())
        } else {
            Err(message_from_token(
                &self.filename,
                name_token.as_ref().unwrap(),
                "can only currently handle 'qelib1.inc'",
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
                &self.filename,
                owner,
                &format!("name '{}' is already defined", name),
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
}

fn check_requirement(
    result: &Result<Token, Option<Token>>,
    required: &str,
    cause: &Token,
    filename: &str,
) -> Result<(), String> {
    match result {
        Ok(_) => Ok(()),
        Err(None) => Err(message_bad_eof(filename, required, cause)),
        Err(Some(other)) => Err(message_incorrect_requirement(filename, required, other)),
    }
}

fn message_from_token(filename: &str, token: &Token, message: &str) -> String {
    format!(
        "{}:{},{}: {}",
        filename, token.line, token.start_col, message,
    )
}

fn message_incorrect_requirement(filename: &str, required: &str, received: &Token) -> String {
    message_from_token(
        filename,
        received,
        &format!("needed {}, but instead saw '{}'", required, received.text()),
    )
}

fn message_bad_eof(filename: &str, required: &str, owner: &Token) -> String {
    message_from_token(
        filename,
        owner,
        &format!("unexpected end-of-file when expecting to see {}", required),
    )
}

pub fn parse(tokens: TokenStream) -> Result<Vec<InternalByteCode>, String> {
    let filename = tokens.filename.clone();
    let mut state = State::new(tokens);
    if let Some(TokenType::OpenQASM(_)) = state.peek().map(|tok| &tok.ttype) {
        state.parse_version()?;
    };
    while state.peek().is_some() {
        let token = state.peek().unwrap();
        match &token.ttype {
            TokenType::Id(_) => state.parse_gate_application(None),
            TokenType::Creg => state.parse_creg(),
            TokenType::Qreg => state.parse_qreg(),
            TokenType::Include => state.parse_include(),
            TokenType::Measure => state.parse_measure(None),
            TokenType::Reset => state.parse_reset(None),
            TokenType::Barrier => state.parse_barrier(),
            TokenType::If => state.parse_conditional(),
            TokenType::Opaque => state.parse_opaque_definition(),
            TokenType::Gate => state.parse_gate_definition(),
            _ => Err(message_from_token(
                &filename,
                token,
                &format!(
                    "needed a start-of-statement token, but got {}",
                    token.text()
                ),
            )),
        }?;
    }
    Ok(state.bc)
}
