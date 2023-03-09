//! The core of the parsing algorithm.  This module contains the core logic for the
//! recursive-descent parser, which handles all statements of OpenQASM 2.  In places where we have
//! to evaluate a mathematical expression on parameters, we instead swap to a short-lived
//! operator-precedence parser.

use hashbrown::{HashMap, HashSet};
use pyo3::prelude::*;

use crate::bytecode::InternalBytecode;
use crate::error::{
    message_bad_eof, message_from_token, message_incorrect_requirement, QASM2ParseError,
};
use crate::expr::{Expr, ExprParser};
use crate::lex::{Token, TokenContext, TokenStream, TokenType, Version};
use crate::{CustomClassical, CustomInstruction};

/// The number of gates that are built in to the OpenQASM 2 language.  This is U and CX.
const N_BUILTIN_GATES: usize = 2;
/// The "qelib1.inc" special include.  The elements of the tuple are the gate name, the number of
/// parameters it takes, and the number of qubits it acts on.
const QELIB1: [(&str, usize, usize); 23] = [
    ("u3", 3, 1),
    ("u2", 2, 1),
    ("u1", 1, 1),
    ("cx", 0, 2),
    ("id", 0, 1),
    ("x", 0, 1),
    ("y", 0, 1),
    ("z", 0, 1),
    ("h", 0, 1),
    ("s", 0, 1),
    ("sdg", 0, 1),
    ("t", 0, 1),
    ("tdg", 0, 1),
    ("rx", 1, 1),
    ("ry", 1, 1),
    ("rz", 1, 1),
    ("cz", 0, 2),
    ("cy", 0, 2),
    ("ch", 0, 2),
    ("ccx", 0, 3),
    ("crz", 1, 2),
    ("cu1", 1, 2),
    ("cu3", 3, 2),
];

/// A symbol in the global symbol table.  Parameters and individual qubits can't be in the global
/// symbol table, as there is no way for them to be defined.
pub enum GlobalSymbol {
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
        custom: bool,
        defined: bool,
    },
    Classical {
        callable: PyObject,
        n_params: usize,
    },
}

impl GlobalSymbol {
    pub fn describe(&self) -> &'static str {
        match self {
            Self::Qreg { .. } => "a quantum register",
            Self::Creg { .. } => "a classical register",
            Self::Gate { .. } => "a gate",
            Self::Classical { .. } => "a custom classical function",
        }
    }
}

/// A symbol in the scope of a single gate definition.  This only includes the symbols that are
/// specifically gate-scoped.  The rest are part of [GlobalSymbol].
pub enum GateSymbol {
    Qubit { index: usize },
    Parameter { index: usize },
}

impl GateSymbol {
    pub fn describe(&self) -> &'static str {
        match self {
            Self::Qubit { .. } => "a qubit",
            Self::Parameter { .. } => "a parameter",
        }
    }
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

/// Find the first match for the partial [filename] in the directories along [path].  Returns
/// `None` if the cannot be found.
fn find_include_path(
    filename: &std::path::Path,
    path: &[std::path::PathBuf],
) -> Option<std::path::PathBuf> {
    for directory in path.iter() {
        let mut absolute_path = directory.clone();
        absolute_path.push(filename);
        if absolute_path.is_file() {
            return Some(absolute_path);
        }
    }
    None
}

/// The state of the parser (but not its output).  This struct is opaque to the rest of the
/// program; only its associated functions ever need to modify its internals.  The counts of
/// qubits, clbits, classical registers and gates are necessary to efficiently assign index keys to
/// new symbols as they arise.  We don't need to track quantum registers like this because no part
/// of the output instruction set requires a reference to a quantum register, since we resolve any
/// broadcast gate applications from within Rust.
pub struct State {
    tokens: Vec<TokenStream>,
    /// The context object that owns all the text strings that back the tokens seen so far.  This
    /// needs to be given as a read-only reference to the [Token] methods that extract information
    /// based on the text they came from.
    context: TokenContext,
    include_path: Vec<std::path::PathBuf>,
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
    /// Whether a version statement is allowed in this position.
    allow_version: bool,
    /// Whether we're in strict mode or (the default) more permissive parse.
    strict: bool,
}

impl State {
    /// Create and initialise a state for the parser.
    pub fn new(
        tokens: TokenStream,
        include_path: Vec<std::path::PathBuf>,
        custom_instructions: &[CustomInstruction],
        custom_classical: &[CustomClassical],
        strict: bool,
    ) -> PyResult<Self> {
        let mut state = State {
            tokens: vec![tokens],
            context: TokenContext::new(),
            include_path,
            // For Qiskit-created circuits, all files will have the builtin gates and `qelib1.inc`,
            // so we allocate with that in mind.  There may well be overlap between libraries and
            // custom instructions, but this is small-potatoes allocation and we'd rather not have
            // to reallocate.
            symbols: HashMap::with_capacity(
                N_BUILTIN_GATES + QELIB1.len() + custom_instructions.len() + custom_classical.len(),
            ),
            gate_symbols: HashMap::new(),
            n_qubits: 0,
            n_clbits: 0,
            n_cregs: 0,
            n_gates: 0,
            allow_version: true,
            strict,
        };
        for inst in custom_instructions {
            if state
                .symbols
                .insert(
                    inst.name.clone(),
                    GlobalSymbol::Gate {
                        n_params: inst.n_params,
                        n_qubits: inst.n_qubits,
                        index: state.n_gates,
                        custom: true,
                        defined: inst.name == "U" || inst.name == "CX" || inst.builtin,
                    },
                )
                .is_some()
            {
                return Err(QASM2ParseError::new_err(format!(
                    "duplicate custom instruction '{}'",
                    inst.name
                )));
            }
            state.n_gates += 1;
        }
        let dummy_token = Token::dummy();
        state.define_gate(&dummy_token, "U".into(), 3, 1)?;
        state.define_gate(&dummy_token, "CX".into(), 0, 2)?;
        for classical in custom_classical {
            match state.symbols.insert(
                classical.name.clone(),
                GlobalSymbol::Classical {
                    n_params: classical.n_params,
                    callable: classical.callable.clone(),
                },
            ) {
                Some(GlobalSymbol::Gate { .. }) => {
                    let message = match classical.name.as_str() {
                        "U" | "CX" => format!(
                            "custom classical instructions cannot shadow built-in gates, but got '{}'",
                            &classical.name,
                        ),
                        _ => format!(
                            "custom classical instruction '{}' has a naming clash with a custom gate",
                            &classical.name,
                        ),
                    };
                    return Err(QASM2ParseError::new_err(message));
                }
                Some(GlobalSymbol::Classical { .. }) => {
                    return Err(QASM2ParseError::new_err(format!(
                        "duplicate custom classical function '{}'",
                        &classical.name,
                    )));
                }
                _ => (),
            }
        }
        Ok(state)
    }

    /// Get the next token available in the stack of token streams, popping and removing any
    /// complete streams, except the base case.  Will only return `None` once all streams are
    /// exhausted.
    fn next_token(&mut self) -> Option<Token> {
        let mut pointer = self.tokens.len() - 1;
        while pointer > 0 {
            let out = self.tokens[pointer].next(&mut self.context);
            if out.is_some() {
                return out;
            }
            self.tokens.pop();
            pointer -= 1;
        }
        self.tokens[0].next(&mut self.context)
    }

    /// Peek the next token in the stack of token streams.  This does not remove any complete
    /// streams yet.  Will only return `None` once all streams are exhausted.
    fn peek_token(&mut self) -> Option<&Token> {
        let mut pointer = self.tokens.len() - 1;
        while pointer > 0 && self.tokens[pointer].peek(&mut self.context).is_none() {
            pointer -= 1;
        }
        self.tokens[pointer].peek(&mut self.context)
    }

    /// Get the filename associated with the currently active token stream.
    fn current_filename(&self) -> &std::ffi::OsStr {
        &self.tokens[self.tokens.len() - 1].filename
    }

    /// Take a token from the stream that is known to be present and correct, generally because it
    /// has already been peeked.  Panics if the token type is not correct.
    fn expect_known(&mut self, expected: TokenType) -> Token {
        let out = self.next_token().unwrap();
        if out.ttype != expected {
            panic!(
                "expected '{}' but got '{}'",
                expected.describe(),
                out.ttype.describe()
            )
        }
        out
    }

    /// Take the next token from the stream, expecting that it is of a particular type because it
    /// is required to be in order for the input program to be valid OpenQASM 2.  This returns the
    /// token if successful, and a suitable error message if the token type is incorrect, or the
    /// end of the file is reached.
    fn expect(&mut self, expected: TokenType, required: &str, cause: &Token) -> PyResult<Token> {
        let token = match self.next_token() {
            None => {
                return Err(QASM2ParseError::new_err(message_bad_eof(
                    self.current_filename(),
                    required,
                    cause,
                )))
            }
            Some(token) => token,
        };
        if token.ttype == expected {
            Ok(token)
        } else if token.ttype == TokenType::Error {
            Err(QASM2ParseError::new_err(message_from_token(
                &token,
                token.text(&self.context),
                self.current_filename(),
            )))
        } else {
            Err(QASM2ParseError::new_err(message_incorrect_requirement(
                self.current_filename(),
                required,
                &token,
            )))
        }
    }

    /// Take the next token from the stream, if it is of the correct type.  Returns `None` and
    /// leaves the next token in the underlying iterator if it does not match.
    fn accept(&mut self, expected: TokenType) -> Option<Token> {
        let peeked = self.peek_token();
        if peeked.is_some() && peeked.unwrap().ttype == expected {
            self.next_token()
        } else {
            None
        }
    }

    /// True if the next token in the stream matches the given type, and false if it doesn't.
    fn next_is(&mut self, expected: TokenType) -> bool {
        let peeked = self.peek_token();
        peeked.is_some() && peeked.unwrap().ttype == expected
    }

    /// If in `strict` mode, and we have a trailing comma, emit a suitable error message.
    fn check_trailing_comma(&self, comma: Option<&Token>) -> PyResult<()> {
        match (self.strict, comma) {
            (true, Some(token)) => Err(QASM2ParseError::new_err(message_from_token(
                token,
                "[strict] trailing commas in parameter and qubit lists are forbidden",
                self.current_filename(),
            ))),
            _ => Ok(()),
        }
    }

    /// Take a complete quantum argument from the token stream, if the next token is an identifier.
    /// This includes resolving any following subscript operation.  Returns an error variant if the
    /// next token _is_ an identifier, but the symbol represents something other than a quantum
    /// register, or isn't defined.  This can also be an error if the subscript is opened, but
    /// cannot be completely resolved due to a typing error or other invalid parse.  `Ok(None)` is
    /// returned if the next token in the stream does not match a possible quantum argument.
    fn accept_qarg(&mut self) -> PyResult<Option<Operand>> {
        let (name, name_token) = match self.accept(TokenType::Id) {
            None => return Ok(None),
            Some(token) => (token.id(&self.context), token),
        };
        let (register_size, register_start) = match self.symbols.get(&name) {
            Some(GlobalSymbol::Qreg { size, start }) => (*size, *start),
            Some(symbol) => {
                return Err(QASM2ParseError::new_err(message_from_token(
                    &name_token,
                    &format!(
                        "'{}' is {}, not a quantum register",
                        name,
                        symbol.describe()
                    ),
                    self.current_filename(),
                )))
            }
            None => {
                return Err(QASM2ParseError::new_err(message_from_token(
                    &name_token,
                    &format!("'{}' is not defined in this scope", name),
                    self.current_filename(),
                )))
            }
        };
        self.complete_operand(&name, register_size, register_start)
            .map(Some)
    }

    /// Take a complete quantum argument from the stream, if it matches.  This is for use within
    /// gates, and so the only valid type of quantum argument is a single qubit.
    fn accept_qarg_gate(&mut self) -> PyResult<Option<Operand>> {
        let (name, name_token) = match self.accept(TokenType::Id) {
            None => return Ok(None),
            Some(token) => (token.id(&self.context), token),
        };
        match self.gate_symbols.get(&name) {
            Some(GateSymbol::Qubit { index }) => Ok(Some(Operand::Single(*index))),
            Some(GateSymbol::Parameter { .. }) => {
                Err(QASM2ParseError::new_err(message_from_token(
                    &name_token,
                    &format!("'{}' is a parameter, not a qubit", name),
                    self.current_filename(),
                )))
            }
            None => {
                if let Some(symbol) = self.symbols.get(&name) {
                    Err(QASM2ParseError::new_err(message_from_token(
                        &name_token,
                        &format!("'{}' is {}, not a qubit", name, symbol.describe()),
                        self.current_filename(),
                    )))
                } else {
                    Err(QASM2ParseError::new_err(message_from_token(
                        &name_token,
                        &format!("'{}' is not defined in this scope", name),
                        self.current_filename(),
                    )))
                }
            }
        }
    }

    /// Take a complete quantum argument from the token stream, returning an error message if one
    /// is not present.
    fn require_qarg(&mut self, instruction: &Token) -> PyResult<Operand> {
        match self.peek_token().map(|tok| tok.ttype) {
            Some(TokenType::Id) => self.accept_qarg().map(Option::unwrap),
            Some(_) => {
                let token = self.next_token();
                Err(QASM2ParseError::new_err(message_incorrect_requirement(
                    self.current_filename(),
                    "a quantum argument",
                    &token.unwrap(),
                )))
            }
            None => Err(QASM2ParseError::new_err(message_bad_eof(
                self.current_filename(),
                "a quantum argument",
                instruction,
            ))),
        }
    }

    /// Take a complete classical argument from the token stream, if the next token is an
    /// identifier.  This includes resolving any following subscript operation.  Returns an error
    /// variant if the next token _is_ an identifier, but the symbol represents something other
    /// than a classical register, or isn't defined.  This can also be an error if the subscript is
    /// opened, but cannot be completely resolved due to a typing error or other invalid parse.
    /// `Ok(None)` is returned if the next token in the stream does not match a possible classical
    /// argument.
    fn accept_carg(&mut self) -> PyResult<Option<Operand>> {
        let (name, name_token) = match self.accept(TokenType::Id) {
            None => return Ok(None),
            Some(token) => (token.id(&self.context), token),
        };
        let (register_size, register_start) = match self.symbols.get(&name) {
            Some(GlobalSymbol::Creg { size, start, .. }) => (*size, *start),
            Some(symbol) => {
                return Err(QASM2ParseError::new_err(message_from_token(
                    &name_token,
                    &format!(
                        "'{}' is {}, not a classical register",
                        name,
                        symbol.describe()
                    ),
                    self.current_filename(),
                )))
            }
            None => {
                return Err(QASM2ParseError::new_err(message_from_token(
                    &name_token,
                    &format!("'{}' is not defined in this scope", name),
                    self.current_filename(),
                )))
            }
        };
        self.complete_operand(&name, register_size, register_start)
            .map(Some)
    }

    /// Take a complete classical argument from the token stream, returning an error message if one
    /// is not present.
    fn require_carg(&mut self, instruction: &Token) -> PyResult<Operand> {
        match self.peek_token().map(|tok| tok.ttype) {
            Some(TokenType::Id) => self.accept_carg().map(Option::unwrap),
            Some(_) => {
                let token = self.next_token();
                Err(QASM2ParseError::new_err(message_incorrect_requirement(
                    self.current_filename(),
                    "a classical argument",
                    &token.unwrap(),
                )))
            }
            None => Err(QASM2ParseError::new_err(message_bad_eof(
                self.current_filename(),
                "a classical argument",
                instruction,
            ))),
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
    ) -> PyResult<Operand> {
        let lbracket_token = match self.accept(TokenType::LBracket) {
            Some(token) => token,
            None => return Ok(Operand::Range(register_size, register_start)),
        };
        let index_token = self.expect(TokenType::Integer, "an integer index", &lbracket_token)?;
        let index = index_token.int(&self.context);
        self.expect(TokenType::RBracket, "a closing bracket", &lbracket_token)?;
        if index < register_size {
            Ok(Operand::Single(register_start + index))
        } else {
            Err(QASM2ParseError::new_err(message_from_token(
                &index_token,
                &format!(
                    "index {} is out-of-range for register '{}' of size {}",
                    index, name, register_size
                ),
                self.current_filename(),
            )))
        }
    }

    /// Parse an `OPENQASM <version>;` statement completely.  This function does not need to take
    /// the bytecode stream because the version information has no actionable effects for Qiskit
    /// to care about.  We simply error if the version supplied by the file is not the version of
    /// OpenQASM that we are able to support.  This assumes that the `OPENQASM` token is still in
    /// the stream.
    fn parse_version(&mut self) -> PyResult<usize> {
        let openqasm_token = self.expect_known(TokenType::OpenQASM);
        let version_token = self.expect(TokenType::Version, "version number", &openqasm_token)?;
        match version_token.version(&self.context) {
            Version {
                major: 2,
                minor: Some(0) | None,
            } => Ok(()),
            _ => Err(QASM2ParseError::new_err(message_from_token(
                &version_token,
                &format!(
                    "can only handle OpenQASM 2.0, but given {}",
                    version_token.text(&self.context),
                ),
                self.current_filename(),
            ))),
        }?;
        self.expect(TokenType::Semicolon, ";", &openqasm_token)?;
        Ok(0)
    }

    /// Parse a complete gate definition (including the body of the definition).  This assumes that
    /// the `gate` token is still in the scheme.  This function will likely result in many
    /// instructions being pushed onto the bytecode stream; one for the start and end of the gate
    /// definition, and then one instruction each for the gate applications in the body.
    fn parse_gate_definition(&mut self, bc: &mut Vec<Option<InternalBytecode>>) -> PyResult<usize> {
        let gate_token = self.expect_known(TokenType::Gate);
        let name_token = self.expect(TokenType::Id, "an identifier", &gate_token)?;
        let name = name_token.id(&self.context);
        // Parse the gate parameters (if any) into the symbol take.
        let mut n_params = 0usize;
        if let Some(lparen_token) = self.accept(TokenType::LParen) {
            let mut comma = None;
            while let Some(param_token) = self.accept(TokenType::Id) {
                let param_name = param_token.id(&self.context);
                if let Some(symbol) = self.gate_symbols.insert(
                    param_name.to_owned(),
                    GateSymbol::Parameter { index: n_params },
                ) {
                    return Err(QASM2ParseError::new_err(message_from_token(
                        &param_token,
                        &format!(
                            "'{}' is already defined as {}",
                            param_name,
                            symbol.describe()
                        ),
                        self.current_filename(),
                    )));
                }
                n_params += 1;
                comma = self.accept(TokenType::Comma);
                if comma.is_none() {
                    break;
                }
            }
            self.check_trailing_comma(comma.as_ref())?;
            self.expect(TokenType::RParen, "a closing parenthesis", &lparen_token)?;
        }
        // Parse the quantum parameters into the symbol table.
        let mut n_qubits = 0usize;
        let mut comma = None;
        while let Some(qubit_token) = self.accept(TokenType::Id) {
            let qubit_name = qubit_token.id(&self.context).to_owned();
            if let Some(symbol) = self
                .gate_symbols
                .insert(qubit_name.to_owned(), GateSymbol::Qubit { index: n_qubits })
            {
                return Err(QASM2ParseError::new_err(message_from_token(
                    &qubit_token,
                    &format!(
                        "'{}' is already defined as {}",
                        qubit_name,
                        symbol.describe()
                    ),
                    self.current_filename(),
                )));
            }
            n_qubits += 1;
            comma = self.accept(TokenType::Comma);
            if comma.is_none() {
                break;
            }
        }
        self.check_trailing_comma(comma.as_ref())?;
        if n_qubits == 0 {
            return if self.peek_token().is_none() {
                Err(QASM2ParseError::new_err(message_bad_eof(
                    self.current_filename(),
                    "a qubit identifier",
                    &gate_token,
                )))
            } else {
                Err(QASM2ParseError::new_err(message_from_token(
                    &gate_token,
                    "gates must act on at least one qubit",
                    self.current_filename(),
                )))
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
            match self.peek_token().map(|tok| tok.ttype) {
                Some(TokenType::Id) => statements += self.parse_gate_application(bc, None, true)?,
                Some(TokenType::Barrier) => statements += self.parse_barrier(bc, Some(n_qubits))?,
                Some(TokenType::RBrace) => {
                    self.expect_known(TokenType::RBrace);
                    break;
                }
                Some(_) => {
                    let token = self.next_token().unwrap();
                    return Err(QASM2ParseError::new_err(message_from_token(
                        &token,
                        &format!(
                            "only gate applications are valid within a 'gate' body, but saw {}",
                            token.text(&self.context)
                        ),
                        self.current_filename(),
                    )));
                }
                None => {
                    return Err(QASM2ParseError::new_err(message_bad_eof(
                        self.current_filename(),
                        "a closing brace '}' of the gate body",
                        &lbrace_token,
                    )))
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
    ) -> PyResult<usize> {
        let opaque_token = self.expect_known(TokenType::Opaque);
        let name = self
            .expect(TokenType::Id, "an identifier", &opaque_token)?
            .text(&self.context)
            .to_owned();
        let mut n_params = 0usize;
        if let Some(lparen_token) = self.accept(TokenType::LParen) {
            let mut comma = None;
            while self.accept(TokenType::Id).is_some() {
                n_params += 1;
                comma = self.accept(TokenType::Comma);
                if comma.is_none() {
                    break;
                }
            }
            self.check_trailing_comma(comma.as_ref())?;
            self.expect(TokenType::RParen, "closing parenthesis", &lparen_token)?;
        }
        let mut n_qubits = 0usize;
        let mut comma = None;
        while self.accept(TokenType::Id).is_some() {
            n_qubits += 1;
            comma = self.accept(TokenType::Comma);
            if comma.is_none() {
                break;
            }
        }
        self.check_trailing_comma(comma.as_ref())?;
        self.expect(TokenType::Semicolon, ";", &opaque_token)?;
        if n_qubits == 0 {
            return Err(QASM2ParseError::new_err(message_from_token(
                &opaque_token,
                "gates must act on at least one qubit",
                self.current_filename(),
            )));
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
    ) -> PyResult<usize> {
        let name_token = self.expect_known(TokenType::Id);
        let name = name_token.id(&self.context);
        let (index, n_params, n_qubits) = match self.symbols.get(&name) {
            Some(GlobalSymbol::Gate {
                n_params,
                n_qubits,
                index,
                custom,
                defined,
            }) => {
                if *custom && !defined {
                    Err(QASM2ParseError::new_err(message_from_token(
                        &name_token,
                        &format!(
                            "cannot use non-builtin custom instruction '{}' before definition",
                            name,
                        ),
                        self.current_filename(),
                    )))
                } else {
                    Ok((*index, *n_params, *n_qubits))
                }
            }
            Some(symbol) => Err(QASM2ParseError::new_err(message_from_token(
                &name_token,
                &format!("'{}' is {}, not a gate", name, symbol.describe()),
                self.current_filename(),
            ))),
            None => Err(QASM2ParseError::new_err(message_from_token(
                &name_token,
                &format!("'{}' is not defined in this scope", name),
                self.current_filename(),
            ))),
        }?;
        let parameters = self.expect_gate_parameters(&name_token, n_params, in_gate)?;
        let mut qargs = Vec::<Operand>::with_capacity(n_qubits);
        let mut comma = None;
        if in_gate {
            while let Some(qarg) = self.accept_qarg_gate()? {
                qargs.push(qarg);
                comma = self.accept(TokenType::Comma);
                if comma.is_none() {
                    break;
                }
            }
        } else {
            while let Some(qarg) = self.accept_qarg()? {
                qargs.push(qarg);
                comma = self.accept(TokenType::Comma);
                if comma.is_none() {
                    break;
                }
            }
        }
        self.check_trailing_comma(comma.as_ref())?;
        if qargs.len() != n_qubits {
            return match self.peek_token().map(|tok| tok.ttype) {
                Some(TokenType::Semicolon) => Err(QASM2ParseError::new_err(message_from_token(
                    &name_token,
                    &format!(
                        "'{}' takes {} quantum argument{}, but got {}",
                        name,
                        n_qubits,
                        if n_qubits == 1 { "" } else { "s" },
                        qargs.len()
                    ),
                    self.current_filename(),
                ))),
                Some(_) => Err(QASM2ParseError::new_err(message_incorrect_requirement(
                    self.current_filename(),
                    "the end of the argument list",
                    &name_token,
                ))),
                None => Err(QASM2ParseError::new_err(message_bad_eof(
                    self.current_filename(),
                    "the end of the argument list",
                    &name_token,
                ))),
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
    ) -> PyResult<GateParameters> {
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
        let mut comma = None;
        // This code duplication is to avoid duplication of allocation when parsing the far more
        // common case of expecting constant parameters for a gate application in the body of the
        // OQ2 file.
        let parameters = if in_gate {
            let mut parameters = Vec::<Expr>::with_capacity(n_params);
            while !self.next_is(TokenType::RParen) {
                let mut expr_parser = ExprParser {
                    tokens: &mut self.tokens,
                    context: &mut self.context,
                    gate_symbols: &self.gate_symbols,
                    global_symbols: &self.symbols,
                    strict: self.strict,
                };
                parameters.push(expr_parser.parse_expression(&lparen_token)?);
                seen_params += 1;
                comma = self.accept(TokenType::Comma);
                if comma.is_none() {
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
                    context: &mut self.context,
                    gate_symbols: &self.gate_symbols,
                    global_symbols: &self.symbols,
                    strict: self.strict,
                };
                match expr_parser.parse_expression(&lparen_token)? {
                    Expr::Constant(value) => parameters.push(value),
                    _ => {
                        return Err(QASM2ParseError::new_err(message_from_token(
                            &lparen_token,
                            "non-constant expression in program body",
                            self.current_filename(),
                        )))
                    }
                }
                seen_params += 1;
                comma = self.accept(TokenType::Comma);
                if comma.is_none() {
                    break;
                }
            }
            self.expect(TokenType::RParen, "')'", &lparen_token)?;
            GateParameters::Constant(parameters)
        };
        self.check_trailing_comma(comma.as_ref())?;
        if seen_params != n_params {
            return Err(QASM2ParseError::new_err(message_from_token(
                name_token,
                &format!(
                    "'{}' takes {} parameter{}, but got {}",
                    &name_token.text(&self.context),
                    n_params,
                    if n_params == 1 { "" } else { "s" },
                    seen_params
                ),
                self.current_filename(),
            )));
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
    ) -> PyResult<usize> {
        // Fast path for most common gate patterns that don't need broadcasting.
        if let Some(qubits) = match qargs {
            [Operand::Single(index)] => Some(vec![*index]),
            [Operand::Single(left), Operand::Single(right)] => {
                if *left == *right {
                    return Err(QASM2ParseError::new_err(message_from_token(
                        instruction,
                        "duplicate qubits in gate application",
                        self.current_filename(),
                    )));
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
                        return Err(QASM2ParseError::new_err(message_from_token(
                            instruction,
                            "duplicate qubits in gate application",
                            self.current_filename(),
                        )));
                    }
                }
                Operand::Range(size, start) => {
                    if broadcast_length != 0 && broadcast_length != *size {
                        return Err(QASM2ParseError::new_err(message_from_token(
                            instruction,
                            "cannot resolve broadcast in gate application",
                            self.current_filename(),
                        )));
                    }
                    for index in *start..*start + *size {
                        if !qubits.insert(index) {
                            return Err(QASM2ParseError::new_err(message_from_token(
                                instruction,
                                "duplicate qubits in gate application",
                                self.current_filename(),
                            )));
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
    ) -> PyResult<usize> {
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
    ) -> PyResult<usize> {
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
    fn parse_conditional(&mut self, bc: &mut Vec<Option<InternalBytecode>>) -> PyResult<usize> {
        let if_token = self.expect_known(TokenType::If);
        let lparen_token = self.expect(TokenType::LParen, "'('", &if_token)?;
        let name_token = self.expect(TokenType::Id, "classical register", &if_token)?;
        self.expect(TokenType::Equals, "'=='", &if_token)?;
        let value = self
            .expect(TokenType::Integer, "an integer", &if_token)?
            .int(&self.context);
        self.expect(TokenType::RParen, "')'", &lparen_token)?;
        let name = name_token.id(&self.context);
        let creg = match self.symbols.get(&name) {
            Some(GlobalSymbol::Creg { index, .. }) => Ok(*index),
            Some(symbol) => Err(QASM2ParseError::new_err(message_from_token(
                &name_token,
                &format!(
                    "'{}' is {}, not a classical register",
                    name,
                    symbol.describe()
                ),
                self.current_filename(),
            ))),
            None => Err(QASM2ParseError::new_err(message_from_token(
                &name_token,
                &format!("'{}' is not defined in this scope", name),
                self.current_filename(),
            ))),
        }?;
        let condition = Some(Condition { creg, value });
        match self.peek_token().map(|tok| tok.ttype) {
            Some(TokenType::Id) => self.parse_gate_application(bc, condition, false),
            Some(TokenType::Measure) => self.parse_measure(bc, condition),
            Some(TokenType::Reset) => self.parse_reset(bc, condition),
            Some(_) => {
                let token = self.next_token();
                Err(QASM2ParseError::new_err(message_incorrect_requirement(
                    self.current_filename(),
                    "a gate application, measurement or reset",
                    &token.unwrap(),
                )))
            }
            None => Err(QASM2ParseError::new_err(message_bad_eof(
                self.current_filename(),
                "a gate, measurement or reset to condition",
                &if_token,
            ))),
        }
    }

    /// Parse a barrier statement.  This assumes that the `barrier` token is still in the token
    /// stream.
    fn parse_barrier(
        &mut self,
        bc: &mut Vec<Option<InternalBytecode>>,
        n_gate_qubits: Option<usize>,
    ) -> PyResult<usize> {
        let barrier_token = self.expect_known(TokenType::Barrier);
        let qubits = if !self.next_is(TokenType::Semicolon) {
            let mut qubits = Vec::new();
            let mut used = HashSet::<usize>::new();
            let mut comma = None;
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
                comma = self.accept(TokenType::Comma);
                if comma.is_none() {
                    break;
                }
            }
            self.check_trailing_comma(comma.as_ref())?;
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
    ) -> PyResult<usize> {
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
                _ => Err(QASM2ParseError::new_err(message_from_token(
                    &measure_token,
                    "cannot resolve broadcast in measurement",
                    self.current_filename(),
                ))),
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
                _ => Err(QASM2ParseError::new_err(message_from_token(
                    &measure_token,
                    "cannot resolve broadcast in measurement",
                    self.current_filename(),
                ))),
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
    ) -> PyResult<usize> {
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
    fn parse_creg(&mut self, bc: &mut Vec<Option<InternalBytecode>>) -> PyResult<usize> {
        let creg_token = self.expect_known(TokenType::Creg);
        let name_token = self.expect(TokenType::Id, "a valid identifier", &creg_token)?;
        let name = name_token.id(&self.context);
        let lbracket_token = self.expect(TokenType::LBracket, "'['", &creg_token)?;
        let size = self
            .expect(TokenType::Integer, "an integer", &lbracket_token)?
            .int(&self.context);
        self.expect(TokenType::RBracket, "']'", &lbracket_token)?;
        self.expect(TokenType::Semicolon, "';'", &creg_token)?;
        match self.symbols.insert(
            name.clone(),
            GlobalSymbol::Creg {
                size,
                start: self.n_clbits,
                index: self.n_cregs,
            },
        ) {
            None | Some(GlobalSymbol::Gate { defined: false, .. }) => {
                self.n_clbits += size;
                self.n_cregs += 1;
                bc.push(Some(InternalBytecode::DeclareCreg { name, size }));
                Ok(1)
            }
            _ => Err(QASM2ParseError::new_err(message_from_token(
                &name_token,
                &format!("'{}' is already defined", name_token.id(&self.context)),
                self.current_filename(),
            ))),
        }
    }

    /// Parse a declaration of a quantum register, emitting the relevant bytecode and adding the
    /// definition to the relevant parts of the internal symbol tables in the parser state.  This
    /// assumes that the `qreg` token is still in the token stream.
    fn parse_qreg(&mut self, bc: &mut Vec<Option<InternalBytecode>>) -> PyResult<usize> {
        let qreg_token = self.expect_known(TokenType::Qreg);
        let name_token = self.expect(TokenType::Id, "a valid identifier", &qreg_token)?;
        let name = name_token.id(&self.context);
        let lbracket_token = self.expect(TokenType::LBracket, "'['", &qreg_token)?;
        let size = self
            .expect(TokenType::Integer, "an integer", &lbracket_token)?
            .int(&self.context);
        self.expect(TokenType::RBracket, "']'", &lbracket_token)?;
        self.expect(TokenType::Semicolon, "';'", &qreg_token)?;
        match self.symbols.insert(
            name.clone(),
            GlobalSymbol::Qreg {
                size,
                start: self.n_qubits,
            },
        ) {
            None | Some(GlobalSymbol::Gate { defined: false, .. }) => {
                self.n_qubits += size;
                bc.push(Some(InternalBytecode::DeclareQreg { name, size }));
                Ok(1)
            }
            _ => Err(QASM2ParseError::new_err(message_from_token(
                &name_token,
                &format!("'{}' is already defined", name_token.id(&self.context)),
                self.current_filename(),
            ))),
        }
    }

    /// Parse an include statement.  This currently only actually handles includes of `qelib1.inc`,
    /// which aren't actually parsed; the parser has a built-in version of the file that it simply
    /// updates its state with (and the Python side of the parser does the same) rather than
    /// re-parsing the same file every time.  This assumes that the `include` token is still in the
    /// token stream.
    fn parse_include(&mut self, bc: &mut Vec<Option<InternalBytecode>>) -> PyResult<usize> {
        let include_token = self.expect_known(TokenType::Include);
        let filename_token =
            self.expect(TokenType::Filename, "a filename string", &include_token)?;
        self.expect(TokenType::Semicolon, "';'", &include_token)?;
        let filename = filename_token.filename(&self.context);
        if filename == "qelib1.inc" {
            self.symbols.reserve(QELIB1.len());
            let mut indices = Vec::with_capacity(QELIB1.len());
            for (i, (name, n_params, n_qubits)) in QELIB1.iter().enumerate() {
                if self.define_gate(&include_token, name.to_string(), *n_params, *n_qubits)? {
                    indices.push(i);
                }
            }
            bc.push(Some(InternalBytecode::SpecialInclude { indices }));
            Ok(1)
        } else {
            let base_filename = std::path::PathBuf::from(&filename);
            let absolute_filename = find_include_path(&base_filename, &self.include_path)
                .ok_or_else(|| {
                    QASM2ParseError::new_err(message_from_token(
                        &filename_token,
                        &format!(
                            "unable to find '{}' in the include search path",
                            base_filename.display()
                        ),
                        self.current_filename(),
                    ))
                })?;
            let new_stream = TokenStream::from_path(absolute_filename, self.strict).map_err(|err| {
                QASM2ParseError::new_err(message_from_token(
                    &filename_token,
                    &format!("unable to open file '{}' for reading: {}", &filename, err),
                    self.current_filename(),
                ))
            })?;
            self.tokens.push(new_stream);
            self.allow_version = true;
            Ok(0)
        }
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
    ) -> PyResult<bool> {
        match self.symbols.get_mut(&name) {
            None => {
                self.symbols.insert(
                    name,
                    GlobalSymbol::Gate {
                        n_params,
                        n_qubits,
                        index: self.n_gates,
                        custom: false,
                        defined: true,
                    },
                );
                self.n_gates += 1;
                Ok(true)
            }
            Some(GlobalSymbol::Gate {
                n_params: defined_n_params,
                n_qubits: defined_n_qubits,
                custom: true,
                defined,
                ..
            }) => {
                if n_params != *defined_n_params || n_qubits != *defined_n_qubits {
                    let plural = |count: usize, singular: &str| {
                        let mut out = format!("{} {}", count, singular);
                        if count != 1 {
                            out.push('s');
                        }
                        out
                    };
                    let from_custom = format!(
                        "{} and {}",
                        plural(*defined_n_params, "parameter"),
                        plural(*defined_n_qubits, "qubit")
                    );
                    let from_program = format!(
                        "{} and {}",
                        plural(n_params, "parameter"),
                        plural(n_qubits, "qubit")
                    );
                    Err(QASM2ParseError::new_err(message_from_token(
                        owner,
                        &format!(
                            concat!(
                                "custom instruction '{}' is mismatched with its definition: ",
                                "OpenQASM program has {}, custom has {}",
                            ),
                            name, from_program, from_custom
                        ),
                        self.current_filename(),
                    )))
                } else {
                    *defined = true;
                    Ok(false)
                }
            }
            _ => Err(QASM2ParseError::new_err(message_from_token(
                owner,
                &format!("'{}' is already defined", name),
                self.current_filename(),
            ))),
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
    ) -> PyResult<Option<usize>> {
        if self.strict && self.allow_version {
            match self.peek_token().map(|tok| tok.ttype) {
                Some(TokenType::OpenQASM) => self.parse_version(),
                Some(_) => {
                    let token = self.next_token().unwrap();
                    Err(QASM2ParseError::new_err(message_from_token(
                        &token,
                        "[strict] the first statement must be 'OPENQASM 2.0;'",
                        self.current_filename(),
                    )))
                }
                None => {
                    // No message-builder function because there's no triggering token.
                    Err(QASM2ParseError::new_err(
                        "[strict] saw an empty token stream, but needed a version statement"
                            .to_string(),
                    ))
                }
            }?;
            self.allow_version = false;
        }
        let allow_version = self.allow_version;
        self.allow_version = false;
        while let Some(ttype) = self.peek_token().map(|tok| tok.ttype) {
            let emitted = match ttype {
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
                    if allow_version {
                        self.parse_version()
                    } else {
                        let token = self.next_token().unwrap();
                        Err(QASM2ParseError::new_err(message_from_token(
                            &token,
                            "only the first statement may be a version declaration",
                            self.current_filename(),
                        )))
                    }
                }
                TokenType::Semicolon => {
                    let token = self.next_token().unwrap();
                    if self.strict {
                        Err(QASM2ParseError::new_err(message_from_token(
                            &token,
                            "[strict] empty statements and/or extra semicolons are forbidden",
                            self.current_filename(),
                        )))
                    } else {
                        Ok(0)
                    }
                }
                ttype => {
                    let token = self.next_token().unwrap();
                    let base = if let TokenType::Error = ttype {
                        ""
                    } else {
                        "needed a start-of-statement token, but instead got "
                    };
                    Err(QASM2ParseError::new_err(message_from_token(
                        &token,
                        &format!(
                            "{}{}",
                            base,
                            token.text(&self.context)
                        ),
                        self.current_filename(),
                    )))
                }
            }?;
            if emitted > 0 {
                return Ok(Some(emitted));
            }
        }
        Ok(None)
    }
}
