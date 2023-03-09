//! The lexing logic for OpenQASM 2, responsible for turning a sequence of bytes into a
//! lexed [TokenStream] for consumption by the parsing machinery.  The general strategy here is
//! quite simple; for all the symbol-like tokens, the lexer can use a very simple single-byte
//! lookahead to determine what token it needs to emit.  For keywords and identifiers, we just read
//! the identifier in completely, then produce the right token once we see the end of the
//! identifier characters.
//!
//! We effectively use a custom lexing mode to handle the version information after the `OPENQASM`
//! keyword; the spec technically says that any real number is valid, but in reality that leads to
//! weirdness like `200.0e-2` being a valid version specifier.  We do things with a custom
//! context-dependent match after seeing an `OPENQASM` token, to avoid clashes with the general
//! real-number tokenisation.

use hashbrown::HashMap;

use std::path::Path;

/// Tokenised version information data.  This is more structured than the real number suggested by
/// the specification.
#[derive(Clone, Debug)]
pub struct Version {
    pub major: usize,
    pub minor: Option<usize>,
}

/// The context that is necessary to fully extract the information from a [Token].  This owns, for
/// example, the text of each token (where a token does not have a static text representation),
/// from which the other properties can later be derived.  This struct is effectively entirely
/// opaque outside this module; the associated functions on [Token] take this context object,
/// however, and extract the information from it.
#[derive(Clone, Debug)]
pub struct TokenContext {
    text: Vec<String>,
    lookup: HashMap<Vec<u8>, usize>,
}

impl TokenContext {
    /// Create a new context for tokens.  Nothing is heap-allocated until required.
    pub fn new() -> Self {
        TokenContext {
            text: vec![],
            lookup: HashMap::new(),
        }
    }

    /// Take ownership of the given `ascii_text` of a [Token], and return an index into the
    /// [TokenContext].  This will not store strings that are already present in the context;
    /// instead, the previous index is transparently returned.
    fn index(&mut self, ascii_text: &[u8]) -> usize {
        match self.lookup.get(ascii_text) {
            Some(index) => *index,
            None => {
                let index = self.text.len();
                self.lookup.insert(ascii_text.to_vec(), index);
                self.text
                    .push(std::str::from_utf8(ascii_text).unwrap().to_owned());
                index
            }
        }
    }

    /// Directly push a string into the context, ignoring the possibility of lookups to it.  This
    /// is used to pass generated error messages from the lexer to the parser.
    fn push(&mut self, text: String) -> usize {
        let index = self.text.len();
        self.text.push(text);
        index
    }
}

// Clippy complains without this.
impl Default for TokenContext {
    fn default() -> Self {
        Self::new()
    }
}

/// An enumeration of the different types of [Token] that can be created during lexing.  This is
/// deliberately not a data enum, to make various abstract `expect` (and so on) methods more
/// ergonomic to use; one does not need to completely define the pattern match each time, but can
/// simply pass the type identifier.  This also saves memory, since the static variants do not need
/// to be aligned to include the space necessary for text pointers that would be in the non-static
/// forms, and allows strings to be shared between many tokens (using the [TokenContext] store).
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum TokenType {
    // Keywords
    OpenQASM,
    Barrier,
    Cos,
    Creg,
    Exp,
    Gate,
    If,
    Include,
    Ln,
    Measure,
    Opaque,
    Qreg,
    Reset,
    Sin,
    Sqrt,
    Tan,
    Pi,
    // Symbols
    Plus,
    Minus,
    Arrow,
    Asterisk,
    Equals,
    Slash,
    Caret,
    Semicolon,
    Comma,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    // Content
    Id,
    Real,
    Integer,
    Filename,
    Version,
    // Not actually a valid token, used to more easily signal an error.
    Error,
}

impl TokenType {
    pub fn variable_text(&self) -> bool {
        match self {
            TokenType::OpenQASM
            | TokenType::Barrier
            | TokenType::Cos
            | TokenType::Creg
            | TokenType::Exp
            | TokenType::Gate
            | TokenType::If
            | TokenType::Include
            | TokenType::Ln
            | TokenType::Measure
            | TokenType::Opaque
            | TokenType::Qreg
            | TokenType::Reset
            | TokenType::Sin
            | TokenType::Sqrt
            | TokenType::Tan
            | TokenType::Pi
            | TokenType::Plus
            | TokenType::Minus
            | TokenType::Arrow
            | TokenType::Asterisk
            | TokenType::Equals
            | TokenType::Slash
            | TokenType::Caret
            | TokenType::Semicolon
            | TokenType::Comma
            | TokenType::LParen
            | TokenType::RParen
            | TokenType::LBracket
            | TokenType::RBracket
            | TokenType::LBrace
            | TokenType::RBrace => false,
            TokenType::Id
            | TokenType::Real
            | TokenType::Integer
            | TokenType::Filename
            | TokenType::Version
            | TokenType::Error => true,
        }
    }

    /// Get a static description of the token type.  This is useful for producing messages when the
    /// full token context isn't available, or isn't important.
    pub fn describe(&self) -> &'static str {
        match self {
            TokenType::OpenQASM => "OPENQASM",
            TokenType::Barrier => "barrier",
            TokenType::Cos => "cos",
            TokenType::Creg => "creg",
            TokenType::Exp => "exp",
            TokenType::Gate => "gate",
            TokenType::If => "if",
            TokenType::Include => "include",
            TokenType::Ln => "ln",
            TokenType::Measure => "measure",
            TokenType::Opaque => "opaque",
            TokenType::Qreg => "qreg",
            TokenType::Reset => "reset",
            TokenType::Sin => "sin",
            TokenType::Sqrt => "sqrt",
            TokenType::Tan => "tan",
            TokenType::Pi => "pi",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Arrow => "->",
            TokenType::Asterisk => "*",
            TokenType::Equals => "==",
            TokenType::Slash => "/",
            TokenType::Caret => "^",
            TokenType::Semicolon => ";",
            TokenType::Comma => ",",
            TokenType::LParen => "(",
            TokenType::RParen => ")",
            TokenType::LBracket => "[",
            TokenType::RBracket => "]",
            TokenType::LBrace => "{",
            TokenType::RBrace => "}",
            TokenType::Id => "an identifier",
            TokenType::Real => "a real number",
            TokenType::Integer => "an integer",
            TokenType::Filename => "a filename string",
            TokenType::Version => "a '<major>.<minor>' version",
            TokenType::Error => "<invalid token during lexing>",
        }
    }
}

/// A representation of a token, including its type, span information and pointer to where its text
/// is stored in the context object.  These are relatively lightweight objects (though of course
/// not as light as the single type information).
#[derive(Clone, Copy, Debug)]
pub struct Token {
    pub ttype: TokenType,
    // This is just start line and end line; the spans can be calculated from the text, if you also
    // have a reference to the token context.
    pub line: usize,
    pub col: usize,
    // Index into the TokenContext object, to retrieve the text that makes up the token.  We don't
    // resolve this into a value during lexing; that comes with annoying typing issues or storage
    // wastage.  Instead, we only convert the text into a value type when asked to by calling a
    // relevant method on the token.
    index: usize,
}

impl Token {
    /// Create a dummy token that doesn't represent anything.  This is useful in a couple of places
    /// where the general functions of the parser always take a token to "blame" if something goes
    /// wrong, but the operation is being called for built-in objects (like the `U` gate) that are
    /// guaranteed to succeed.
    pub fn dummy() -> Self {
        Token {
            ttype: TokenType::Error,
            line: 0,
            col: 0,
            index: usize::MAX,
        }
    }

    /// Get a reference to the string that was seen to generate this token.
    pub fn text<'a>(&self, context: &'a TokenContext) -> &'a str {
        match self.ttype {
            TokenType::Id
            | TokenType::Real
            | TokenType::Integer
            | TokenType::Filename
            | TokenType::Version
            | TokenType::Error => &context.text[self.index],
            _ => self.ttype.describe(),
        }
    }

    /// If the token is an identifier, this method can be called to get an owned string containing
    /// the text of the identifier.  Panics if the token is not an identifier.
    pub fn id(&self, context: &TokenContext) -> String {
        if self.ttype != TokenType::Id {
            panic!()
        }
        (&context.text[self.index]).into()
    }

    /// If the token is a real number, this method can be called to evaluate its value.  Panics if
    /// the token is not a real number.
    pub fn real(&self, context: &TokenContext) -> f64 {
        if self.ttype != TokenType::Real {
            panic!()
        }
        context.text[self.index].parse().unwrap()
    }

    /// If the token is an integer (by type, not just by value), this method can be called to
    /// evaluate its value.  Panics if the token is not an integer type.
    pub fn int(&self, context: &TokenContext) -> usize {
        if self.ttype != TokenType::Integer {
            panic!()
        }
        context.text[self.index].parse().unwrap()
    }

    /// If the token is a filename path, this method can be called to get a (regular) string
    /// representing it.  Panics if the token type was not a filename.
    pub fn filename(&self, context: &TokenContext) -> String {
        if self.ttype != TokenType::Filename {
            panic!()
        }
        let out = &context.text[self.index];
        // String slicing is fine to assume bytes here, because the characters we're slicing out
        // must both be the ASCII '"', which is a single-byte UTF-8 character.
        out[1..out.len() - 1].into()
    }

    /// If the token is a version-information token, this method can be called to evaluate the
    /// version information.  Panics if the token was not of the correct type.
    pub fn version(&self, context: &TokenContext) -> Version {
        if self.ttype != TokenType::Version {
            panic!()
        }
        // Everything in the version token is a valid ASCII character, so must be a one-byte token.
        let text = &context.text[self.index];
        match text.chars().position(|c| c == '.') {
            Some(pos) => Version {
                major: text[0..pos].parse().unwrap(),
                minor: Some(text[pos + 1..text.len()].parse().unwrap()),
            },
            None => Version {
                major: text.parse().unwrap(),
                minor: None,
            },
        }
    }
}

/// The workhouse struct of the lexer.  This represents a peekable iterable object that is abstract
/// over some buffered reader.  The struct itself essentially represents the mutable state of the
/// lexer, with its main public associated functions being the iterable method [Self::next()] and
/// the [std::iter::Peekable]-like function [Self::peek()].
///
/// The stream exposes two public attributes directly: the [filename] that this stream comes from
/// (set to some placeholder value for streams that do not have a backing file), and the
/// [TokenContext] object [context], which owns all the strings of all the text representations of
/// the previously seen tokens.
pub struct TokenStream {
    /// The filename from which this stream is derived.  May be a placeholder if there is no
    /// backing file or other named resource.
    pub filename: std::ffi::OsString,
    strict: bool,
    source: Box<dyn std::io::BufRead + Send>,
    line_buffer: Vec<u8>,
    done: bool,
    line: usize,
    col: usize,
    try_version: bool,
    // This is a manual peekable structure (rather than using the `peekable` method of `Iterator`)
    // because we still want to be able to access the other members of the struct at the same time.
    peeked: Option<Option<Token>>,
}

impl TokenStream {
    /// Create and initialise a generic [TokenStream], given a source that implements
    /// [std::io::BufRead] and a filename (or resource path) that describes its source.
    fn new(
        source: Box<dyn std::io::BufRead + Send>,
        filename: std::ffi::OsString,
        strict: bool,
    ) -> Self {
        TokenStream {
            filename,
            strict,
            source,
            line_buffer: Vec::with_capacity(80),
            done: false,
            // The first line is numbered "1", and the first column is "0".  The counts are
            // initialised like this so the first call to `next_byte` can easily detect that it
            // needs to extract the next line.
            line: 0,
            col: 0,
            try_version: false,
            peeked: None,
        }
    }

    /// Create a [TokenStream] from a string containing the OpenQASM 2 program.
    pub fn from_string(string: String, strict: bool) -> Self {
        TokenStream::new(
            Box::new(std::io::Cursor::new(string)),
            "<input>".into(),
            strict,
        )
    }

    /// Create a [TokenStream] from a path containing the OpenQASM 2 program.
    pub fn from_path<P: AsRef<Path>>(path: P, strict: bool) -> Result<Self, std::io::Error> {
        let file = std::fs::File::open(path.as_ref())?;
        Ok(TokenStream::new(
            Box::new(std::io::BufReader::new(file)),
            Path::file_name(path.as_ref()).unwrap().into(),
            strict,
        ))
    }

    /// Read the next line into the managed buffer in the struct, updating the tracking information
    /// of the position, and the `done` state of the iterator.
    fn advance_line(&mut self) -> Result<usize, String> {
        if self.done {
            Ok(0)
        } else {
            self.line += 1;
            self.col = 0;
            self.line_buffer.clear();
            // We can assume that nobody's running this on ancient Mac software that uses only '\r'
            // as its linebreak character.
            match self.source.read_until(b'\n', &mut self.line_buffer) {
                Ok(count) => {
                    if count == 0 || self.line_buffer[count - 1] != b'\n' {
                        self.done = true;
                    }
                    Ok(count)
                }
                Err(err) => {
                    self.done = true;
                    Err(format!("lexer failed to read stream: {}", err))
                }
            }
        }
    }

    /// Get the next character in the stream.  This updates the line and column information for the
    /// current bute as well.
    fn next_byte(&mut self) -> Result<Option<u8>, String> {
        if self.col >= self.line_buffer.len() && self.advance_line()? == 0 {
            return Ok(None);
        }
        let out = self.line_buffer[self.col];
        self.col += 1;
        match out {
            b @ 0x80..=0xff => {
                self.done = true;
                Err(format!("encountered a non-ASCII byte: {:02X?}", b))
            }
            b => Ok(Some(b)),
        }
    }

    /// Peek at the next byte in the stream without consuming it.  This still returns an error if
    /// the next byte isn't in the valid range for OpenQASM 2, or if the file/stream has failed to
    /// read into the buffer for some reason.
    fn peek_byte(&mut self) -> Result<Option<u8>, String> {
        if self.col >= self.line_buffer.len() && self.advance_line()? == 0 {
            return Ok(None);
        }
        match self.line_buffer[self.col] {
            b @ 0x80..=0xff => {
                self.done = true;
                Err(format!("encountered a non-ASCII byte: {:02X?}", b))
            }
            b => Ok(Some(b)),
        }
    }

    /// Expect that the next byte is not a word continuation, providing a suitable error message if
    /// it is.
    fn expect_word_boundary(&mut self, after: &str) -> Result<(), String> {
        match self.peek_byte()? {
            Some(c @ (b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_')) => Err(format!(
                "expected a word boundary after {}, but saw '{}'",
                after, c as char
            )),
            _ => Ok(()),
        }
    }

    /// Complete the lexing of a floating point value after the decimal point has been consumed.
    fn lex_float_after_decimal(&mut self) -> Result<TokenType, String> {
        // Consume the rest of the fractional part.
        while let Some(b'0'..=b'9') = self.peek_byte()? {
            self.next_byte()?;
        }
        if !matches!(self.peek_byte()?, Some(b'e' | b'E')) {
            self.expect_word_boundary("a float")?;
            return Ok(TokenType::Real);
        }
        // Consume the rest of the exponent.
        self.next_byte()?;
        if let Some(b'+' | b'-') = self.peek_byte()? {
            self.next_byte()?;
        }
        // Exponents must have at least one digit in them.
        if !matches!(self.peek_byte()?, Some(b'0'..=b'9')) {
            return Err("needed to see an integer exponent for this float".to_owned());
        }
        while let Some(b'0'..=b'9') = self.peek_byte()? {
            self.next_byte()?;
        }
        self.expect_word_boundary("a float")?;
        Ok(TokenType::Real)
    }

    /// Lex a numeric token completely.  This can return a successful integer or a real number; the
    /// function distinguishes based on what it sees.
    fn lex_numeric(&mut self) -> Result<TokenType, String> {
        let first_index = self.col - 1;
        let first = self.line_buffer[first_index];
        if first == b'.' {
            return match self.next_byte()? {
                // In the case of a float that begins with '.', we require at least one digit, so
                // just force consume it and continue like normal.
                Some(b'0'..=b'9') => self.lex_float_after_decimal(),
                _ => Err(
                    "expected a numeric fractional part after the bare decimal point".to_owned(),
                ),
            };
        }
        while let Some(b'0'..=b'9') = self.peek_byte()? {
            self.next_byte()?;
        }
        if let Some(b'.') = self.peek_byte()? {
            self.next_byte()?;
            return self.lex_float_after_decimal();
        }
        if first == b'0' && self.col - first_index > 1 {
            // Integers can't start with a leading zero unless they are only the single '0', but we
            // didn't see a decimal point.
            Err("integers cannot have leading zeroes".to_owned())
        } else {
            self.expect_word_boundary("an integer")?;
            Ok(TokenType::Integer)
        }
    }

    /// Lex a text-like token into a complete token.  This can return any of the keyword-like
    /// tokens (e.g. [TokenType::Pi]), or a [TokenType::Id] if the token is not a built-in keyword.
    fn lex_textlike(&mut self) -> Result<TokenType, String> {
        let first_index = self.col - 1;
        let first = self.line_buffer[first_index];
        while let Some(b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_') = self.peek_byte()? {
            self.next_byte()?;
        }
        // No need to expect the word boundary after this, because it's the same check as above.
        let text = &self.line_buffer[first_index..self.col];
        if let b'A'..=b'Z' = first {
            match text {
                b"OPENQASM" => {
                    self.try_version = true;
                    Ok(TokenType::OpenQASM)
                }
                b"U" | b"CX" => Ok(TokenType::Id),
                _ => Err("identifiers cannot start with capital letters except for the builtins 'U' and 'CX'".to_owned()),
            }
        } else {
            match text {
                b"barrier" => Ok(TokenType::Barrier),
                b"cos" => Ok(TokenType::Cos),
                b"creg" => Ok(TokenType::Creg),
                b"exp" => Ok(TokenType::Exp),
                b"gate" => Ok(TokenType::Gate),
                b"if" => Ok(TokenType::If),
                b"include" => Ok(TokenType::Include),
                b"ln" => Ok(TokenType::Ln),
                b"measure" => Ok(TokenType::Measure),
                b"opaque" => Ok(TokenType::Opaque),
                b"qreg" => Ok(TokenType::Qreg),
                b"reset" => Ok(TokenType::Reset),
                b"sin" => Ok(TokenType::Sin),
                b"sqrt" => Ok(TokenType::Sqrt),
                b"tan" => Ok(TokenType::Tan),
                b"pi" => Ok(TokenType::Pi),
                _ => Ok(TokenType::Id),
            }
        }
    }

    /// Lex a filename token completely.  This is always triggered by seeing a `b'"'` byte in the
    /// input stream.
    fn lex_filename(&mut self, terminator: u8) -> Result<TokenType, String> {
        loop {
            match self.next_byte()? {
                None => return Err("unexpected end-of-file while lexing string literal".to_owned()),
                Some(b'\n' | b'\r') => {
                    return Err("unexpected line break while lexing string literal".to_owned())
                }
                Some(c) if c == terminator => {
                    return Ok(TokenType::Filename);
                }
                Some(_) => (),
            }
        }
    }

    /// Attempt to lex a version identifier as the next some.  Returns `true` if a version matched,
    /// and `false` if not. This function should only be called immediately after the emission of a
    /// [TokenType::OpenQASM] token; it is not valid anywhere else in an OpenQASM 2 program.
    #[cold]
    fn try_lex_version(&mut self) -> Result<bool, String> {
        if let Some(b'1'..=b'9') = self.peek_byte()? {
            self.next_byte()?;
        } else {
            return Ok(false);
        }
        while let Some(b'0'..=b'9') = self.peek_byte()? {
            self.next_byte()?;
        }
        if let Some(b'.') = self.peek_byte()? {
            self.next_byte()?;
        } else {
            return Ok(true);
        };
        // If we don't see at least one digit following the dot, it's still not a valid version.
        if let Some(b'0'..=b'9') = self.peek_byte()? {
        } else {
            return Ok(false);
        }
        while let Some(b'0'..=b'9') = self.peek_byte()? {
            self.next_byte()?;
        }
        Ok(true)
    }

    /// The actual core of the iterator.  Read from the stream (ignoring preceding whitespace)
    /// until a complete [Token] has been constructed, or the end of the iterator is reached.  This
    /// returns `Some` for all tokens, including the error token, and only returns `None` if there
    /// are no more tokens left to take.
    fn next_inner(&mut self, context: &mut TokenContext) -> Option<Token> {
        // Consume preceding whitespace.  Beware that this can still exhaust the underlying stream,
        // or scan through an invalid token in the encoding.
        loop {
            match self.peek_byte() {
                Ok(None) => return None,
                Ok(Some(b' ' | b'\t' | b'\r' | b'\n')) => {
                    self.next_byte().ok()?;
                }
                Err(message) => {
                    return Some(Token {
                        ttype: TokenType::Error,
                        line: self.line,
                        col: self.col,
                        index: context.push(message),
                    })
                }
                _ => break,
            }
        }
        let start_col = self.col;
        // This should quickly get branch-predicted to be very cold.  Even if the version fails to
        // match, we shouldn't try again, because another [Token] will get emitted instead, and
        // then we're no longer in the `OPENQASM` statement mode.
        if self.try_version {
            self.try_version = false;
            match self.try_lex_version() {
                Ok(true) => {
                    return Some(Token {
                        ttype: TokenType::Version,
                        line: self.line,
                        col: start_col,
                        index: context.index(&self.line_buffer[start_col..self.col]),
                    });
                }
                Err(message) => {
                    return Some(Token {
                        ttype: TokenType::Error,
                        line: self.line,
                        col: start_col,
                        index: context.push(message),
                    })
                }
                _ => (),
            }
        }
        // The whitespace loop (or [Self::try_lex_version]) has already peeked the next token, so
        // we know it's going to be the `Some` variant.
        let ttype_result = match self.next_byte().map(Option::unwrap) {
            Ok(b'+') => Ok(TokenType::Plus),
            Ok(b'*') => Ok(TokenType::Asterisk),
            Ok(b'^') => Ok(TokenType::Caret),
            Ok(b';') => Ok(TokenType::Semicolon),
            Ok(b',') => Ok(TokenType::Comma),
            Ok(b'(') => Ok(TokenType::LParen),
            Ok(b')') => Ok(TokenType::RParen),
            Ok(b'[') => Ok(TokenType::LBracket),
            Ok(b']') => Ok(TokenType::RBracket),
            Ok(b'{') => Ok(TokenType::LBrace),
            Ok(b'}') => Ok(TokenType::RBrace),
            Ok(b'/') => {
                if let Ok(Some(b'/')) = self.peek_byte() {
                    match self.advance_line() {
                        Ok(_) => return self.next(context),
                        Err(message) => Err(message),
                    }
                } else {
                    Ok(TokenType::Slash)
                }
            }
            Ok(b'-') => {
                if let Ok(Some(b'>')) = self.peek_byte() {
                    self.col += 1;
                    Ok(TokenType::Arrow)
                } else {
                    Ok(TokenType::Minus)
                }
            }
            Ok(b'=') => {
                if let Ok(Some(b'=')) = self.peek_byte() {
                    self.col += 1;
                    Ok(TokenType::Equals)
                } else {
                    Err("single equals '=' is never valid".to_owned())
                }
            }
            Ok(b'0'..=b'9' | b'.') => self.lex_numeric(),
            Ok(b'a'..=b'z' | b'A'..=b'Z') => self.lex_textlike(),
            Ok(c @ (b'"' | b'\'')) => {
                if self.strict && c != b'"' {
                    Err("[strict] paths must be in double quotes (\"\")".to_owned())
                } else {
                    self.lex_filename(c)
                }
            }
            Ok(c) => Err(format!(
                "encountered '{}', which doesn't match any valid tokens",
                // Non-ASCII bytes should already have been rejected by `next_byte()`.
                c as char,
            )),
            // Rust isn't smart enough to recognise that that a wildcard identifier here would
            // have type `Result<!, String>` (because the `Ok(c)` above is irrefutable for the
            // `Ok` variant), so we have to manually destructure and restructure.
            Err(message) => Err(message),
        };
        match ttype_result {
            Ok(ttype) => Some(Token {
                ttype,
                line: self.line,
                col: start_col,
                index: if ttype.variable_text() {
                    context.index(&self.line_buffer[start_col..self.col])
                } else {
                    usize::MAX
                },
            }),
            Err(message) => Some(Token {
                ttype: TokenType::Error,
                line: self.line,
                col: start_col,
                index: context.push(message),
            }),
        }
    }

    /// Get an optional reference to the next token in the iterator stream without consuming it.
    /// This is a direct analogue of the same method on the [std::iter::Peekable] struct, except it
    /// is manually defined here to avoid hiding the rest of the public fields of the [TokenStream]
    /// struct itself.
    pub fn peek(&mut self, context: &mut TokenContext) -> Option<&Token> {
        if self.peeked.is_none() {
            self.peeked = Some(self.next_inner(context));
        }
        self.peeked.as_ref().unwrap().as_ref()
    }

    pub fn next(&mut self, context: &mut TokenContext) -> Option<Token> {
        match self.peeked.take() {
            Some(token) => token,
            None => self.next_inner(context),
        }
    }
}
