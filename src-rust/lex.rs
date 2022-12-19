use hashbrown::HashMap;

#[derive(Clone, Copy, Debug)]
pub enum LexerFailure {
    FailedRead,
    BadEncoding,
}

#[derive(Clone, Debug)]
pub struct Version {
    pub major: usize,
    pub minor: Option<usize>,
}

#[derive(Clone, Debug)]
pub struct TokenContext {
    text: Vec<String>,
    lookup: HashMap<String, usize>,
}

impl TokenContext {
    fn new() -> Self {
        TokenContext {
            text: vec![],
            lookup: HashMap::new(),
        }
    }

    fn index(&mut self, text: String) -> usize {
        match self.lookup.get(&text) {
            Some(index) => *index,
            None => {
                let index = self.text.len();
                self.lookup.insert(text.clone(), index);
                self.text.push(text);
                index
            }
        }
    }
}

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
    Colon,
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
            TokenType::Colon => ":",
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

#[derive(Clone, Copy, Debug)]
pub struct Token {
    pub ttype: TokenType,
    // This is just start line and end line; the spans can be calculated from the text.
    pub line: usize,
    pub col: usize,
    // Index into the TokenContext object, to retrieve the text that makes up the token.  We don't
    // resolve this into a value during lexing; that comes with annoying typing issues or storage
    // wastage.  Instead, we only convert the text into a value type when asked to by calling a
    // relevant method on the token.
    index: usize,
}

impl Token {
    pub fn dummy() -> Self {
        Token {
            ttype: TokenType::Error,
            line: 0,
            col: 0,
            index: usize::MAX,
        }
    }

    pub fn text<'a>(&self, context: &'a TokenContext) -> &'a str {
        match self.ttype {
            TokenType::Id
            | TokenType::Real
            | TokenType::Integer
            | TokenType::Filename
            | TokenType::Version => &context.text[self.index],
            _ => self.ttype.describe(),
        }
    }

    pub fn id(&self, context: &TokenContext) -> String {
        if self.ttype != TokenType::Id {
            panic!()
        }
        (&context.text[self.index]).into()
    }

    pub fn real(&self, context: &TokenContext) -> f64 {
        if self.ttype != TokenType::Real {
            panic!()
        }
        context.text[self.index].parse().unwrap()
    }

    pub fn int(&self, context: &TokenContext) -> usize {
        if self.ttype != TokenType::Integer {
            panic!()
        }
        context.text[self.index].parse().unwrap()
    }

    pub fn filename(&self, context: &TokenContext) -> String {
        if self.ttype != TokenType::Filename {
            panic!()
        }
        let out = &context.text[self.index];
        // String slicing is fine to assume bytes here, because the characters we're slicing out
        // must both be the ASCII '"', which is a single-byte UTF-8 character.
        out[1..out.len() - 1].into()
    }

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

pub struct TokenStream<T: std::io::BufRead> {
    pub filename: String,
    pub context: TokenContext,
    source: T,
    line_buffer: String,
    done: bool,
    line: usize,
    col: usize,
    try_version: bool,
    // This is a manual peekable structure (rather than using the `peekable` method of `Iterator`)
    // because we still want to be able to access the other members of the struct at the same time.
    peeked: Option<Option<Token>>,
}

impl TokenStream<std::io::Cursor<String>> {
    pub fn from_string(string: String) -> Self {
        TokenStream::new(std::io::Cursor::new(string), "<input>".to_string())
    }
}

impl TokenStream<std::io::BufReader<std::fs::File>> {
    pub fn from_path(path: &std::ffi::OsStr) -> Result<Self, std::io::Error> {
        let file = std::fs::File::open(path)?;
        Ok(TokenStream::new(
            std::io::BufReader::new(file),
            path.to_string_lossy().into(),
        ))
    }
}

impl<T: std::io::BufRead> TokenStream<T> {
    fn new(source: T, filename: String) -> Self {
        TokenStream {
            source,
            line_buffer: String::new(),
            context: TokenContext::new(),
            filename,
            // The first line is numbered "1", and the first column is "0".  The counts are
            // initialised like this so the first call to `next_byte` can easily detect that it
            // needs to extract the next line.
            line: 0,
            col: 0,
            try_version: false,
            peeked: None,
            done: false,
        }
    }

    fn advance_line(&mut self) -> Result<usize, ()> {
        if self.done {
            Ok(0)
        } else {
            self.line += 1;
            self.col = 0;
            self.line_buffer.clear();
            match self.source.read_line(&mut self.line_buffer) {
                Ok(0) => {
                    self.done = true;
                    Ok(0)
                }
                Ok(count) => Ok(count),
                Err(_) => {
                    self.done = true;
                    Err(())
                }
            }
        }
    }

    /// Get the next character in the stream.  This tracks the line and column position, and
    /// normalises all line endings to "\r\n".
    fn next_byte(&mut self) -> Result<Option<u8>, LexerFailure> {
        if self.done
            || (self.col >= self.line_buffer.len()
                && self.advance_line().map_err(|()| LexerFailure::FailedRead)? == 0)
        {
            return Ok(None);
        }
        let bytes = self.line_buffer.as_bytes();
        let out = bytes[self.col];
        self.col += 1;
        match out {
            b'\r' => Ok(Some(b'\n')),
            0x80..=0xff => {
                self.done = true;
                Err(LexerFailure::BadEncoding)
            }
            b => Ok(Some(b)),
        }
    }

    fn peek_byte(&mut self) -> Result<Option<u8>, LexerFailure> {
        if self.done
            || (self.col >= self.line_buffer.len()
                && self.advance_line().map_err(|()| LexerFailure::FailedRead)? == 0)
        {
            return Ok(None);
        }
        match self.line_buffer.as_bytes()[self.col] {
            b'\r' => Ok(Some(b'\n')),
            0x80..=0xff => {
                self.done = true;
                Err(LexerFailure::BadEncoding)
            }
            b => Ok(Some(b)),
        }
    }

    fn lex_float_after_decimal(
        &mut self,
        mut out: String,
    ) -> Result<(TokenType, Option<String>), LexerFailure> {
        // Consume the rest of the fractional part.
        while let Some(c @ b'0'..=b'9') = self.peek_byte()? {
            out.push(c as char);
            self.next_byte()?;
        }
        if !matches!(self.peek_byte()?, Some(b'e' | b'E')) {
            return Ok((TokenType::Real, Some(out)));
        }
        // Consume the rest of the exponent.
        out.push(self.next_byte()?.unwrap() as char);
        if let Some(b'+' | b'-') = self.peek_byte()? {
            out.push(self.next_byte()?.unwrap() as char);
        }
        // Exponents must have at least one digit in them.
        if !matches!(self.peek_byte()?, Some(b'0'..=b'9')) {
            return Ok((TokenType::Error, None));
        }
        while let Some(b'0'..=b'9') = self.peek_byte()? {
            out.push(self.next_byte()?.unwrap() as char);
        }
        Ok((TokenType::Real, Some(out)))
    }

    fn lex_numeric(&mut self, first: u8) -> Result<(TokenType, Option<String>), LexerFailure> {
        let mut out = (first as char).to_string();
        if first == b'.' {
            return match self.next_byte()? {
                // In the case of a float that begins with '.', we require at least one digit, so
                // just force consume it and continue like normal.
                Some(c @ (b'0'..=b'9')) => {
                    out.push(c as char);
                    self.lex_float_after_decimal(out)
                }
                _ => Ok((TokenType::Error, None)),
            };
        }
        while let Some(c @ (b'0'..=b'9')) = self.peek_byte()? {
            out.push(c as char);
            self.next_byte()?;
        }
        if let Some(c @ b'.') = self.peek_byte()? {
            out.push(c as char);
            self.next_byte()?;
            return self.lex_float_after_decimal(out);
        }
        if first == b'0' && out.len() > 1 {
            // Integers can't start with a leading zero unless they are only the single '0', but we
            // didn't see a decimal point.
            Ok((TokenType::Error, None))
        } else {
            Ok((TokenType::Integer, Some(out)))
        }
    }

    fn lex_textlike(&mut self, first: u8) -> Result<(TokenType, Option<String>), LexerFailure> {
        let first_capital = matches!(first, b'A'..=b'Z');
        let mut out: String = (first as char).to_string();
        while let Some(b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_') = self.peek_byte()? {
            out.push(self.next_byte()?.unwrap() as char)
        }
        if first_capital {
            match out.as_str() {
                "OPENQASM" => {
                    self.try_version = true;
                    Ok((TokenType::OpenQASM, None))
                }
                "U" | "CX" => Ok((TokenType::Id, Some(out))),
                _ => Ok((TokenType::Error, None)),
            }
        } else {
            match out.as_str() {
                "barrier" => Ok((TokenType::Barrier, None)),
                "cos" => Ok((TokenType::Cos, None)),
                "creg" => Ok((TokenType::Creg, None)),
                "exp" => Ok((TokenType::Exp, None)),
                "gate" => Ok((TokenType::Gate, None)),
                "if" => Ok((TokenType::If, None)),
                "include" => Ok((TokenType::Include, None)),
                "ln" => Ok((TokenType::Ln, None)),
                "measure" => Ok((TokenType::Measure, None)),
                "opaque" => Ok((TokenType::Opaque, None)),
                "qreg" => Ok((TokenType::Qreg, None)),
                "reset" => Ok((TokenType::Reset, None)),
                "sin" => Ok((TokenType::Sin, None)),
                "sqrt" => Ok((TokenType::Sqrt, None)),
                "tan" => Ok((TokenType::Tan, None)),
                "pi" => Ok((TokenType::Pi, None)),
                _ => Ok((TokenType::Id, Some(out))),
            }
        }
    }

    fn lex_filename(&mut self) -> Result<(TokenType, Option<String>), LexerFailure> {
        let mut out = String::with_capacity(16);
        out.push('"');
        loop {
            match self.next_byte()? {
                None | Some(b'\n') | Some(b'\r') => return Ok((TokenType::Error, None)),
                Some(b'"') => {
                    out.push('"');
                    return Ok((TokenType::Filename, Some(out)));
                }
                Some(c) => {
                    out.push(c as char);
                }
            }
        }
    }

    #[cold]
    fn try_lex_version(&mut self) -> Result<Option<(TokenType, Option<String>)>, LexerFailure> {
        if let Some(b'1'..=b'9') = self.peek_byte()? {
        } else {
            return Ok(None);
        }
        let mut out = (self.next_byte()?.unwrap() as char).to_string();
        while let Some(b'0'..=b'9') = self.peek_byte()? {
            out.push(self.next_byte()?.unwrap() as char);
        }
        if let Some(b'.') = self.peek_byte()? {
            out.push(self.next_byte()?.unwrap() as char);
        } else {
            return Ok(Some((TokenType::Version, Some(out))));
        };
        while let Some(b'0'..=b'9') = self.peek_byte()? {
            out.push(self.next_byte()?.unwrap() as char);
        }
        Ok(Some((TokenType::Version, Some(out))))
    }

    fn error_token(&self) -> Token {
        Token {
            ttype: TokenType::Error,
            line: self.line,
            col: self.col,
            index: usize::MAX,
        }
    }

    fn next_inner(&mut self) -> Option<Token> {
        loop {
            match self.peek_byte() {
                Ok(None) => return None,
                Ok(Some(b' ' | b'\t' | b'\r' | b'\n')) => {
                    self.col += 1;
                }
                Err(_) => return Some(self.error_token()),
                _ => break,
            }
        }
        let start_col = self.col;
        if self.try_version {
            self.try_version = false;
            match self.try_lex_version() {
                Ok(Some((ttype, text))) => {
                    return Some(Token {
                        ttype,
                        line: self.line,
                        col: start_col,
                        index: if let Some(text) = text {
                            self.context.index(text)
                        } else {
                            usize::MAX
                        },
                    });
                }
                Err(_) => return Some(self.error_token()),
                _ => (),
            }
        }
        let (ttype, text) = match self.next_byte().map(Option::unwrap) {
            Ok(b'+') => (TokenType::Plus, None),
            Ok(b'*') => (TokenType::Asterisk, None),
            Ok(b'^') => (TokenType::Caret, None),
            Ok(b';') => (TokenType::Semicolon, None),
            Ok(b',') => (TokenType::Comma, None),
            Ok(b':') => (TokenType::Colon, None),
            Ok(b'(') => (TokenType::LParen, None),
            Ok(b')') => (TokenType::RParen, None),
            Ok(b'[') => (TokenType::LBracket, None),
            Ok(b']') => (TokenType::RBracket, None),
            Ok(b'{') => (TokenType::LBrace, None),
            Ok(b'}') => (TokenType::RBrace, None),
            Ok(b'/') => {
                if let Ok(Some(b'/')) = self.peek_byte() {
                    if self.advance_line().is_ok() {
                        return self.next();
                    } else {
                        return Some(self.error_token());
                    }
                } else {
                    (TokenType::Slash, None)
                }
            }
            Ok(b'-') => {
                if let Ok(Some(b'>')) = self.peek_byte() {
                    self.col += 1;
                    (TokenType::Arrow, None)
                } else {
                    (TokenType::Minus, None)
                }
            }
            Ok(b'=') => {
                if let Ok(Some(b'=')) = self.peek_byte() {
                    self.col += 1;
                    (TokenType::Equals, None)
                } else {
                    return Some(self.error_token());
                }
            }
            Ok(first @ (b'0'..=b'9')) => {
                self.lex_numeric(first).unwrap_or((TokenType::Error, None))
            }
            Ok(first @ (b'a'..=b'z' | b'A'..=b'Z')) => {
                self.lex_textlike(first).unwrap_or((TokenType::Error, None))
            }
            Ok(b'"') => self.lex_filename().unwrap_or((TokenType::Error, None)),
            _ => return Some(self.error_token()),
        };
        Some(Token {
            ttype,
            line: self.line,
            col: start_col,
            index: if let Some(text) = text {
                self.context.index(text)
            } else {
                usize::MAX
            },
        })
    }

    pub fn peek(&mut self) -> Option<&Token> {
        if self.peeked.is_none() {
            self.peeked = Some(self.next_inner());
        }
        self.peeked.as_ref().unwrap().as_ref()
    }
}

impl<T: std::io::BufRead> Iterator for TokenStream<T> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peeked.take() {
            Some(token) => token,
            None => self.next_inner(),
        }
    }
}
