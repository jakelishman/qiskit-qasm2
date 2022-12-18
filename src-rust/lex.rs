use core::iter::Peekable;
use core::str::Chars;

use hashbrown::HashMap;

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
            TokenType::Error => "<error>",
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

pub struct TokenStream<'a> {
    pub filename: String,
    pub context: TokenContext,
    chars: Peekable<Chars<'a>>,
    line: usize,
    col: usize,
    try_version: bool,
    // This is a manual peekable structure (rather than using the `peekable` method of `Iterator`)
    // because we still want to be able to access the other members of the struct at the same time.
    peeked: Option<Option<Token>>,
}

impl<'a> TokenStream<'a> {
    pub fn new(string: &'a str) -> Self {
        TokenStream {
            chars: string.chars().peekable(),
            context: TokenContext::new(),
            filename: "<input>".into(),
            line: 1,
            col: 0,
            try_version: false,
            peeked: None,
        }
    }

    /// Get the next character in the stream.  This tracks the line and column position, and
    /// normalises "\r\n"
    fn next_char(&mut self) -> Option<char> {
        let out = self.chars.next();
        match out {
            Some('\r') => {
                if let Some('\n') = self.chars.peek() {
                    self.chars.next();
                };
                self.line += 1;
                self.col = 0;
            }
            Some('\n') => {
                self.line += 1;
                self.col = 0;
            }
            Some(_) => {
                self.col += 1;
            }
            None => (),
        }
        out
    }

    fn lex_float_after_decimal(&mut self, mut out: String) -> (TokenType, Option<String>) {
        // Consume the rest of the fractional part.
        while let Some(c @ '0'..='9') = self.chars.peek() {
            out.push(*c);
            self.next_char();
        }
        if !matches!(self.chars.peek(), Some('e' | 'E')) {
            return (TokenType::Real, Some(out));
        }
        // Consume the rest of the exponent.
        out.push(self.next_char().unwrap());
        if let Some('+' | '-') = self.chars.peek() {
            out.push(self.next_char().unwrap());
        }
        // Exponents must have at least one digit in them.
        if !matches!(self.chars.peek(), Some('0'..='9')) {
            return (TokenType::Error, None);
        }
        while let Some('0'..='9') = self.chars.peek() {
            out.push(self.next_char().unwrap());
        }
        (TokenType::Real, Some(out))
    }

    fn lex_numeric(&mut self, first: char) -> (TokenType, Option<String>) {
        let mut out = first.to_string();
        if first == '.' {
            return match self.next_char() {
                // In the case of a float that begins with '.', we require at least one digit, so
                // just force consume it and continue like normal.
                Some(c @ ('0'..='9')) => {
                    out.push(c);
                    self.lex_float_after_decimal(out)
                }
                _ => (TokenType::Error, None),
            };
        }
        while let Some(c @ ('0'..='9')) = self.chars.peek() {
            out.push(*c);
            self.next_char();
        }
        if let Some(c @ '.') = self.chars.peek() {
            out.push(*c);
            self.next_char();
            return self.lex_float_after_decimal(out);
        }
        if first == '0' && out.len() > 1 {
            // Integers can't start with a leading zero unless they are only the single '0', but we
            // didn't see a decimal point.
            (TokenType::Error, None)
        } else {
            (TokenType::Integer, Some(out))
        }
    }

    fn lex_textlike(&mut self, first: char) -> (TokenType, Option<String>) {
        let first_capital = matches!(first, 'A'..='Z');
        let mut out: String = first.into();
        while let Some('a'..='z' | 'A'..='Z' | '0'..='9' | '_') = self.chars.peek() {
            out.push(self.next_char().unwrap())
        }
        if first_capital {
            match out.as_str() {
                "OPENQASM" => {
                    self.try_version = true;
                    (TokenType::OpenQASM, None)
                }
                "U" | "CX" => (TokenType::Id, Some(out)),
                _ => (TokenType::Error, None),
            }
        } else {
            match out.as_str() {
                "barrier" => (TokenType::Barrier, None),
                "cos" => (TokenType::Cos, None),
                "creg" => (TokenType::Creg, None),
                "exp" => (TokenType::Exp, None),
                "gate" => (TokenType::Gate, None),
                "if" => (TokenType::If, None),
                "include" => (TokenType::Include, None),
                "ln" => (TokenType::Ln, None),
                "measure" => (TokenType::Measure, None),
                "opaque" => (TokenType::Opaque, None),
                "qreg" => (TokenType::Qreg, None),
                "reset" => (TokenType::Reset, None),
                "sin" => (TokenType::Sin, None),
                "sqrt" => (TokenType::Sqrt, None),
                "tan" => (TokenType::Tan, None),
                "pi" => (TokenType::Pi, None),
                _ => (TokenType::Id, Some(out)),
            }
        }
    }

    fn lex_filename(&mut self) -> (TokenType, Option<String>) {
        let mut out = String::with_capacity(16);
        out.push('"');
        loop {
            match self.next_char() {
                None | Some('\n') | Some('\r') => return (TokenType::Error, None),
                Some('"') => {
                    out.push('"');
                    return (TokenType::Filename, Some(out));
                }
                Some(c) => {
                    out.push(c);
                }
            }
        }
    }

    #[cold]
    fn try_lex_version(&mut self) -> Option<(TokenType, Option<String>)> {
        if let Some('1'..='9') = self.chars.peek() {
        } else {
            return None;
        }
        let mut out = self.next_char().unwrap().to_string();
        while let Some('0'..='9') = self.chars.peek() {
            out.push(self.next_char().unwrap());
        }
        if let Some('.') = self.chars.peek() {
            out.push(self.next_char().unwrap());
        } else {
            return Some((TokenType::Version, Some(out)));
        };
        while let Some('0'..='9') = self.chars.peek() {
            out.push(self.next_char().unwrap());
        }
        Some((TokenType::Version, Some(out)))
    }

    fn next_inner(&mut self) -> Option<Token> {
        loop {
            match self.chars.peek() {
                None => return None,
                Some(' ' | '\t' | '\r' | '\n') => {
                    self.next_char();
                }
                _ => break,
            }
        }
        let start_col = self.col;
        if self.try_version {
            self.try_version = false;
            if let Some((ttype, text)) = self.try_lex_version() {
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
        }
        let (ttype, text) = match self.next_char().unwrap() {
            '+' => (TokenType::Plus, None),
            '*' => (TokenType::Asterisk, None),
            '^' => (TokenType::Caret, None),
            ';' => (TokenType::Semicolon, None),
            ',' => (TokenType::Comma, None),
            ':' => (TokenType::Colon, None),
            '(' => (TokenType::LParen, None),
            ')' => (TokenType::RParen, None),
            '[' => (TokenType::LBracket, None),
            ']' => (TokenType::RBracket, None),
            '{' => (TokenType::LBrace, None),
            '}' => (TokenType::RBrace, None),
            '/' => {
                if let Some('/') = self.chars.peek() {
                    self.next_char();
                    loop {
                        match self.next_char() {
                            Some('\n') | None => break,
                            _ => (),
                        }
                    }
                    return self.next();
                } else {
                    (TokenType::Slash, None)
                }
            }
            '-' => {
                if let Some('>') = self.chars.peek() {
                    self.next_char();
                    (TokenType::Arrow, None)
                } else {
                    (TokenType::Minus, None)
                }
            }
            '=' => {
                if let Some('=') = self.chars.peek() {
                    self.next_char();
                    (TokenType::Equals, None)
                } else {
                    (TokenType::Error, None)
                }
            }
            first @ ('0'..='9') => self.lex_numeric(first),
            first @ ('a'..='z' | 'A'..='Z') => self.lex_textlike(first),
            '"' => self.lex_filename(),
            _ => (TokenType::Error, None),
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

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peeked.take() {
            Some(token) => token,
            None => self.next_inner(),
        }
    }
}
