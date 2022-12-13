use core::iter::Peekable;
use core::str::Chars;

#[derive(Clone, Debug)]
pub struct Version {
    pub major: usize,
    pub minor: Option<usize>,
}

#[derive(Clone, Debug)]
pub enum TokenType {
    // Id-like
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
    // Symbol-like
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
    Id(String),
    Real(f64),
    Integer(usize),
    Filename(String),
    // Strictly `OPENQASM` and the version identifier should be two separate tokens, but we want to
    // lex the version information specially (since lexing it as a float like the spec suggests is
    // pretty weird), so we may as well just pull it into one token.
    OpenQASM(Version),
    // Not actually a valid token, used to more easily signal an error.
    Error,
}

impl TokenType {
    pub fn text(&self) -> String {
        match self {
            TokenType::Barrier => "barrier".into(),
            TokenType::Cos => "cos".into(),
            TokenType::Creg => "creg".into(),
            TokenType::Exp => "exp".into(),
            TokenType::Gate => "gate".into(),
            TokenType::If => "if".into(),
            TokenType::Include => "include".into(),
            TokenType::Ln => "ln".into(),
            TokenType::Measure => "measure".into(),
            TokenType::Opaque => "opaque".into(),
            TokenType::Qreg => "qreg".into(),
            TokenType::Reset => "reset".into(),
            TokenType::Sin => "sin".into(),
            TokenType::Sqrt => "sqrt".into(),
            TokenType::Tan => "tan".into(),
            TokenType::Pi => "pi".into(),
            TokenType::Plus => "+".into(),
            TokenType::Minus => "-".into(),
            TokenType::Arrow => "->".into(),
            TokenType::Asterisk => "*".into(),
            TokenType::Equals => "==".into(),
            TokenType::Slash => "/".into(),
            TokenType::Caret => "^".into(),
            TokenType::Semicolon => ";".into(),
            TokenType::Comma => ",".into(),
            TokenType::Colon => ":".into(),
            TokenType::LParen => "(".into(),
            TokenType::RParen => ")".into(),
            TokenType::LBracket => "[".into(),
            TokenType::RBracket => "]".into(),
            TokenType::LBrace => "{".into(),
            TokenType::RBrace => "}".into(),
            TokenType::Id(name) => name.clone(),
            // Real numbers have multiple possible representations, and since we didn't store the
            // text manually, we emit a message indicating that.
            TokenType::Real(val) => format!("<real {}>", val),
            TokenType::Integer(val) => format!("{}", val),
            TokenType::Filename(name) => format!("\"{}\"", name),
            TokenType::OpenQASM(Version { major, minor: None }) => format!("<OPENQASM {}>", major),
            TokenType::OpenQASM(Version {
                major,
                minor: Some(minor),
            }) => format!("<OPENQASM {}.{}>", major, minor),
            TokenType::Error => "<error>".into(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Token {
    pub ttype: TokenType,
    // There aren't any tokens in OQ2 that can span more than one line, except for our special
    // handling of the `OPENQASM` version identifier.  Here we just ignore that detail, at the risk
    // of emitting slightly incorrect diagnostics for that case.
    pub line: usize,
    pub start_col: usize,
    pub end_col: usize,
}

impl Token {
    pub fn text(&self) -> String {
        // For now, we just have the token type calculate the text.  For real numbers and the
        // OpenQASM version identifier, this is a bit lossy.
        self.ttype.text()
    }
}

pub struct TokenStream<'a> {
    pub filename: String,
    chars: Peekable<Chars<'a>>,
    line: usize,
    col: usize,
}

impl<'a> TokenStream<'a> {
    pub fn new(string: &'a str) -> Self {
        TokenStream {
            chars: string.chars().peekable(),
            filename: "<input>".into(),
            line: 1,
            col: 0,
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

    fn lex_float_after_decimal(&mut self, mut out: String) -> Option<TokenType> {
        // Consume the rest of the fractional part.
        while let Some(c @ '0'..='9') = self.chars.peek() {
            out.push(*c);
            self.next_char();
        }
        if !matches!(self.chars.peek(), Some('e' | 'E')) {
            return Some(TokenType::Real(out.parse().unwrap()));
        }
        // Consume the rest of the exponent.
        out.push(self.next_char().unwrap());
        if let Some('+' | '-') = self.chars.peek() {
            out.push(self.next_char().unwrap());
        }
        // Exponents must have at least one digit in them.
        if !matches!(self.chars.peek(), Some('0'..='9')) {
            return Some(TokenType::Error);
        }
        while let Some('0'..='9') = self.chars.peek() {
            out.push(self.next_char().unwrap());
        }
        Some(TokenType::Real(out.parse().unwrap()))
    }

    fn lex_numeric(&mut self, first: char) -> Option<TokenType> {
        let mut out = first.to_string();
        if first == '.' {
            return match self.next_char() {
                // In the case of a float that begins with '.', we require at least one digit, so
                // just force consume it and continue like normal.
                Some(c @ ('0'..='9')) => {
                    out.push(c);
                    self.lex_float_after_decimal(out)
                }
                _ => Some(TokenType::Error),
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
            Some(TokenType::Error)
        } else {
            Some(TokenType::Integer(out.parse().unwrap()))
        }
    }

    fn lex_textlike(&mut self, first: char) -> Option<TokenType> {
        let first_capital = matches!(first, 'A'..='Z');
        let mut out: String = first.into();
        while let Some('a'..='z' | 'A'..='Z' | '0'..='9' | '_') = self.chars.peek() {
            out.push(self.next_char().unwrap())
        }
        if first_capital {
            match out.as_str() {
                "OPENQASM" => self.lex_version(),
                "U" | "CX" => Some(TokenType::Id(out)),
                _ => Some(TokenType::Error),
            }
        } else {
            match out.as_str() {
                "barrier" => Some(TokenType::Barrier),
                "cos" => Some(TokenType::Cos),
                "creg" => Some(TokenType::Creg),
                "exp" => Some(TokenType::Exp),
                "gate" => Some(TokenType::Gate),
                "if" => Some(TokenType::If),
                "include" => Some(TokenType::Include),
                "ln" => Some(TokenType::Ln),
                "measure" => Some(TokenType::Measure),
                "opaque" => Some(TokenType::Opaque),
                "qreg" => Some(TokenType::Qreg),
                "reset" => Some(TokenType::Reset),
                "sin" => Some(TokenType::Sin),
                "sqrt" => Some(TokenType::Sqrt),
                "tan" => Some(TokenType::Tan),
                "pi" => Some(TokenType::Pi),
                _ => Some(TokenType::Id(out)),
            }
        }
    }

    fn lex_filename(&mut self) -> Option<TokenType> {
        let mut out = String::new();
        loop {
            match self.next_char() {
                None | Some('\n') | Some('\r') => return Some(TokenType::Error),
                Some('"') => return Some(TokenType::Filename(out)),
                Some(c) => {
                    out.push(c);
                }
            }
        }
    }

    fn lex_version(&mut self) -> Option<TokenType> {
        let first = loop {
            match self.next_char() {
                Some(c @ '1'..='9') => break c,
                Some(' ' | '\t' | '\n' | '\r') => (),
                Some('/') => match self.next_char() {
                    Some('/') => loop {
                        match self.next_char() {
                            Some('\n' | '\r') | None => break,
                            _ => (),
                        }
                    },
                    _ => return Some(TokenType::Error),
                },
                _ => return Some(TokenType::Error),
            }
        };
        let mut buf = first.to_string();
        while let Some('0'..='9') = self.chars.peek() {
            buf.push(self.next_char().unwrap());
        }
        let major = buf.parse().unwrap();
        if let Some('.') = self.chars.peek() {
            self.next_char();
        } else {
            return Some(TokenType::OpenQASM(Version { major, minor: None }));
        };
        match self.next_char() {
            Some(c @ '0'..='9') => match self.lex_numeric(c) {
                Some(TokenType::Integer(minor)) => Some(TokenType::OpenQASM(Version {
                    major,
                    minor: Some(minor),
                })),
                _ => Some(TokenType::Error),
            },
            _ => Some(TokenType::Error),
        }
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
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
        let ttype = match self.next_char().unwrap() {
            '+' => Some(TokenType::Plus),
            '*' => Some(TokenType::Asterisk),
            '^' => Some(TokenType::Caret),
            ';' => Some(TokenType::Semicolon),
            ',' => Some(TokenType::Comma),
            ':' => Some(TokenType::Colon),
            '(' => Some(TokenType::LParen),
            ')' => Some(TokenType::RParen),
            '[' => Some(TokenType::LBracket),
            ']' => Some(TokenType::RBracket),
            '{' => Some(TokenType::LBrace),
            '}' => Some(TokenType::RBrace),
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
                    Some(TokenType::Slash)
                }
            }
            '-' => {
                if let Some('>') = self.chars.peek() {
                    self.next_char();
                    Some(TokenType::Arrow)
                } else {
                    Some(TokenType::Minus)
                }
            }
            '=' => {
                if let Some('=') = self.chars.peek() {
                    self.next_char();
                    Some(TokenType::Equals)
                } else {
                    Some(TokenType::Error)
                }
            }
            first @ ('0'..='9') => self.lex_numeric(first),
            first @ ('a'..='z' | 'A'..='Z') => self.lex_textlike(first),
            '"' => self.lex_filename(),
            _ => Some(TokenType::Error),
        };
        ttype.map(|ttype| Token {
            ttype,
            line: self.line,
            start_col,
            end_col: self.col,
        })
    }
}
