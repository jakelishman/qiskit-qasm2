use pyo3::create_exception;
use pyo3::exceptions::PyException;

use crate::lex::Token;

pub struct Position<'a> {
    filename: &'a std::ffi::OsStr,
    line: usize,
    col: usize,
}

impl<'a> Position<'a> {
    pub fn new(filename: &'a std::ffi::OsStr, line: usize, col: usize) -> Self {
        Self {
            filename,
            line,
            col,
        }
    }
}

impl<'a> std::fmt::Display for &Position<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{},{}",
            self.filename.to_string_lossy(),
            self.line,
            self.col
        )
    }
}

/// Create an error message that includes span data from the given [token][Token].  The base of the
/// message is `message`, and `filename` is the file the triggering OpenQASM 2 code came from.  For
/// string inputs, this can be a placeholder.
pub fn message_generic(position: Option<&Position>, message: &str) -> String {
    if let Some(position) = position {
        format!("{}: {}", position, message)
    } else {
        message.to_owned()
    }
}

/// Shorthand form for creating an error message when a particular type of token was required, but
/// something else was `received`.
pub fn message_incorrect_requirement(
    required: &str,
    received: &Token,
    filename: &std::ffi::OsStr,
) -> String {
    message_generic(
        Some(&Position::new(filename, received.line, received.col)),
        &format!(
            "needed {}, but instead saw {}",
            required,
            received.ttype.describe()
        ),
    )
}

/// Shorthand form for creating an error message when a particular type of token was required, but
/// the input ended unexpectedly.  The `owner` [Token] is whatever the token was that caused us to
/// know something else was required.
pub fn message_bad_eof(position: Option<&Position>, required: &str) -> String {
    message_generic(
        position,
        &format!("unexpected end-of-file when expecting to see {}", required),
    )
}

create_exception!(
    core,
    QASM2ParseError,
    PyException,
    "An error raised during parsing of OpenQASM 2 programs."
);
