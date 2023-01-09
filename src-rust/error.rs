use crate::lex::Token;

/// Create an error message that includes span data from the given [token][Token].  The base of the
/// message is `message`, and `filename` is the file the triggering OpenQASM 2 code came from.  For
/// string inputs, this can be a placeholder.
pub fn message_from_token(token: &Token, message: &str, filename: &std::ffi::OsStr) -> String {
    format!(
        "{}:{},{}: {}",
        filename.to_string_lossy(),
        token.line,
        token.col,
        message
    )
}

/// Shorthand form for creating an error message when a particular type of token was required, but
/// something else was `received`.
pub fn message_incorrect_requirement(
    filename: &std::ffi::OsStr,
    required: &str,
    received: &Token,
) -> String {
    message_from_token(
        received,
        &format!(
            "needed {}, but instead saw {}",
            required,
            received.ttype.describe()
        ),
        filename,
    )
}

/// Shorthand form for creating an error message when a particular type of token was required, but
/// the input ended unexpectedly.  The `owner` [Token] is whatever the token was that caused us to
/// know something else was required.
pub fn message_bad_eof(filename: &std::ffi::OsStr, required: &str, owner: &Token) -> String {
    message_from_token(
        owner,
        &format!("unexpected end-of-file when expecting to see {}", required),
        filename,
    )
}
