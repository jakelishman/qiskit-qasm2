use crate::lex::Token;

pub fn message_from_token(token: &Token, message: &str, filename: &str) -> String {
    format!("{}:{},{}: {}", filename, token.line, token.col, message,)
}

pub fn message_incorrect_requirement(filename: &str, required: &str, received: &Token) -> String {
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

pub fn message_bad_eof(filename: &str, required: &str, owner: &Token) -> String {
    message_from_token(
        owner,
        &format!("unexpected end-of-file when expecting to see {}", required),
        filename,
    )
}
