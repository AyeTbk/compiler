use crate::serialize::ast::Span;

#[derive(Debug)]
pub struct Error {
    pub message: String,
    pub from: usize,
    pub to: usize,
}

impl Error {
    pub fn new(message: impl Into<String>, span: &Span) -> Self {
        Self {
            message: message.into(),
            from: span.from,
            to: span.to,
        }
    }
}
