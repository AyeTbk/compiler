use super::lexer::Token;

#[derive(Debug)]
pub struct Error<'a> {
    pub kind: ErrorKind<'a>,
    pub from: usize,
    pub to: usize,
}

#[derive(Debug)]
pub enum ErrorKind<'a> {
    UnexpectedEndOfFile {
        expected: ExpectedKind,
    },
    Unexpected {
        expected: ExpectedKind,
        got: &'a str,
    },
    InvalidRegister,
}

impl<'a> Error<'a> {
    pub fn is_recoverable(&self) -> bool {
        match self.kind {
            ErrorKind::UnexpectedEndOfFile { .. } => false,
            _ => true,
        }
    }

    pub fn unexpected_end_of_file(expected: impl Into<ExpectedKind>) -> Self {
        Self {
            kind: ErrorKind::UnexpectedEndOfFile {
                expected: expected.into(),
            },
            from: 0,
            to: 0,
        }
    }

    pub fn unexpected(expected: impl Into<ExpectedKind>, token: Token<'a>) -> Self {
        Self {
            kind: ErrorKind::Unexpected {
                expected: expected.into(),
                got: token.text,
            },
            from: token.start,
            to: token.end,
        }
    }

    pub fn invalid_register(token: Token<'a>) -> Self {
        Self {
            kind: ErrorKind::InvalidRegister,
            from: token.start,
            to: token.end,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpectedKind {
    Keywords(Vec<&'static str>),
    Block,
    BlockName,
    RegisterName,
    TypeAnnotation,
    TypeName,
    Opcode,
    Operand,
}

impl From<&'static str> for ExpectedKind {
    fn from(value: &'static str) -> Self {
        Self::Keywords(vec![value])
    }
}

impl From<&'static [&'static str]> for ExpectedKind {
    fn from(value: &'static [&'static str]) -> Self {
        Self::Keywords(value.to_vec())
    }
}

pub(super) trait ResultExt<'a>: Sized {
    fn map_eof_err_to(self, err: Error<'a>) -> Self;
}

impl<'a, T> ResultExt<'a> for Result<T, Error<'a>> {
    fn map_eof_err_to(self, error: Error<'a>) -> Self {
        match self {
            Ok(ok) => Ok(ok),
            Err(err) => match err.kind {
                ErrorKind::UnexpectedEndOfFile { .. } => Err(error),
                _ => Err(err),
            },
        }
    }
}
