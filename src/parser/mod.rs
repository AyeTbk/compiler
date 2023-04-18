use std::iter::Peekable;

use crate::module::*;

pub mod lexer;
use self::lexer::*;

pub struct Parser<'a> {
    tokens: Peekable<Lex<'a>>,
    errors: Vec<Error<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            tokens: lex(src).peekable(),
            errors: vec![],
        }
    }

    pub fn parse(mut self) -> ParsingResult<'a> {
        let mut module = Module::default();

        while self.tokens.peek().is_some() {
            let proc = self.parse_proc().unwrap();
            module.procedures.push(proc);
        }

        ParsingResult {
            module,
            errors: self.errors,
        }
    }

    fn parse_proc(&mut self) -> Result<Procedure, Error<'a>> {
        self.expect_keyword("proc")?;

        let entry_name = self.expect_token()?.text.to_owned();
        let entry_params = self.parse_parameter_list()?;
        let return_typ = self.parse_typ_annotation()?;
        let entry_block = BasicBlock {
            name: entry_name,
            parameters: entry_params,
            instructions: vec![],
        };

        Ok(Procedure {
            return_typ,
            entry_block,
            other_blocks: vec![],
        })
    }

    fn parse_parameter_list(&mut self) -> Result<Vec<Parameter>, Error<'a>> {
        self.parse_delimited_list(|p| p.parse_parameter(), "(", ",", ")")
    }

    fn parse_parameter(&mut self) -> Result<Parameter, Error<'a>> {
        let register = self.parse_register()?;
        let typ = self.parse_typ_annotation()?;

        Ok(Parameter { register, typ })
    }

    fn parse_register(&mut self) -> Result<u32, Error<'a>> {
        let register_name_token = self.expect_any_alphanumeric(ExpectedKind::RegisterName)?;
        let register_name = register_name_token.text;
        if register_name.chars().count() < 2 || !register_name.starts_with('v') {
            return Err(Error::invalid_register(register_name_token));
        }

        if let Ok(register) = register_name.trim_start_matches('v').parse::<u32>() {
            Ok(register)
        } else {
            Err(Error::invalid_register(register_name_token))
        }
    }

    fn parse_typ_annotation(&mut self) -> Result<Typ, Error<'a>> {
        match self.expect_keyword(":") {
            Ok(_) => (),
            Err(err) if err.is_recoverable() => {
                let token = self.predict_token()?;
                return Err(Error::missing(ExpectedKind::TypeAnnotation, token));
            }
            Err(err) => return Err(err),
        }
        let typ = self.expect_any_alphanumeric(ExpectedKind::TypeName)?;
        Ok(Typ)
    }

    fn parse_delimited_list<T>(
        &mut self,
        parse_item: impl Fn(&mut Self) -> Result<T, Error<'a>>,
        open: &'static str,
        sep: &'static str,
        close: &'static str,
    ) -> Result<Vec<T>, Error<'a>> {
        // recovers on bad item, but if the list is bad, let the parent do the recovery

        self.expect_keyword(open)?;

        let mut items = Vec::new();
        while !self.predict_keyword(close)? {
            match parse_item(self) {
                Ok(item) => items.push(item),
                Err(err) if err.is_recoverable() => {
                    self.add_error(err);
                    self.recover_until_keywords([sep, close])?;
                }
                Err(err) => return Err(err),
            }

            // If the upcoming token doesn't end the list, expect a separator
            if !self.predict_keyword(close)? {
                self.expect_keyword(sep)?;
            }
        }
        self.expect_keyword(close)?;

        Ok(items)
    }

    fn recover_until_keywords<const N: usize>(
        &mut self,
        keywords: [&'static str; N],
    ) -> Result<(), Error<'a>> {
        // returns such that the next token to be read will be one of the keywords. Error on EOF.
        'top: loop {
            for keyword in keywords {
                let correct_prediction = self.predict_keyword(keyword)?;
                if correct_prediction {
                    break 'top;
                }
            }
            self.read_token();
        }
        Ok(())
    }

    fn recover_until_matching_pair(
        &mut self,
        open: &'static str,
        close: &'static str,
    ) -> Result<(), ()> {
        todo!()
    }

    fn add_error(&mut self, error: Error<'a>) {
        self.errors.push(error);
    }

    fn predict_keyword(&mut self, keyword: &'static str) -> Result<bool, Error<'a>> {
        let token = self.predict_token()?;
        Ok(token.text == keyword)
    }

    fn predict_token(&mut self) -> Result<Token<'a>, Error<'a>> {
        self.peek_token().ok_or(Error::unexpected_end_of_file())
    }

    fn expect_keyword(&mut self, keyword: &'static str) -> Result<Token<'a>, Error<'a>> {
        // only consume token if it corresponds to keyword

        let token = self.predict_token()?;
        if token.text == keyword {
            self.read_token();
            Ok(token)
        } else {
            Err(Error::unexpected_keyword(keyword, token))
        }
    }

    fn expect_any_alphanumeric(&mut self, what: ExpectedKind) -> Result<Token<'a>, Error<'a>> {
        // only consume token if it's an alphanumeric

        let token = self.predict_token()?;
        if token.kind == TokenKind::Alphanumeric {
            self.read_token();
            Ok(token)
        } else {
            Err(Error::unexpected(what, token))
        }
    }

    fn expect_token(&mut self) -> Result<Token<'a>, Error<'a>> {
        self.read_token().ok_or(Error::unexpected_end_of_file())
    }

    fn read_keyword(&mut self, keyword: &'static str) -> Result<Token<'a>, Option<Token<'a>>> {
        let Some(token) = self.read_token() else { return Err(None) };
        if token.text == keyword {
            Ok(token)
        } else {
            Err(Some(token))
        }
    }

    fn read_token(&mut self) -> Option<Token<'a>> {
        let token = self.tokens.next()?;
        Some(token)
    }

    fn peek_token(&mut self) -> Option<Token<'a>> {
        self.tokens.peek().map(|t| *t)
    }
}

#[derive(Debug)]
pub struct ParsingResult<'a> {
    errors: Vec<Error<'a>>,
    module: Module,
}

#[derive(Debug)]
pub struct Error<'a> {
    kind: ErrorKind<'a>,
    from: usize,
    to: usize,
}

#[derive(Debug)]
pub enum ErrorKind<'a> {
    UnexpectedEndOfFile,
    UnexpectedKeyword {
        expected: &'a str,
        got: &'a str,
    },
    Unexpected {
        expected: ExpectedKind,
        got: &'a str,
    },
    Missing(ExpectedKind),
    InvalidRegister,
}

impl<'a> Error<'a> {
    pub fn is_recoverable(&self) -> bool {
        match self.kind {
            ErrorKind::UnexpectedEndOfFile => false,
            _ => true,
        }
    }

    pub fn unexpected_end_of_file() -> Self {
        Self {
            kind: ErrorKind::UnexpectedEndOfFile,
            from: 0,
            to: 0,
        }
    }

    pub fn unexpected_keyword(expected: &'a str, token: Token<'a>) -> Self {
        Self {
            kind: ErrorKind::UnexpectedKeyword {
                expected,
                got: token.text,
            },
            from: token.start,
            to: token.end,
        }
    }

    pub fn unexpected(expected: ExpectedKind, token: Token<'a>) -> Self {
        Self {
            kind: ErrorKind::Unexpected {
                expected,
                got: token.text,
            },
            from: token.start,
            to: token.end,
        }
    }

    pub fn missing(missing: ExpectedKind, token: Token<'a>) -> Self {
        Self {
            kind: ErrorKind::Missing(missing),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExpectedKind {
    RegisterName,
    TypeAnnotation,
    TypeName,
}
