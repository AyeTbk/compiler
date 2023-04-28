use std::iter::Peekable;

pub mod lexer;
use self::lexer::*;

pub mod ast;
use self::ast::*;

pub mod error;
use self::error::*;

pub const KEYWORDS: &'static [&'static str] = &["proc", "if"];

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
            // let proc = match self.parse_proc() {
            //     Ok(proc) => proc,
            //     Err(err) => {
            //         panic!("{:?}", err);
            //         self.add_error(err);
            //         break;
            //     }
            // };
            let proc = self.parse_proc().unwrap();
            module.procedures.push(proc);
        }

        ParsingResult {
            module,
            errors: self.errors,
        }
    }

    fn parse_proc(&mut self) -> Result<Procedure<'a>, Error<'a>> {
        self.expect_keyword("proc")?;

        let name = self.expect_token()?.span();
        let parameters = self.parse_parameter_list()?;

        let mut return_typ = None;
        if self.predict_keyword(":")? {
            return_typ = Some(self.parse_typ_annotation()?);
        }

        let basic_blocks = self.parse_proc_body()?;

        Ok(Procedure {
            name,
            parameters,
            return_typ,
            basic_blocks,
        })
    }

    fn parse_parameter_list(&mut self) -> Result<Vec<Parameter<'a>>, Error<'a>> {
        self.parse_delimited_list(|p| p.parse_parameter(), "(", Some(","), ")")
    }

    fn parse_parameter(&mut self) -> Result<Parameter<'a>, Error<'a>> {
        let name = self
            .expect_any_alphanumeric(ExpectedKind::RegisterName)?
            .span();
        let typ = self.parse_typ_annotation()?;

        Ok(Parameter { name, typ })
    }

    fn parse_typ_annotation(&mut self) -> Result<Span<'a>, Error<'a>> {
        match self.expect_keyword(":") {
            Ok(_) => (),
            Err(err) if err.is_recoverable() => {
                let token = self.predict_token()?;
                return Err(Error::unexpected(ExpectedKind::TypeAnnotation, token));
            }
            Err(err) => return Err(err),
        }
        let typ_token = self.expect_any_alphanumeric(ExpectedKind::TypeName)?;
        Ok(typ_token.span())
    }

    fn parse_proc_body(&mut self) -> Result<Vec<BasicBlock<'a>>, Error<'a>> {
        self.parse_delimited_list(|this| this.parse_basic_block(), "{", None, "}")
    }

    fn parse_basic_block(&mut self) -> Result<BasicBlock<'a>, Error<'a>> {
        let name = self
            .expect_any_alphanumeric(ExpectedKind::BlockName)?
            .span();

        let mut parameters = vec![];

        let token = self.predict_token()?;
        match token.text {
            "(" => parameters = self.parse_parameter_list()?,
            "{" => (),
            _ => return Err(Error::unexpected(ExpectedKind::Block, token)),
        }
        let instructions = self.parse_basic_block_body()?;

        Ok(BasicBlock {
            name,
            parameters,
            instructions,
        })
    }

    fn parse_basic_block_body(&mut self) -> Result<Vec<Instruction<'a>>, Error<'a>> {
        self.parse_delimited_list(|this| this.parse_instruction(), "{", None, "}")
    }

    fn parse_instruction(&mut self) -> Result<Instruction<'a>, Error<'a>> {
        let first = self.expect_any_alphanumeric(ExpectedKind::Opcode)?;
        let second = self.predict_token()?;

        let mut destination = None;
        let opcode;

        if second.text == "=" {
            // Instruction is an assignment
            destination = Some(first.span());
            self.expect_token()?; // Skip second token which is "="
            opcode = self.expect_any_alphanumeric(ExpectedKind::Opcode)?.span();
        } else {
            // Instruction is not an assignment
            opcode = first.span();
        }

        let mut operands = Vec::new();
        let mut target_block = None;
        let mut condition = None;

        let mut token = self.predict_token()?;
        if token.text == "#" {
            target_block = Some(self.parse_instruction_target_block()?);
            token = self.predict_token()?;
        }
        if token.kind == TokenKind::Alphanumeric && !KEYWORDS.contains(&token.text) {
            operands = self.parse_instruction_operands()?;
            token = self.predict_token()?;
        }
        if token.text == "if" {
            condition = Some(self.parse_instruction_condition()?);
        }

        self.expect_keyword(";")?;

        Ok(Instruction {
            opcode,
            operands,
            destination,
            target_block,
            condition,
        })
    }

    fn parse_instruction_target_block(&mut self) -> Result<Span<'a>, Error<'a>> {
        self.expect_keyword("#")?;
        let target_block_name = self
            .expect_any_alphanumeric(ExpectedKind::BlockName)?
            .span();
        Ok(target_block_name)
    }

    fn parse_instruction_operands(&mut self) -> Result<Vec<Span<'a>>, Error<'a>> {
        let mut operands = Vec::new();
        loop {
            let operand = self.expect_any_alphanumeric(ExpectedKind::Operand)?.span();
            operands.push(operand);
            if !self.predict_keyword(",")? {
                break;
            }
            self.expect_token()?;
        }
        Ok(operands)
    }

    fn parse_instruction_condition(&mut self) -> Result<Condition<'a>, Error<'a>> {
        self.expect_keyword("if")?;
        let op1 = self.expect_any_alphanumeric(ExpectedKind::Operand)?.span();
        let operator = self.expect_any_keyword_of(&["==", "!="])?;
        let op2 = self.expect_any_alphanumeric(ExpectedKind::Operand)?.span();
        if operator.text == "==" {
            Ok(Condition::Equals(op1, op2))
        } else if operator.text == "!=" {
            Ok(Condition::NotEquals(op1, op2))
        } else {
            panic!("dont happen plz tyvm");
        }
    }

    fn parse_delimited_list<T>(
        &mut self,
        parse_item: impl Fn(&mut Self) -> Result<T, Error<'a>>,
        open: &'static str,
        separator: Option<&'static str>,
        close: &'static str,
    ) -> Result<Vec<T>, Error<'a>> {
        // recovers on bad item, but if the list is bad, let the parent do the recovery

        // The lexer should not output tokens with empty text, so I'll use that to handle when caller doesn't want a separator.
        let sep = separator.unwrap_or("");

        self.expect_keyword(open)?;

        let mut items = Vec::new();
        while !self.predict_keyword(close)? {
            match parse_item(self) {
                Ok(item) => items.push(item),
                Err(err) if err.is_recoverable() => {
                    self.add_error(err);
                    self.recover([
                        RecoveryStrategy::Keyword(sep),
                        RecoveryStrategy::MatchingPair {
                            open,
                            close,
                            balance: 1,
                        },
                    ])?;
                }
                Err(err) => return Err(err),
            }

            // If the upcoming token doesn't end the list, expect a separator (if caller wants one).
            if separator.is_some() && !self.predict_keyword(close)? {
                self.expect_keyword(sep)?;
            }
        }
        self.expect_keyword(close)?;

        Ok(items)
    }

    fn recover<const N: usize>(
        &mut self,
        mut strategies: [RecoveryStrategy; N],
    ) -> Result<(), Error<'a>> {
        // returns such that the next token to be read will be one of the keywords. Error on EOF.
        'top: loop {
            for strategy in &mut strategies {
                match strategy {
                    RecoveryStrategy::Keyword(keyword) => {
                        if self.predict_keyword(keyword)? {
                            break 'top;
                        }
                    }
                    RecoveryStrategy::MatchingPair {
                        open,
                        close,
                        balance,
                    } => {
                        let token = self.predict_token()?;
                        if token.text == *open {
                            *balance += 1;
                        } else if token.text == *close {
                            *balance -= 1;
                        }
                        if *balance == 0 {
                            break 'top;
                        }
                    }
                }
            }
            self.read_token();
        }
        Ok(())
    }

    fn add_error(&mut self, error: Error<'a>) {
        self.errors.push(error);
    }

    fn predict_keyword(&mut self, keyword: &'static str) -> Result<bool, Error<'a>> {
        let token = self
            .predict_token()
            .map_eof_err_to(Error::unexpected_end_of_file(keyword))?;
        Ok(token.text == keyword)
    }

    fn predict_token(&mut self) -> Result<Token<'a>, Error<'a>> {
        self.peek_token()
            .ok_or(Error::unexpected_end_of_file("token"))
    }

    fn expect_keyword(&mut self, keyword: &'static str) -> Result<Token<'a>, Error<'a>> {
        // only consume token if it corresponds to keyword
        let token = self
            .predict_token()
            .map_eof_err_to(Error::unexpected_end_of_file(keyword))?;
        if token.text == keyword {
            self.read_token();
            Ok(token)
        } else {
            Err(Error::unexpected(keyword, token))
        }
    }

    fn expect_any_keyword_of(
        &mut self,
        keywords: &'static [&'static str],
    ) -> Result<Token<'a>, Error<'a>> {
        // only consume token if it corresponds to keyword
        let token = self
            .predict_token()
            .map_eof_err_to(Error::unexpected_end_of_file(keywords))?;
        for keyword in keywords {
            if token.text == *keyword {
                self.read_token();
                return Ok(token);
            }
        }

        Err(Error::unexpected(keywords, token))
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
        self.read_token()
            .ok_or(Error::unexpected_end_of_file("token"))
    }

    fn read_token(&mut self) -> Option<Token<'a>> {
        let token = self.tokens.next()?;
        Some(token)
    }

    fn peek_token(&mut self) -> Option<Token<'a>> {
        self.tokens.peek().map(|t| *t)
    }
}

impl<'a> lexer::Token<'a> {
    pub fn span(self) -> Span<'a> {
        Span {
            text: self.text,
            from: self.start,
            to: self.end,
        }
    }
}

#[derive(Debug)]
pub enum RecoveryStrategy {
    Keyword(&'static str),
    MatchingPair {
        open: &'static str,
        close: &'static str,
        balance: i32,
    },
}

#[derive(Debug)]
pub struct ParsingResult<'a> {
    pub errors: Vec<Error<'a>>,
    pub module: Module<'a>,
}
