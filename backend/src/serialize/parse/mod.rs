use std::iter::Peekable;

use super::ast::*;
use super::lexer::{self, *};

pub mod error;
use error::*;

const KEYWORDS: &'static [&'static str] = &["proc", "extern", "data", "const", "if"];

pub fn parse<'a>(src: &'a str) -> ParsingResult<'a> {
    Parser::new(src).parse()
}

struct Parser<'a> {
    tokens: Peekable<Lex<'a>>,
    errors: Vec<Error<'a>>,
    current_token: Option<Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            tokens: lex(src).peekable(),
            errors: vec![],
            current_token: None,
        }
    }

    pub fn parse(mut self) -> ParsingResult<'a> {
        let module = self.parse_module().unwrap();

        ParsingResult {
            module,
            errors: self.errors,
        }
    }

    fn parse_module(&mut self) -> Result<Module<'a>, Error<'a>> {
        let mut module = Module::default();

        while self.tokens.peek().is_some() {
            match self.parse_module_item() {
                Ok(module_item) => match module_item {
                    ModuleItem::Proc(proc) => module.procedures.push(proc),
                    ModuleItem::ExternProc(exproc) => module.external_procedures.push(exproc),
                    ModuleItem::Data(data) => module.data.push(data),
                },
                Err(err) => {
                    self.add_error(err);
                    if self
                        .try_recover([
                            RecoveryStrategy::Keyword("proc"),
                            RecoveryStrategy::Keyword("extern"),
                            RecoveryStrategy::Keyword("data"),
                        ])
                        .is_err()
                    {
                        // Having failed to recover, there is no choice but to give up on parsing.
                        break;
                    }
                }
            }
        }

        Ok(module)
    }

    fn parse_module_item(&mut self) -> Result<ModuleItem<'a>, Error<'a>> {
        let token = self.predict_token()?;
        match token.text {
            "proc" => Ok(ModuleItem::Proc(self.parse_proc()?)),
            "extern" => Ok(ModuleItem::ExternProc(self.parse_extern()?)),
            "data" => Ok(ModuleItem::Data(self.parse_data()?)),
            _ => Err(Error::unexpected(
                ExpectedKind::Keywords(vec!["proc", "extern", "data"]),
                token,
            )),
        }
    }

    fn parse_proc(&mut self) -> Result<Procedure<'a>, Error<'a>> {
        let signature = self.parse_proc_signature()?;
        let blocks = self.parse_proc_body()?;
        Ok(Procedure { signature, blocks })
    }

    fn parse_extern(&mut self) -> Result<ProcedureSignature<'a>, Error<'a>> {
        self.expect_keyword("extern")?;
        let sig = self.parse_proc_signature()?;
        self.expect_separator(";")?;
        Ok(sig)
    }

    fn parse_data(&mut self) -> Result<Data<'a>, Error<'a>> {
        self.expect_keyword("data")?;
        let name = self
            .expect_any_alphanumeric(ExpectedKind::Identifier)?
            .span();
        let typ = self.parse_type_annotation()?;
        self.expect_keyword("=")?;
        let value = self.expect_token()?.span();
        self.expect_separator(";")?;
        Ok(Data { name, value, typ })
    }

    fn parse_proc_signature(&mut self) -> Result<ProcedureSignature<'a>, Error<'a>> {
        self.expect_keyword("proc")?;

        let name = self
            .expect_any_alphanumeric(ExpectedKind::ProcedureName)?
            .span();
        let parameters = self.parse_parameter_list()?;

        let returns = if self.predict_keyword("->")? {
            self.read_token();
            vec![self.expect_any_alphanumeric(ExpectedKind::TypeName)?.span()]
        } else {
            vec![]
        };

        Ok(ProcedureSignature {
            name,
            parameters,
            returns,
        })
    }

    fn parse_parameter_list(&mut self) -> Result<Vec<Parameter<'a>>, Error<'a>> {
        self.parse_delimited_list(|p| p.parse_parameter(), "(", Between(","), ")")
    }

    fn parse_parameter(&mut self) -> Result<Parameter<'a>, Error<'a>> {
        let first = self
            .expect_any_alphanumeric(ExpectedKind::RegisterName)?
            .span();
        let maybe_second = if self.predict_keyword(":")? {
            Some(self.parse_type_annotation()?)
        } else {
            None
        };

        let (name, typ) = if let Some(second) = maybe_second {
            (Some(first), second)
        } else {
            (None, first)
        };

        Ok(Parameter { name, typ })
    }

    fn parse_type_annotation(&mut self) -> Result<Span<'a>, Error<'a>> {
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

    fn parse_proc_body(&mut self) -> Result<Vec<Block<'a>>, Error<'a>> {
        self.parse_delimited_list(|this| this.parse_block(), "{", Nothing, "}")
    }

    fn parse_block(&mut self) -> Result<Block<'a>, Error<'a>> {
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
        let instructions = self.parse_block_body()?;

        Ok(Block {
            name,
            parameters,
            instructions,
        })
    }

    fn parse_block_body(&mut self) -> Result<Vec<Instruction<'a>>, Error<'a>> {
        self.parse_delimited_list(|this| this.parse_instruction(), "{", After(";"), "}")
    }

    fn parse_instruction(&mut self) -> Result<Instruction<'a>, Error<'a>> {
        let first = self.expect_any_alphanumeric(ExpectedKind::Opcode)?;
        let second = self.predict_token()?;

        let mut destination = None;
        let opcode;

        if second.text == "=" || second.text == ":" {
            // Instruction is an assignment
            let typ = if second.text == ":" {
                // assignment with type annotation
                let typ = self.parse_type_annotation()?;
                self.expect_keyword("=")?;
                Some(typ)
            } else {
                // assignment without type annotation
                self.expect_token()?; // Skip second token which is "="
                None
            };
            destination = Some(Destination {
                name: first.span(),
                typ,
            });

            opcode = self.expect_any_alphanumeric(ExpectedKind::Opcode)?.span();
        } else {
            // Instruction is not an assignment
            opcode = first.span();
        }

        let mut operands = Vec::new();
        let mut target = None;
        let mut condition = None;

        let mut token = self.predict_token()?;
        if ["#", "@"].contains(&token.text) {
            target = Some(self.parse_instruction_target()?);
            token = self.predict_token()?;
        }

        if target.is_none()
            && token.kind == TokenKind::Alphanumeric
            && !KEYWORDS.contains(&token.text)
        {
            operands = self.parse_instruction_operands()?;
            token = self.predict_token()?;
        }
        if token.text == "if" {
            condition = Some(self.parse_instruction_condition()?);
        }

        Ok(Instruction {
            opcode,
            operands,
            destination,
            target,
            condition,
        })
    }

    fn parse_instruction_target(&mut self) -> Result<Target<'a>, Error<'a>> {
        let sigil = self.expect_any_keyword_of(&["#", "@"])?.span();

        let name = self
            .expect_any_alphanumeric(ExpectedKind::TargetName)?
            .span();

        let arguments = if self.predict_keyword("(")? {
            self.parse_delimited_list(
                |this| {
                    this.expect_any_alphanumeric(ExpectedKind::RegisterName)
                        .map(|token| token.span())
                },
                "(",
                Between(","),
                ")",
            )?
        } else {
            Vec::new()
        };

        Ok(Target {
            sigil,
            name,
            arguments,
        })
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
        // TODO Handle just  if some_register;
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
        separator: Separator,
        close: &'static str,
    ) -> Result<Vec<T>, Error<'a>> {
        // recovers on bad item, but if the list is bad, let the parent do the recovery

        // The lexer should not output tokens with empty text, so I'll use that to handle when caller doesn't want a separator.
        let sep = separator.as_str();

        self.expect_keyword(open)?;

        let mut items = Vec::new();
        while !self.predict_keyword(close)? {
            match parse_item(self) {
                Ok(item) => items.push(item),
                Err(err) if err.is_recoverable() => {
                    self.add_error(err);
                    self.try_recover([
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

            let sep_result = if separator.is_after() {
                // Expect the separator if it's an After.
                self.expect_keyword(sep)
            } else if separator.is_between() && !self.predict_keyword(close)? {
                // Expect the separator if it's a Between and the closing token isn't ahead.
                // 'expect_any_keyword_of' is used just to offer a more descriptive error.
                self.expect_any_keyword_of(&[sep, close])
            } else {
                Ok(Token::dummy())
            };
            if let Err(mut err) = sep_result {
                err.from = self.current_token().span().to;
                err.to = self.current_token().span().to + 1;
                self.add_error(err);
                self.try_recover([
                    RecoveryStrategy::AfterKeyword(sep),
                    RecoveryStrategy::MatchingPair {
                        open,
                        close,
                        balance: 1,
                    },
                ])?;
            }
        }

        self.expect_keyword(close)?;

        Ok(items)
    }

    /// Consume tokens until any of the given recovery strategy succeeds.
    ///   Keyword:
    ///     On successful recovery, the next token will be the given keyword.
    ///   AfterKeyword:
    ///     On successful recovery, the next token will be one directly
    ///      following the given keyword.
    ///   Matching pair:
    ///     On successful recovery, the next token will be the one following
    ///      the balanced matching pair.
    ///     Make sure to set the value of the balance field such that it
    ///      represents how unbalanced the matching pair initally is.
    ///      A balance of 1 or greater indicates the pair is unbalanced.
    /// Error on EOF.
    fn try_recover<const N: usize>(
        &mut self,
        mut strategies: [RecoveryStrategy; N],
    ) -> Result<(), Error<'a>> {
        'top: loop {
            for strategy in &mut strategies {
                match strategy {
                    RecoveryStrategy::Keyword(keyword) => {
                        if self.predict_keyword(keyword)? {
                            break 'top;
                        }
                    }
                    RecoveryStrategy::AfterKeyword(keyword) => {
                        if self.predict_keyword(keyword)? {
                            self.read_token();
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

    fn add_error(&mut self, err: Error<'a>) {
        self.errors.push(err);
    }

    fn predict_keyword(&mut self, keyword: &'static str) -> Result<bool, Error<'a>> {
        let token = self
            .predict_token()
            .map_eof_err_to(Error::unexpected_end_of_file(keyword))?;
        Ok(token.text == keyword)
    }

    fn _predict_any_keyword_of(&mut self, keywords: &[&'static str]) -> Result<bool, Error<'a>> {
        let token = self
            .predict_token()
            .map_eof_err_to(Error::unexpected_end_of_file(keywords))?;
        for keyword in keywords {
            if token.text == *keyword {
                self.read_token();
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn predict_token(&mut self) -> Result<Token<'a>, Error<'a>> {
        self.peek_token()
            .ok_or(Error::unexpected_end_of_file("token"))
    }

    fn expect_separator(&mut self, keyword: &'static str) -> Result<Token<'a>, Error<'a>> {
        // only consume token if it corresponds to keyword.
        // position error between current and next token.
        let next_token = self
            .predict_token()
            .map_eof_err_to(Error::unexpected_end_of_file(keyword))?;
        if next_token.text == keyword {
            self.read_token();
            Ok(next_token)
        } else {
            let mut err_token = next_token;
            err_token.start = self.current_token().end;
            err_token.end = self.current_token().end + 1;
            Err(Error::unexpected(keyword, err_token))
        }
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

    fn expect_any_keyword_of(&mut self, keywords: &[&'static str]) -> Result<Token<'a>, Error<'a>> {
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

    /// Peek next token, then, either read and return it if it's an Alphanumeric, or return None.
    /// Return None if there is no next token.
    fn _read_maybe_any_alphanumeric(&mut self) -> Option<Token<'a>> {
        if self.peek_token()?.kind == TokenKind::Alphanumeric {
            self.read_token()
        } else {
            None
        }
    }

    fn read_token(&mut self) -> Option<Token<'a>> {
        let token = self.tokens.next()?;
        self.current_token = Some(token);
        Some(token)
    }

    fn peek_token(&mut self) -> Option<Token<'a>> {
        self.tokens.peek().map(|t| *t)
    }

    fn current_token(&self) -> Token<'a> {
        self.current_token
            .expect("this method shouldn't be called before reading/expecting a token")
    }
}

enum ModuleItem<'a> {
    Proc(Procedure<'a>),
    ExternProc(ProcedureSignature<'a>),
    Data(Data<'a>),
}

enum Separator {
    Nothing,
    Between(&'static str), // Between and optionally after
    After(&'static str),   // Always after
}
use Separator::*;
impl Separator {
    pub fn is_between(&self) -> bool {
        match self {
            Self::Between(_) => true,
            _ => false,
        }
    }

    pub fn is_after(&self) -> bool {
        match self {
            Self::After(_) => true,
            _ => false,
        }
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Nothing => "",
            Self::Between(s) => s,
            Self::After(s) => s,
        }
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

#[derive(Debug, Clone)]
pub enum RecoveryStrategy {
    Keyword(&'static str),
    AfterKeyword(&'static str),
    MatchingPair {
        open: &'static str,
        close: &'static str,
        balance: i32,
    },
}

#[derive(Debug)]
pub struct ParsingResult<'a> {
    pub module: Module<'a>,
    pub errors: Vec<Error<'a>>,
}
