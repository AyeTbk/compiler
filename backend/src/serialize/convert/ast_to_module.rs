use super::ast;
use super::ConvertAstToModuleResult;
use super::Error;
use crate::instruction::Condition;
use crate::module::StackSlot;
use crate::module::StackSlotKind;
use crate::{
    instruction::{Instruction, Opcode, Operand, SourceOperands},
    module::{Block, Blocks, Module, Parameter, Procedure, ProcedureData, Typ, Variable},
};

pub struct ConverterAstToModule {
    errors: Vec<Error>,
}

impl ConverterAstToModule {
    pub fn new() -> Self {
        Self { errors: vec![] }
    }

    pub fn parse(mut self, ast_module: &ast::Module) -> ConvertAstToModuleResult {
        let module = match self.ast_module_to_module(ast_module) {
            Ok(module) => module,
            Err(err) => {
                self.add_error(err);
                Module::default()
            }
        };

        ConvertAstToModuleResult {
            module,
            errors: self.errors,
        }
    }

    fn ast_module_to_module(&mut self, ast_module: &ast::Module) -> Result<Module, Error> {
        let mut procedures = Vec::new();

        for ast_proc in &ast_module.procedures {
            let proc = self.ast_proc_to_proc(ast_proc)?;
            procedures.push(proc);
        }

        Ok(Module { procedures })
    }

    fn ast_proc_to_proc(&mut self, ast_proc: &ast::Procedure) -> Result<Procedure, Error> {
        // FIXME handle entry block better also stack block

        if ast_proc.blocks.is_empty() {
            return Err(Error::new("procedure has no blocks", &ast_proc.name));
        }

        let maybe_ast_stack_block = ast_proc
            .blocks
            .iter()
            .find(|block| block.name.text == "stack");
        let stack_slots = if let Some(ast_stack_block) = maybe_ast_stack_block {
            self.ast_stack_block_to_stack_slots(ast_stack_block)?
        } else {
            Vec::new()
        };

        let ast_entry_block = ast_proc
            .blocks
            .iter()
            .find(|block| block.name.text == "entry")
            .ok_or_else(|| Error::new("missing entry block", &ast_proc.name))?;
        let ast_other_blocks = ast_proc
            .blocks
            .iter()
            .filter(|block| !(block.name.text == "stack" || block.name.text == "entry"));

        if !ast_entry_block.parameters.is_empty() {
            self.add_error(Error::new(
                "entry block shouldn't have parameters",
                &ast_entry_block.name,
            ));
        }

        let entry_block = self.ast_block_to_block(ast_entry_block)?;

        let mut other_blocks = Vec::new();
        for ast_block in ast_other_blocks {
            other_blocks.push(self.ast_block_to_block(ast_block)?);
        }

        let proc = Procedure {
            name: ast_proc.name.text.to_string(),
            parameters: ast_map(&ast_proc.parameters, Self::ast_parameter_to_parameter)?,
            returns: ast_map(&ast_proc.returns, Self::ast_parameter_to_parameter)?,
            blocks: Blocks {
                entry: entry_block,
                others: other_blocks,
            },
            data: ProcedureData {
                stack_slots,
                highest_virtual_id: 1000, // FIXME actually get the highest id from the AST
            },
            calling_convention: None,
        };

        Ok(proc)
    }

    fn ast_block_to_block(&mut self, ast_block: &ast::Block) -> Result<Block, Error> {
        let parameters = ast_map(&ast_block.parameters, Self::ast_parameter_to_parameter)?;

        let mut instructions = Vec::new();
        for ast_instruction in &ast_block.instructions {
            let instr = match Self::ast_instruction_to_instruction(ast_instruction) {
                Ok(instr) => instr,
                Err(err) => {
                    self.add_error(err);
                    Instruction::invalid()
                }
            };
            instructions.push(instr);
        }

        Ok(Block {
            name: ast_block.name.text.to_string(),
            parameters,
            instructions,
        })
    }

    fn ast_stack_block_to_stack_slots(
        &mut self,
        ast_stack_block: &ast::Block,
    ) -> Result<Vec<StackSlot>, Error> {
        if !ast_stack_block.parameters.is_empty() {
            self.add_error(Error::new(
                "stack block shouldn't have parameters",
                &ast_stack_block.name,
            ))
        }

        let mut stack_slots = Vec::new();
        for (i, ast_instruction) in ast_stack_block.instructions.iter().enumerate() {
            let expected_id = i as u32;
            stack_slots.push(Self::ast_instruction_to_stack_slot(
                ast_instruction,
                expected_id,
            )?);
        }

        Ok(stack_slots)
    }

    fn ast_instruction_to_stack_slot(
        ast_instruction: &ast::Instruction,
        expected_id: u32,
    ) -> Result<StackSlot, Error> {
        let stack_slot_var = Self::ast_variable_to_variable(
            ast_instruction
                .destination
                .as_ref()
                .expect("stack slot must be named"),
        )?;
        assert_eq!(stack_slot_var.as_stack(), Some(expected_id));
        let kind = match ast_instruction.opcode.text {
            "local" => {
                let ast_var = ast_instruction.operands.first().ok_or_else(|| {
                    Error::new(
                        "local stack slot missing allocated_for variable",
                        &ast_instruction.opcode,
                    )
                })?;
                let allocated_for = Self::ast_variable_to_variable(ast_var)?
                    .to_non_stack()
                    .ok_or_else(|| {
                        Error::new(
                            "local stack slot can only be allocated for a non stack variable",
                            &ast_instruction.opcode,
                        )
                    })?;
                StackSlotKind::Local { allocated_for }
            }
            "caller" => StackSlotKind::Caller,
            kind_str => {
                return Err(Error::new(
                    format!("invalid stack slot kind `{}`", kind_str),
                    &ast_instruction.opcode,
                ));
            }
        };
        Ok(StackSlot { typ: Typ, kind })
    }

    fn ast_parameter_to_parameter(ast_parameter: &ast::Parameter) -> Result<Parameter, Error> {
        Ok(Parameter {
            variable: Self::ast_variable_to_variable(&ast_parameter.name)?,
            typ: Typ,
        })
    }

    fn ast_instruction_to_instruction(
        ast_instruction: &ast::Instruction,
    ) -> Result<Instruction, Error> {
        let (src, target_block) = if let Some(ast_target_block) = &ast_instruction.target_block {
            let src = SourceOperands {
                operands: ast_map(&ast_target_block.arguments, Self::ast_operand_to_operand)?,
            };
            (src, Some(ast_target_block.name.text.to_string()))
        } else {
            let src = SourceOperands {
                operands: ast_map(&ast_instruction.operands, Self::ast_operand_to_operand)?,
            };
            (src, None)
        };

        Ok(Instruction {
            opcode: Self::ast_opcode_to_opcode(&ast_instruction.opcode)?,
            src,
            dst: ast_map_option(&ast_instruction.destination, Self::ast_variable_to_variable)?,
            target_block,
            cond: ast_map_option(&ast_instruction.condition, Self::ast_condition_to_condition)?,
        })
    }

    fn ast_condition_to_condition(ast_condition: &ast::Condition) -> Result<Condition, Error> {
        let condition = match ast_condition {
            ast::Condition::Equals(a, b) => Condition::Equals(
                Self::ast_operand_to_operand(a)?,
                Self::ast_operand_to_operand(b)?,
            ),
            ast::Condition::NotEquals(a, b) => Condition::NotEquals(
                Self::ast_operand_to_operand(a)?,
                Self::ast_operand_to_operand(b)?,
            ),
        };
        Ok(condition)
    }

    fn ast_operand_to_operand(ast_operand: &ast::Span) -> Result<Operand, Error> {
        if ast_operand.text.chars().all(char::is_numeric) {
            let imm = ast_operand
                .text
                .parse()
                .map_err(|_| Error::new("failed to parse immediate value", ast_operand))?;
            Ok(Operand::Imm(imm))
        } else {
            let variable = Self::ast_variable_to_variable(ast_operand)?;
            Ok(Operand::Var(variable))
        }
    }

    fn ast_variable_to_variable(ast_variable: &ast::Span) -> Result<Variable, Error> {
        if let Some((_, id_str)) = ast_variable.text.split_once(&['v', 'r', 's']) {
            let id = id_str.parse::<u32>().map_err(|_| {
                Error::new(
                    format!(
                        "invalid variable id `{}`, expected integer",
                        ast_variable.text
                    ),
                    ast_variable,
                )
            })?;

            let variable = if ast_variable.text.starts_with("v") {
                Variable::Virtual(id)
            } else if ast_variable.text.starts_with("r") {
                Variable::Register(id)
            } else if ast_variable.text.starts_with("s") {
                Variable::Stack(id)
            } else {
                unreachable!("if this is actually reachable, rewrite this whole function plz");
            };

            Ok(variable)
        } else {
            Err(Error::new(
                format!(
                    "invalid variable kind `{}`, expected `v`, `r` or `s`",
                    ast_variable.text
                ),
                ast_variable,
            ))
        }
    }

    fn ast_opcode_to_opcode(ast_opcode: &ast::Span) -> Result<Opcode, Error> {
        Opcode::from_str(ast_opcode.text).ok_or(Error::new(
            format!("unknown opcode `{}`", ast_opcode.text),
            ast_opcode,
        ))
    }

    fn add_error(&mut self, err: Error) {
        self.errors.push(err);
    }
}

fn ast_map<T, U>(ast_items: &Vec<T>, f: fn(&T) -> Result<U, Error>) -> Result<Vec<U>, Error> {
    let mut items = Vec::new();
    for ast_item in ast_items {
        items.push(f(ast_item)?);
    }
    Ok(items)
}

fn ast_map_option<T, U>(
    ast_option: &Option<T>,
    f: fn(&T) -> Result<U, Error>,
) -> Result<Option<U>, Error> {
    if let Some(ast_item) = ast_option {
        Ok(Some(f(ast_item)?))
    } else {
        Ok(None)
    }
}
