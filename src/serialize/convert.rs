use crate::{
    instruction::{Instruction, Opcode, SourceOperand, SourceOperands},
    module::{BasicBlock, BasicBlocks, Module, Parameter, Procedure, ProcedureData, Typ, Variable},
};

use super::ast;

pub mod error;
pub use error::Error;

// TODO actual errors & allow returning multiple errors
pub fn convert_ast(ast_module: &ast::Module) -> Result<Module, Error> {
    ast_module_to_module(ast_module)
}

pub fn ast_module_to_module(ast_module: &ast::Module) -> Result<Module, Error> {
    let mut procedures = Vec::new();

    for ast_proc in &ast_module.procedures {
        // FIXME handle entry block better

        let mut ast_blocks_iter = ast_proc.basic_blocks.iter();
        let ast_entry_block = ast_blocks_iter
            .next()
            .ok_or(Error::new("procedure has no blocks", &ast_proc.name))?;

        if !ast_entry_block.parameters.is_empty() {
            return Err(Error::new(
                "entry block shouldn't have parameters",
                &ast_entry_block.name,
            ));
        }

        let entry_block = BasicBlock {
            name: ast_entry_block.name.text.to_string(),
            parameters: ast_map(&ast_proc.parameters, ast_parameter_to_parameter)?,
            instructions: ast_map(
                &ast_entry_block.instructions,
                ast_instruction_to_instruction,
            )?,
        };

        let mut other_blocks = Vec::new();
        for ast_block in ast_blocks_iter {
            other_blocks.push(BasicBlock {
                name: ast_block.name.text.to_string(),
                parameters: ast_map(&ast_proc.parameters, ast_parameter_to_parameter)?,
                instructions: ast_map(
                    &ast_entry_block.instructions,
                    ast_instruction_to_instruction,
                )?,
            });
        }

        let proc = Procedure {
            return_typ: Typ,
            basic_blocks: BasicBlocks {
                entry: entry_block,
                others: other_blocks,
            },
            data: ProcedureData::default(),
        };
        procedures.push(proc);
    }

    Ok(Module { procedures })
}

fn ast_parameter_to_parameter(ast_parameter: &ast::Parameter) -> Result<Parameter, Error> {
    Ok(Parameter {
        variable: ast_variable_to_variable(&ast_parameter.name)?,
        typ: Typ,
    })
}

fn ast_instruction_to_instruction(
    ast_instruction: &ast::Instruction,
) -> Result<Instruction, Error> {
    Ok(Instruction {
        opcode: ast_opcode_to_opcode(&ast_instruction.opcode)?,
        src: SourceOperands {
            operands: ast_map(&ast_instruction.operands, ast_operand_to_operand)?,
        },
        dst: ast_map_option(&ast_instruction.destination, ast_variable_to_variable)?,
    })
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

fn ast_operand_to_operand(ast_operand: &ast::Span) -> Result<SourceOperand, Error> {
    // TODO support Imm operands
    let variable = ast_variable_to_variable(ast_operand)?;
    Ok(SourceOperand::Var(variable))
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
