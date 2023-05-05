use crate::{
    instruction::{Instruction, Opcode},
    module::{BasicBlock, BasicBlocks, Module, Parameter, Procedure, ProcedureData, Typ, Variable},
};

use super::ast;

pub mod error;
pub use error::Error;

// TODO potentially allow returning multiple errors
pub fn ast_to_module(ast_module: &ast::Module) -> Result<Module, Error> {
    let mut procedures = Vec::new();

    for ast_proc in &ast_module.procedures {
        let mut ast_blocks_iter = ast_proc.basic_blocks.iter();
        let ast_entry_block = ast_blocks_iter.next().ok_or(Error {})?;

        if !ast_entry_block.parameters.is_empty() {
            return Err(Error {});
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
        src: todo!(),
        dst: todo!(),
    })
}

fn ast_variable_to_variable(ast_variable: &ast::Span) -> Result<Variable, Error> {
    if let Some((kind_str, id_str)) = ast_variable.text.split_once(&['v', 'r', 's']) {
        let id = id_str.parse::<u32>().map_err(|_| Error {})?;
        let variable = match kind_str {
            "v" => Variable::Virtual(id),
            "r" => Variable::Register(id),
            "s" => Variable::Stack(id),
            _ => return Err(Error {}),
        };

        Ok(variable)
    } else {
        Err(Error {})
    }
}

fn ast_opcode_to_opcode(ast_opcode: &ast::Span) -> Result<Opcode, Error> {
    todo!()
}

fn ast_map<T, U>(ast_items: &Vec<T>, f: fn(&T) -> Result<U, Error>) -> Result<Vec<U>, Error> {
    let mut items = Vec::new();
    for ast_item in ast_items {
        items.push(f(ast_item)?);
    }
    Ok(items)
}
