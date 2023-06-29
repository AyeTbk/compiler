use crate::module::Module;

use self::ast_to_module::ConverterAstToModule;

use super::ast;

pub mod error;
pub use error::Error;

mod ast_to_module;

// TODO actual errors & allow returning multiple errors
pub fn convert_ast_to_module(ast_module: &ast::Module) -> ConvertAstToModuleResult {
    ConverterAstToModule::new().parse(ast_module)
}

#[derive(Debug)]
pub struct ConvertAstToModuleResult {
    pub module: Module,
    pub errors: Vec<Error>,
}
