use crate::module::Module;

use super::ast;

pub mod error;
pub use error::Error;

mod ast_to_module;
use self::ast_to_module::ast_module_to_module;

// TODO actual errors & allow returning multiple errors
pub fn convert_ast_to_module(ast_module: &ast::Module) -> Result<Module, Vec<Error>> {
    ast_module_to_module(ast_module).map_err(|e| vec![e])
}
