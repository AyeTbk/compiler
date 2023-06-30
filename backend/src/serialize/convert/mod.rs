use crate::module::Module;

use super::ast;

pub mod error;
pub use error::Error;

mod ast_to_module;
use self::ast_to_module::ConverterAstToModule;

mod module_serializer;
use self::module_serializer::ModuleSerializer;

pub fn convert_ast_to_module(ast_module: &ast::Module) -> ConvertAstToModuleResult {
    ConverterAstToModule::new().parse(ast_module)
}

#[derive(Debug)]
pub struct ConvertAstToModuleResult {
    pub module: Module,
    pub errors: Vec<Error>,
}

pub fn convert_module_to_string(module: &Module) -> String {
    let mut buf = String::new();
    ModuleSerializer::new(&mut buf).serialize(module);
    buf
}
