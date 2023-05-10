use std::process::ExitCode;

use compiler_backend::{
    error_reporting::{make_error_report, report_error},
    module::Module,
    serialize::{self, convert::convert_ast_to_module},
};

fn main() -> ExitCode {
    let source_name = "data/errors.ayir";
    let source = std::fs::read_to_string(source_name).unwrap();
    // let result = parser::lexer::lex(&source).collect::<Vec<_>>();
    let parse_result = serialize::parse::parse(&source);

    let mut errors = Vec::new();

    for parse_error in parse_result.errors {
        let e = make_error_report(parse_error, source_name, &source);
        errors.push(e);
    }

    match convert_ast_to_module(&parse_result.module) {
        Err(convert_errors) => {
            for convert_error in convert_errors {
                let e = make_error_report(convert_error, source_name, &source);
                errors.push(e);
            }
        }
        Ok(module) => {
            if errors.is_empty() {
                handle_module(module);
            }
        }
    }

    if errors.is_empty() {
        ExitCode::SUCCESS
    } else {
        for e in errors {
            report_error(e);
        }

        ExitCode::FAILURE
    }
}

fn handle_module(module: Module) {
    dbg!(module);
}
