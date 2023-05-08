use std::process::ExitCode;

use compiler_backend::{
    error_reporting::{make_convert_error_report, make_parse_error_report, report_error},
    module::Module,
    serialize::{self, convert::convert_ast},
};

fn main() -> ExitCode {
    let source_name = "data/errors.ayir";
    let source = std::fs::read_to_string(source_name).unwrap();
    // let result = parser::lexer::lex(&source).collect::<Vec<_>>();
    let result = serialize::Parser::new(&source).parse();

    let mut error_occured = false;

    for parse_error in result.errors {
        let e = make_parse_error_report(parse_error, source_name, &source);
        report_error(e);
        error_occured = true;
    }

    match convert_ast(&result.module) {
        Err(convert_error) => {
            let e = make_convert_error_report(convert_error, source_name, &source);
            report_error(e);
            error_occured = true;
        }
        Ok(module) => handle_module(module),
    }

    if error_occured {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn handle_module(module: Module) {
    dbg!(module);
}
