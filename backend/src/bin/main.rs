use std::process::ExitCode;

use compiler_backend::{
    error_reporting::{make_error_report, report_error},
    module::Module,
    passes::stack::{generate_loads_stores, spill_all_virtual, stack_call_conv},
    serialize::{self, convert::convert_module_to_string},
};

fn main() -> ExitCode {
    let source_name = "data/errors.ayir";
    let source = std::fs::read_to_string(source_name).unwrap();

    let mut errors = Vec::new();

    let parse_result = serialize::parse::parse(&source);
    for parse_error in parse_result.errors {
        let e = make_error_report(parse_error, source_name, &source);
        errors.push(e);
    }

    let convert_result = serialize::convert::convert_ast_to_module(&parse_result.module);
    for convert_error in convert_result.errors {
        let e = make_error_report(convert_error, source_name, &source);
        errors.push(e);
    }

    let module = convert_result.module;

    if errors.is_empty() {
        handle_module(module);

        ExitCode::SUCCESS
    } else {
        for e in errors {
            report_error(e);
        }

        ExitCode::FAILURE
    }
}

fn handle_module(mut module: Module) {
    for proc in &mut module.procedures {
        stack_call_conv(proc);
        spill_all_virtual(proc);
        generate_loads_stores(proc);
    }
    let s = convert_module_to_string(&module);
    println!("{}", s);

    //dbg!(module);
}
