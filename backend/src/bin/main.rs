use std::process::ExitCode;

use compiler::{
    callconv::CallingConventionId,
    context::Context,
    error_reporting::{make_error_report, report_error},
    module::Module,
    serialize, x86_64,
};

fn main() -> ExitCode {
    let source_name = "data/example.ayir";
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

    if errors.is_empty() {
        handle_module(convert_result.module);

        ExitCode::SUCCESS
    } else {
        for e in errors {
            report_error(e);
        }

        ExitCode::FAILURE
    }
}

fn handle_module(mut module: Module) {
    let mut context = Context::new(x86_64::make_isa());
    for proc in module.procedures.iter_mut() {
        proc.signature.calling_convention = Some(CallingConventionId::SysV);
        context.declarations.declare_procedure(&proc);
    }
    for exproc in module.external_procedures.iter_mut() {
        exproc.calling_convention = CallingConventionId::SysV;
        context.declarations.declare_external_procedure(&exproc);
    }

    for proc in module.procedures.iter_mut() {
        x86_64::regalloc::allocate_registers(proc, &context);
    }

    // let s = serialize::convert::convert_module_to_string(&module);
    let s = x86_64::assembly::generate_assembly(&module, &context);

    println!("{}", s);

    //dbg!(module);
}
