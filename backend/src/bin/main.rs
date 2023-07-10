use std::process::ExitCode;

use compiler_backend::{
    calling_convention::CallingConvention,
    error_reporting::{make_error_report, report_error},
    module::Module,
    passes::stack::{
        allstack_allocate_parameters_and_return, allstack_setup_callees,
        fix_memory_to_memory_loads_stores, generate_loads_stores, spill_all_virtual,
    },
    serialize, x86_64,
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
    for proc in module.procedures.iter_mut() {
        proc.signature.calling_convention = Some(CallingConvention::AllStack);
        module.declarations.declare_procedure(&proc.signature);
    }

    for proc in module.procedures.iter_mut() {
        allstack_allocate_parameters_and_return(proc);
        allstack_setup_callees(&module.declarations, proc);
        spill_all_virtual(proc);
        generate_loads_stores(proc);
        fix_memory_to_memory_loads_stores(proc);
        x86_64::regalloc::allocate_registers(proc);
    }

    // let s = serialize::convert::convert_module_to_string(&module);
    let s = x86_64::assembly::generate_assembly(&module);

    println!("{}", s);

    //dbg!(module);
}
