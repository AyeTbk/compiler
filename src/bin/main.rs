use compiler_backend::{
    error_reporting::{convert_parser_error, report_error},
    serialize::{self, convert::ast_to_module},
};

fn main() {
    let source_name = "data/errors.ayir";
    let source = std::fs::read_to_string(source_name).unwrap();
    // let result = parser::lexer::lex(&source).collect::<Vec<_>>();
    let result = serialize::Parser::new(&source).parse();

    for parse_error in result.errors {
        let e = convert_parser_error(parse_error, source_name, &source);
        report_error(e);
    }

    let module = ast_to_module(&result.module).unwrap();

    dbg!(module);
}
