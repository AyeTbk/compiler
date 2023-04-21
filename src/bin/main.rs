use compiler_backend::parser;

fn main() {
    let src = std::fs::read_to_string("data/errors.ayir").unwrap();
    // let result = parser::lexer::lex(&src).collect::<Vec<_>>();
    let result = parser::Parser::new(&src).parse();
    dbg!(result);
}
