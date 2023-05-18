mod codegen;
mod parse;
mod tokenize;
mod ast;
mod types;

use codegen::codegen_all;
use parse::parse_all;
use tokenize::tokenize;

pub fn compile(code: &str, output_path: &str) {
    let mut tok_seq = tokenize(&code);
    eprintln!("TokenList\n{:?}", tok_seq);

    tok_seq.remove_newline();
    eprintln!("TokenList without newline\n{:?}", tok_seq);

    let ast = parse_all(tok_seq);
    eprintln!("AST\n{:?}", ast);

    codegen_all(ast, output_path);
}
