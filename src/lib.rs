mod codegen;
mod parse;
mod tokenize;

use codegen::codegen_all;
use parse::parse_all;
use tokenize::tokenize;

pub fn compile(code: &str, output_path: &str) {
    let mut tok_seq = tokenize(&code);
    tok_seq.remove_newline();

    let ast = parse_all(tok_seq);

    codegen_all(ast, output_path);
}
