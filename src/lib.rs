mod ast;
mod codegen;
mod parse;
mod tokenize;
mod types;

use inkwell::context::Context;

use codegen::CodegenArena;
use parse::parse_all;
use tokenize::tokenize;

pub fn compile(code: &str, output_path: &str) {
    let mut tok_seq = tokenize(&code);

    tok_seq.remove_newline();
    eprintln!("TokenList without newline\n{:?}", tok_seq);

    let ast = parse_all(tok_seq).unwrap();
    eprintln!("AST\n{:?}", ast);

    let context = Context::create();
    let mut codegen_arena = CodegenArena::new(&context);
    codegen_arena.codegen_all(&ast, output_path);
}
