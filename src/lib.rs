mod ast;
mod cfg;
mod codegen;
mod error;
mod obj;
mod parse;
mod tokenize;
mod types;

use inkwell::context::Context;

use codegen::CodegenArena;
use obj::ObjArena;
use parse::parse_all;
use tokenize::tokenize;

use cfg::gen_cfg_all;

pub fn compile(code: &str, output_path: &str) {
    let mut tok_seq = tokenize(&code);

    tok_seq.remove_newline();

    let mut obj_arena = ObjArena::new();

    let ast = parse_all(&mut obj_arena, tok_seq);

    let ast = match ast {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };

    let cfg = gen_cfg_all(&mut obj_arena,&ast);
    eprintln!("{:?}", cfg);

    let context = Context::create();
    let mut codegen_arena = CodegenArena::new(&context);
    codegen_arena.codegen_all(&ast, output_path);
}
