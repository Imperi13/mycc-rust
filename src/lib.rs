mod ast;
mod cfg;
mod codegen;
mod error;
mod obj;
mod parse;
mod tokenize;
mod types;

use codegen::CodegenArena;
use obj::ObjArena;
use parse::parse_all;
use tokenize::tokenize;

use cfg::gen_cfg_all;

use inkwell::context::Context;
use std::fs::read_to_string;
use std::path::Path;
use std::process;

pub fn compile_to_llvm_ir<Pinput: AsRef<Path>, Poutput: AsRef<Path>>(
    input_path: Pinput,
    output_path: Poutput,
) {
    let file_read = read_to_string(input_path);

    let code = match file_read {
        Ok(content) => format!("{content}\n"),
        Err(e) => {
            eprintln!("{}", e);
            process::exit(1);
        }
    };

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

    let cfg = gen_cfg_all(&mut obj_arena, &ast);

    for c in cfg.iter() {
        eprintln!("{:?}", c);
    }

    let context = Context::create();
    let mut codegen_arena = CodegenArena::new(&context);
    codegen_arena.codegen_all(&cfg, output_path);
}
