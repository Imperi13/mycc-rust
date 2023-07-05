mod ast;
mod cfg;
mod codegen;
mod const_value;
mod error;
mod obj;
mod parse;
mod tokenize;
mod types;

use codegen::CodegenArena;
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
    mycc_optimize_level: u8,
    llvm_optimize_level: u8,
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

    let ast = parse_all(tok_seq);

    let ast = match ast {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };

    let mut cfg = gen_cfg_all(&ast);

    if mycc_optimize_level == 1 {
        cfg.eval_constant_propagation();
    }

    eprintln!("{:?}", cfg);

    let context = Context::create();
    let mut codegen_arena = CodegenArena::new(&context);
    codegen_arena.codegen_all(&cfg, output_path, llvm_optimize_level);
}
