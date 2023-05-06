mod codegen;
mod parse;
mod tokenize;

use std::env;

use codegen::codegen_all;
use parse::parse_all;
use tokenize::tokenize;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("incorrect argument count");
    }

    let code = format!("{}\n", args[1]);

    let mut tok_seq = tokenize(&code);
    tok_seq.remove_newline();

    let ast = parse_all(tok_seq);

    codegen_all(ast);
}
