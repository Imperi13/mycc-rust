use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("incorrect argument count");
    }

    let code = format!("{}\n", args[1]);

    mycc_rust::compile(&code, "module.ll");
}
