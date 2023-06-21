use clap::Parser;
use std::path::PathBuf;

#[derive(Parser)]
struct CliArg {
    #[arg(short = 'o',default_value="main.ll")]
    output_path: PathBuf,

    input_path: PathBuf,
}

fn main() {
    let arg = CliArg::parse();

    mycc_rust::compile_to_llvm_ir(arg.input_path, arg.output_path);
}
