use clap::Parser;
use std::path::PathBuf;

#[derive(Parser)]
struct CliArg {
    #[arg(short = 'o', default_value = "main.ll")]
    output_path: PathBuf,

    #[arg(short = 'O', default_value = "0")]
    llvm_optimize_level: u8,

    #[arg(long = "mO", default_value = "0")]
    mycc_optimize_level: u8,

    input_path: PathBuf,
}

fn main() {
    let arg = CliArg::parse();

    mycc_rust::compile_to_llvm_ir(
        arg.input_path,
        arg.output_path,
        arg.mycc_optimize_level,
        arg.llvm_optimize_level,
    );
}
