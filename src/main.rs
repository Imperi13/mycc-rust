use clap::Parser;
use std::path::PathBuf;

#[derive(Parser)]
struct CliArg {
    #[clap(short = 'o')]
    output_path: Option<PathBuf>,

    input_path: PathBuf,
}

fn main() {
    let arg = CliArg::parse();

    mycc_rust::compile_to_llvm_ir(arg.input_path, "module.ll");
}
