use std::fs;
use std::io::Write;
use std::process::Command;
use tempfile::NamedTempFile;

pub fn compile_code(code_str: &str, path: &str) {
    let mut tmpfile = NamedTempFile::new().unwrap();
    write!(tmpfile, "{}", code_str).unwrap();

    mycc_rust::compile_to_llvm_ir(tmpfile.path(), path);
}

pub fn exec_code(path: &str) -> i32 {
    let status = Command::new("lli-15")
        .arg(path)
        .status()
        .expect("failed to execute lli");

    match status.code() {
        Some(code) => code,
        None => panic!("process terminated by signal"),
    }
}

pub fn cleanup(path: &str) -> std::io::Result<()> {
    fs::remove_file(path)
}
