use std::fs;
use std::process::Command;

pub fn compile(code_str: &str, path: &str) {
    let code = format!("{}\n", code_str);
    mycc_rust::compile(&code, path);
}

pub fn exec_code(path: &str) -> i32 {
    let status = Command::new("lli-12")
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

