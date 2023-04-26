use inkwell::context::Context;
use std::env;
use std::path::Path;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("incorrect argument count");
    }

    codegen(&args[1]);
}

fn codegen(code: &str) {
    let ret_num: u64 = code.parse().unwrap();

    let context = Context::create();
    let module = context.create_module("main");
    let builder = context.create_builder();

    let i32_type = context.i32_type();
    let main_fn_type = i32_type.fn_type(&[], false);
    let main_fn = module.add_function("main", main_fn_type, None);
    let basic_block = context.append_basic_block(main_fn, "entry");

    builder.position_at_end(basic_block);
    builder.build_return(Some(&i32_type.const_int(ret_num, false)));

    let path = Path::new("module.ll");
    module.print_to_file(path).unwrap();
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::process::Command;

    #[test]
    fn codegen_test_1() {
        codegen("12");
    }

    #[test]
    fn codegen_test_2() {
        codegen("12");
        let status = Command::new("lli-12")
            .arg("module.ll")
            .status()
            .expect("failed to execute lli");

        match status.code() {
            Some(code) => assert_eq!(12, code),
            None => panic!("process terminated by signal"),
        }
    }
}
