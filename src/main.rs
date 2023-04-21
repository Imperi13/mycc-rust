use inkwell::context::Context;
use std::env;
use std::path::Path;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("incorrect argument count");
    }

    let ret_num: u64 = args[1].parse().unwrap();

    let context = Context::create();
    let module = context.create_module("main");
    let builder = context.create_builder();

    let i32_type = context.i32_type();
    let main_fn_type = i32_type.fn_type(&[], false);
    let main_fn = module.add_function("main", main_fn_type, None);
    let basic_block = context.append_basic_block(main_fn, "entry");

    builder.position_at_end(basic_block);
    builder.build_return(Some(&i32_type.const_int(ret_num, false)));

    let path = Path::new("module.bc");
    module.write_bitcode_to_path(path);

    println!("{}", ret_num);
}
