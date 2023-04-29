use codegen::codegen;
use std::env;
use tokenize::tokenize;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("incorrect argument count");
    }

    let code = format!("{}\n", args[1]);

    let tok_seq = tokenize(&code);
}

mod tokenize {
    use std::cell::RefCell;
    use std::rc::Rc;

    pub enum TokenKind {
        TokenNumber(i64),
        TokenNewline,
    }

    struct Node {
        elem: i32,
        next: Link,
    }

    enum Link {
        End,
        More(Rc<RefCell<Node>>),
    }

    pub struct List {
        head: Rc<RefCell<Link>>,
    }

    fn tokenize_num(code: &str) -> (i64, &str) {
        let num: i64 = 0;
        let mut index: usize = 0;

        loop {
            let (_, ch) = code.char_indices().nth(index).unwrap();
            if ch.is_digit(10) {
                index += 1;
            } else {
                break;
            }
        }

        let (byte_index,_) = code.char_indices().nth(index).unwrap(); 
        (i64::from_str_radix(&code[..byte_index],10).unwrap(),&code[byte_index..])
    }

    pub fn tokenize(mut code: &str) -> List {
        let tok_seq = Rc::new(RefCell::new(Link::End));
        let cur = tok_seq.clone();

        List { head: tok_seq }
    }
}

mod codegen {
    use inkwell::context::Context;
    use std::path::Path;

    pub fn codegen(code: &str) {
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
}
