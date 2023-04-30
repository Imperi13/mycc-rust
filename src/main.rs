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
    println!("{:?}", tok_seq);
}

mod tokenize {
    use std::cell::RefCell;
    use std::fmt;
    use std::rc::Rc;

    #[derive(Debug)]
    pub enum PunctKind {
        PunctPlus,
        PunctMinus,
    }

    #[derive(Debug)]
    pub enum TokenKind {
        TokenNumber(i64),
        TokenPunct(PunctKind),
        TokenNewline,
    }

    struct Node {
        elem: TokenKind,
        next: Rc<RefCell<Link>>,
    }

    enum Link {
        End,
        More(Node),
    }

    pub struct List {
        head: Rc<RefCell<Link>>,
    }

    impl fmt::Debug for List {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let mut cur = self.head.clone();
            let mut index: usize = 0;

            while let Link::More(ref node) = *cur.clone().borrow() {
                write!(f, "{}th node: {:?}\n", index, node.elem)?;
                index += 1;
                cur = node.next.clone();
            }

            write!(f, "")
        }
    }

    fn tokenize_num(code: &str) -> (i64, &str) {
        let mut index: usize = 0;

        loop {
            let (_, ch) = code.char_indices().nth(index).unwrap();
            if ch.is_digit(10) {
                index += 1;
            } else {
                break;
            }
        }

        let (byte_index, _) = code.char_indices().nth(index).unwrap();
        (
            i64::from_str_radix(&code[..byte_index], 10).unwrap(),
            &code[byte_index..],
        )
    }

    pub fn tokenize(mut code: &str) -> List {
        let tok_seq = Rc::new(RefCell::new(Link::End));
        let mut cur = tok_seq.clone();

        while !code.is_empty() {
            let (_, ch) = code.char_indices().nth(0).unwrap();
            if ch.is_digit(10) {
                let num: i64;
                (num, code) = tokenize_num(code);
                let new_tok = Rc::new(RefCell::new(Link::End));
                *cur.borrow_mut() = Link::More(Node {
                    elem: TokenKind::TokenNumber(num),
                    next: new_tok.clone(),
                });

                cur = new_tok;
                continue;
            }

            if code.chars().nth(0) == Some('+') {
                code = &code[1..];
                let new_tok = Rc::new(RefCell::new(Link::End));
                *cur.borrow_mut() = Link::More(Node {
                    elem: TokenKind::TokenPunct(PunctKind::PunctPlus),
                    next: new_tok.clone(),
                });

                cur = new_tok;
                continue;
            }

            if code.chars().nth(0) == Some('-') {
                code = &code[1..];
                let new_tok = Rc::new(RefCell::new(Link::End));
                *cur.borrow_mut() = Link::More(Node {
                    elem: TokenKind::TokenPunct(PunctKind::PunctMinus),
                    next: new_tok.clone(),
                });

                cur = new_tok;
                continue;
            }

            if code.chars().nth(0) == Some('\n') {
                code = &code[1..];
                let new_tok = Rc::new(RefCell::new(Link::End));
                *cur.borrow_mut() = Link::More(Node {
                    elem: TokenKind::TokenNewline,
                    next: new_tok.clone(),
                });

                cur = new_tok;
                continue;
            }

            unreachable!();
        }

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
