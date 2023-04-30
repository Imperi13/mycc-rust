use codegen::codegen;
use parse::parse_all;
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

    let tmp = parse_all(tok_seq);
}

pub mod tokenize {
    use std::cell::RefCell;
    use std::fmt;
    use std::rc::Rc;

    #[derive(Debug, Clone)]
    pub enum PunctKind {
        PunctPlus,
        PunctMinus,
    }

    #[derive(Debug, Clone)]
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

    #[derive(Clone)]
    pub struct TokenList {
        head: Rc<RefCell<Link>>,
    }

    impl TokenList {
        pub fn is_empty(&self) -> bool {
            matches!(*self.head.borrow(), Link::End)
        }

        pub fn next(&self) -> TokenList {
            if let Link::More(ref node) = *self.head.clone().borrow() {
                TokenList {
                    head: node.next.clone(),
                }
            } else {
                panic!("empty List")
            }
        }

        pub fn get_token(&self) -> TokenKind {
            if let Link::More(ref node) = *self.head.clone().borrow() {
                node.elem.clone()
            } else {
                panic!("empty List")
            }
        }
    }

    impl fmt::Debug for TokenList {
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

    pub fn tokenize(mut code: &str) -> TokenList {
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

        TokenList { head: tok_seq }
    }
}

mod parse {
    use super::tokenize::PunctKind;
    use super::tokenize::TokenKind;
    use super::tokenize::TokenList;
    use std::cell::RefCell;
    use std::rc::Rc;

    pub enum BinaryOpKind {
        BinaryOpPlus,
        BinaryOpMinus,
    }

    pub struct BinaryOpNode {
        lhs: Rc<RefCell<ASTNode>>,
        rhs: Rc<RefCell<ASTNode>>,
        kind: BinaryOpKind,
    }

    pub enum ASTNode {
        ASTBinaryOp(BinaryOpNode),
        ASTNumber(i64),
    }

    pub struct AST {
        head: Rc<RefCell<ASTNode>>,
    }

    pub fn parse_all(tok_seq: TokenList) -> AST {
        let node;
        (_, node) = parse_add(tok_seq);

        AST { head: node }
    }

    fn parse_add(mut tok_seq: TokenList) -> (TokenList, Rc<RefCell<ASTNode>>) {
        let mut lhs;
        (tok_seq, lhs) = parse_primary(tok_seq);

        while let TokenKind::TokenPunct(punct) = tok_seq.get_token() {
            let kind = match punct {
                PunctKind::PunctPlus => BinaryOpKind::BinaryOpPlus,
                PunctKind::PunctMinus => BinaryOpKind::BinaryOpMinus,
            };

            tok_seq = tok_seq.next();

            let rhs;
            (tok_seq, rhs) = parse_primary(tok_seq);

            lhs = Rc::new(RefCell::new(ASTNode::ASTBinaryOp(BinaryOpNode {
                lhs,
                rhs,
                kind,
            })));
        }

        (tok_seq, lhs)
    }

    fn parse_primary(tok_seq: TokenList) -> (TokenList, Rc<RefCell<ASTNode>>) {
        if let TokenKind::TokenNumber(num) = tok_seq.get_token() {
            (
                tok_seq.next(),
                Rc::new(RefCell::new(ASTNode::ASTNumber(num))),
            )
        } else {
            unreachable!()
        }
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
