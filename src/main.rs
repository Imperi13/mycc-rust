use codegen::codegen_all;
use parse::parse_all;
use std::env;
use tokenize::tokenize;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("incorrect argument count");
    }

    let code = format!("{}\n", args[1]);

    let mut tok_seq = tokenize(&code);
    println!("{:?}", tok_seq);

    tok_seq.remove_newline();
    println!("{:?}", tok_seq);

    let ast = parse_all(tok_seq);

    codegen_all(ast);
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

        pub fn remove_newline(&mut self) {
            // banpei
            let new_self = Rc::new(RefCell::new(Link::More(Node {
                elem: TokenKind::TokenNewline,
                next: self.head.clone(),
            })));
            let mut new_cur = new_self.clone();
            let mut cur = self.head.clone();

            while let Link::More(ref node) = *cur.clone().borrow_mut() {
                if let TokenKind::TokenNewline = node.elem {
                    cur = node.next.clone();
                } else {
                    if let Link::More(ref mut new_node) = *new_cur.clone().borrow_mut() {
                        new_node.next = cur.clone();
                        new_cur = cur.clone();
                        cur = node.next.clone();
                    } else {
                        unreachable!()
                    }
                }
            }

            if let Link::More(ref mut new_node) = *new_cur.clone().borrow_mut() {
                new_node.next = Rc::new(RefCell::new(Link::End));
            } else {
                unreachable!()
            }

            if let Link::More(ref new_self_node) = *new_self.clone().borrow() {
                self.head = new_self_node.next.clone();
            } else {
                unreachable!()
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

    #[derive(Debug, Clone)]
    pub struct ParseError;

    pub enum BinaryOpKind {
        BinaryOpPlus,
        BinaryOpMinus,
    }

    pub struct BinaryOpNode {
        pub lhs: AST,
        pub rhs: AST,
        pub kind: BinaryOpKind,
    }

    pub enum ASTNode {
        ASTBinaryOp(BinaryOpNode),
        ASTNumber(i64),
    }

    #[derive(Clone)]
    pub struct AST {
        pub head: Rc<RefCell<ASTNode>>,
    }

    pub fn parse_all(mut tok_seq: TokenList) -> AST {
        let node;
        (tok_seq, node) = parse_add(tok_seq).unwrap();

        if !tok_seq.is_empty() {
            panic!("parse error: cannot fully parse to the end")
        }

        AST { head: node }
    }

    fn parse_add(mut tok_seq: TokenList) -> Result<(TokenList, Rc<RefCell<ASTNode>>), ParseError> {
        let mut lhs;
        (tok_seq, lhs) = parse_primary(tok_seq)?;

        while !tok_seq.is_empty() {
            if let TokenKind::TokenPunct(punct) = tok_seq.get_token() {
                let kind = match punct {
                    PunctKind::PunctPlus => BinaryOpKind::BinaryOpPlus,
                    PunctKind::PunctMinus => BinaryOpKind::BinaryOpMinus,
                };

                tok_seq = tok_seq.next();

                let rhs;
                (tok_seq, rhs) = parse_primary(tok_seq)?;

                lhs = Rc::new(RefCell::new(ASTNode::ASTBinaryOp(BinaryOpNode {
                    lhs: AST { head: lhs },
                    rhs: AST { head: rhs },
                    kind,
                })));
            } else {
                break;
            }
        }

        Ok((tok_seq, lhs))
    }

    fn parse_primary(tok_seq: TokenList) -> Result<(TokenList, Rc<RefCell<ASTNode>>), ParseError> {
        if tok_seq.is_empty() {
            return Err(ParseError {});
        }

        if let TokenKind::TokenNumber(num) = tok_seq.get_token() {
            Ok((
                tok_seq.next(),
                Rc::new(RefCell::new(ASTNode::ASTNumber(num))),
            ))
        } else {
            Err(ParseError {})
        }
    }
}

mod codegen {
    use super::parse::ASTNode;
    use super::parse::BinaryOpKind;
    use super::parse::AST;
    use inkwell::builder::Builder;
    use inkwell::context::Context;
    use inkwell::module::Module;
    use inkwell::values::IntValue;
    use std::path::Path;

    struct CodegenArena<'a> {
        context: &'a Context,
        module: Module<'a>,
        builder: Builder<'a>,
    }

    impl CodegenArena<'_> {
        pub fn print_to_file(&self, filepath: &str) {
            let path = Path::new(filepath);
            self.module.print_to_file(path).unwrap();
        }

        pub fn codegen_ret(&self, ast: AST) {
            let i64_type = self.context.i64_type();
            let main_fn_type = i64_type.fn_type(&[], false);
            let main_fn = self.module.add_function("main", main_fn_type, None);
            let basic_block = self.context.append_basic_block(main_fn, "entry");

            let val = self.codegen_add(ast);

            self.builder.position_at_end(basic_block);
            self.builder.build_return(Some(&val));
        }

        pub fn codegen_add(&self, ast: AST) -> IntValue {
            match *ast.head.borrow() {
                ASTNode::ASTNumber(_) => self.codegen_primary(ast.clone()),
                ASTNode::ASTBinaryOp(ref binary_node) => match binary_node.kind {
                    BinaryOpKind::BinaryOpPlus => {
                        let lhs = self.codegen_primary(binary_node.lhs.clone());
                        let rhs = self.codegen_primary(binary_node.rhs.clone());
                        self.builder.build_int_add(lhs, rhs, "add node")
                    }
                    BinaryOpKind::BinaryOpMinus => {
                        let lhs = self.codegen_primary(binary_node.lhs.clone());
                        let rhs = self.codegen_primary(binary_node.rhs.clone());
                        self.builder.build_int_sub(lhs, rhs, "add node")
                    }
                },
            }
        }

        pub fn codegen_primary(&self, ast: AST) -> IntValue {
            let i64_type = self.context.i64_type();

            if let ASTNode::ASTNumber(num) = *ast.head.borrow() {
                i64_type.const_int(num as u64, false)
            } else {
                panic!("not primary node")
            }
        }
    }

    pub fn codegen_all(ast: AST) {
        let context = Context::create();
        let module = context.create_module("main");
        let builder = context.create_builder();

        let arena = CodegenArena {
            context: &context,
            module,
            builder,
        };

        arena.codegen_ret(ast);

        arena.print_to_file("module.ll");
    }
}
