use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum PunctKind {
    PunctPlus,
    PunctDash,
    PunctAsterisk,
    PunctSlash,
    PunctOpenParenthesis,
    PunctCloseParenthesis,
    PunctEqual,
    PunctNotEqual,
    PunctSmaller,
    PunctSmallerEqual,
    PunctGreater,
    PunctGreaterEqual,
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

    pub fn expect_punct(&self, expect_punct: PunctKind) -> Option<TokenList> {
        if let Link::More(ref node) = *self.head.clone().borrow() {
            match &node.elem {
                TokenKind::TokenPunct(punct) => {
                    if &expect_punct == punct {
                        Some(self.next())
                    } else {
                        None
                    }
                }
                _ => None,
            }
        } else {
            None
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

fn tokenize_punct(code: &str) -> Option<(PunctKind, &str)> {
    if code.len() >= 2 {
        match &code[..2] {
            "==" => return Some((PunctKind::PunctEqual, &code[2..])),
            "!=" => return Some((PunctKind::PunctNotEqual, &code[2..])),
            "<=" => return Some((PunctKind::PunctSmallerEqual, &code[2..])),
            ">=" => return Some((PunctKind::PunctGreaterEqual, &code[2..])),
            _ => (),
        }
    }

    assert!(!code.is_empty());

    match &code[..1] {
        "+" => Some((PunctKind::PunctPlus, &code[1..])),
        "-" => Some((PunctKind::PunctDash, &code[1..])),
        "*" => Some((PunctKind::PunctAsterisk, &code[1..])),
        "/" => Some((PunctKind::PunctSlash, &code[1..])),
        "<" => Some((PunctKind::PunctSmaller, &code[1..])),
        ">" => Some((PunctKind::PunctGreater, &code[1..])),
        "(" => Some((PunctKind::PunctOpenParenthesis, &code[1..])),
        ")" => Some((PunctKind::PunctCloseParenthesis, &code[1..])),
        _ => None,
    }
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

        if tokenize_punct(code).is_some() {
            let punct;
            (punct, code) = tokenize_punct(code).unwrap();

            let new_tok = Rc::new(RefCell::new(Link::End));
            *cur.borrow_mut() = Link::More(Node {
                elem: TokenKind::TokenPunct(punct),
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
