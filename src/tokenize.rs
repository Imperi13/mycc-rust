use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

const VARIABLE_LETTERS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum PunctKind {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Comma,
    Ampersand,
    OpenBrace,
    CloseBrace,
    OpenParenthesis,
    CloseParenthesis,
    OpenSquareBracket,
    CloseSquareBracket,
    SemiColon,
    Assign,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum KeywordKind {
    Return,
    Int,
    Char,
    If,
    Else,
    While,
    For,
    Sizeof,
    Alignof,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    Number(u64),
    Punct(PunctKind),
    Keyword(KeywordKind),
    Ident(String),
    StrLiteral(String),
    Newline,
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
                TokenKind::Punct(punct) => {
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

    pub fn expect_keyword(&self, expect_keyword: KeywordKind) -> Option<TokenList> {
        if let Link::More(ref node) = *self.head.clone().borrow() {
            match &node.elem {
                TokenKind::Keyword(keyword) => {
                    if &expect_keyword == keyword {
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
            elem: TokenKind::Newline,
            next: self.head.clone(),
        })));
        let mut new_cur = new_self.clone();
        let mut cur = self.head.clone();

        while let Link::More(ref node) = *cur.clone().borrow_mut() {
            if let TokenKind::Newline = node.elem {
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

fn parse_base(code: &str) -> (u32, &str) {
    let (i1, c1) = code.char_indices().nth(0).unwrap();
    let (i2, c2) = code.char_indices().nth(1).unwrap();

    assert!(c1.is_digit(10));

    if c1 != '0' {
        (10, code)
    } else {
        if c2 == 'x' {
            let i3 = code.char_indices().nth(2).unwrap().0;
            (16, &code[i3..])
        } else if c2 == 'b' {
            let i3 = code.char_indices().nth(2).unwrap().0;
            (2, &code[i3..])
        } else if c2.is_digit(10) {
            (8, &code[i2..])
        } else {
            (8, &code[i1..])
        }
    }
}

fn tokenize_num(code: &str) -> (u64, &str) {
    let (base, code) = parse_base(code);
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
        u64::from_str_radix(&code[..byte_index], base).unwrap(),
        &code[byte_index..],
    )
}

fn tokenize_str_literal(code: &str) -> (String, &str) {
    let mut index: usize = 1;
    loop {
        let (_, ch) = code.char_indices().nth(index).unwrap();
        if ch == '"' {
            break;
        } else {
            index += 1;
        }
    }

    let (str_index, _) = code.char_indices().nth(index).unwrap();
    let (byte_index, _) = code.char_indices().nth(index + 1).unwrap();

    (String::from(&code[1..str_index]), &code[byte_index..])
}

fn tokenize_ident(code: &str) -> (String, &str) {
    let mut index: usize = 0;

    loop {
        let (_, ch) = code.char_indices().nth(index).unwrap();
        if VARIABLE_LETTERS.contains(ch) {
            index += 1;
        } else {
            break;
        }
    }

    let (byte_index, _) = code.char_indices().nth(index).unwrap();
    (String::from(&code[..byte_index]), &code[byte_index..])
}

fn tokenize_punct(code: &str) -> Option<(PunctKind, &str)> {
    if code.len() >= 2 {
        match &code[..2] {
            "==" => return Some((PunctKind::Equal, &code[2..])),
            "!=" => return Some((PunctKind::NotEqual, &code[2..])),
            "<=" => return Some((PunctKind::LessEqual, &code[2..])),
            ">=" => return Some((PunctKind::GreaterEqual, &code[2..])),
            _ => (),
        }
    }

    assert!(!code.is_empty());

    match &code[..1] {
        "+" => Some((PunctKind::Plus, &code[1..])),
        "-" => Some((PunctKind::Minus, &code[1..])),
        "*" => Some((PunctKind::Asterisk, &code[1..])),
        "/" => Some((PunctKind::Slash, &code[1..])),
        "," => Some((PunctKind::Comma, &code[1..])),
        "&" => Some((PunctKind::Ampersand, &code[1..])),
        "<" => Some((PunctKind::Less, &code[1..])),
        ">" => Some((PunctKind::Greater, &code[1..])),
        "{" => Some((PunctKind::OpenBrace, &code[1..])),
        "}" => Some((PunctKind::CloseBrace, &code[1..])),
        "(" => Some((PunctKind::OpenParenthesis, &code[1..])),
        ")" => Some((PunctKind::CloseParenthesis, &code[1..])),
        "[" => Some((PunctKind::OpenSquareBracket, &code[1..])),
        "]" => Some((PunctKind::CloseSquareBracket, &code[1..])),
        ";" => Some((PunctKind::SemiColon, &code[1..])),
        "=" => Some((PunctKind::Assign, &code[1..])),
        _ => None,
    }
}

fn tokenize_keyword(code: &str) -> Option<(KeywordKind, &str)> {
    if code.len() >= 8 {
        match &code[..8] {
            "_Alignof" => return Some((KeywordKind::Alignof, &code[8..])),
            _ => (),
        }
    }

    if code.len() >= 6 {
        match &code[..6] {
            "return" => return Some((KeywordKind::Return, &code[6..])),
            "sizeof" => return Some((KeywordKind::Sizeof, &code[6..])),
            _ => (),
        }
    }

    if code.len() >= 5 {
        match &code[..5] {
            "while" => return Some((KeywordKind::While, &code[5..])),
            _ => (),
        }
    }

    if code.len() >= 4 {
        match &code[..4] {
            "else" => return Some((KeywordKind::Else, &code[4..])),
            "char" => return Some((KeywordKind::Char, &code[4..])),
            _ => (),
        }
    }

    if code.len() >= 3 {
        match &code[..3] {
            "int" => return Some((KeywordKind::Int, &code[3..])),
            "for" => return Some((KeywordKind::For, &code[3..])),
            _ => (),
        }
    }

    if code.len() >= 2 {
        match &code[..2] {
            "if" => return Some((KeywordKind::If, &code[2..])),
            _ => (),
        }
    }

    None
}

pub fn tokenize(mut code: &str) -> TokenList {
    let tok_seq = Rc::new(RefCell::new(Link::End));
    let mut cur = tok_seq.clone();

    while !code.is_empty() {
        let (_, ch) = code.char_indices().nth(0).unwrap();

        if code.chars().nth(0) == Some(' ') {
            code = &code[1..];
            continue;
        }

        if ch.is_digit(10) {
            let num: u64;
            (num, code) = tokenize_num(code);
            let new_tok = Rc::new(RefCell::new(Link::End));
            *cur.borrow_mut() = Link::More(Node {
                elem: TokenKind::Number(num),
                next: new_tok.clone(),
            });

            cur = new_tok;
            continue;
        }

        if ch == '"' {
            let text;
            (text, code) = tokenize_str_literal(code);

            let new_tok = Rc::new(RefCell::new(Link::End));
            *cur.borrow_mut() = Link::More(Node {
                elem: TokenKind::StrLiteral(text),
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
                elem: TokenKind::Punct(punct),
                next: new_tok.clone(),
            });

            cur = new_tok;
            continue;
        }

        if tokenize_keyword(code).is_some() {
            let keyword;
            (keyword, code) = tokenize_keyword(code).unwrap();

            let new_tok = Rc::new(RefCell::new(Link::End));
            *cur.borrow_mut() = Link::More(Node {
                elem: TokenKind::Keyword(keyword),
                next: new_tok.clone(),
            });

            cur = new_tok;
            continue;
        }

        if VARIABLE_LETTERS.contains(ch) {
            let ident: String;
            (ident, code) = tokenize_ident(code);

            let new_tok = Rc::new(RefCell::new(Link::End));
            *cur.borrow_mut() = Link::More(Node {
                elem: TokenKind::Ident(ident),
                next: new_tok.clone(),
            });

            cur = new_tok;
            continue;
        }

        if code.chars().nth(0) == Some('\n') {
            code = &code[1..];
            let new_tok = Rc::new(RefCell::new(Link::End));
            *cur.borrow_mut() = Link::More(Node {
                elem: TokenKind::Newline,
                next: new_tok.clone(),
            });

            cur = new_tok;
            continue;
        }

        unreachable!();
    }

    TokenList { head: tok_seq }
}
