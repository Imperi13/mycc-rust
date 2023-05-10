use super::tokenize::PunctKind;
use super::tokenize::TokenKind;
use super::tokenize::TokenList;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ParseError;

#[derive(Debug)]
pub enum BinaryOpKind {
    BinaryOpPlus,
    BinaryOpMinus,
    BinaryOpMul,
    BinaryOpDiv,
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

impl AST {
    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match *self.head.borrow() {
            ASTNode::ASTBinaryOp(ref binary_node) => {
                writeln!(f, "{}BinaryOp {:?}", indent, binary_node.kind)?;
                writeln!(f, "{}lhs:", indent)?;
                binary_node
                    .lhs
                    .fmt_with_indent(f, &format!("{}\t", indent))?;
                writeln!(f, "{}rhs:", indent)?;
                binary_node.rhs.fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTNode::ASTNumber(num) => {
                writeln!(f, "{}Number {}", indent, num)
            }
        }
    }
}

impl fmt::Debug for AST {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, "")
    }
}

pub fn parse_all(mut tok_seq: TokenList) -> AST {
    let node;
    (tok_seq, node) = parse_expr(tok_seq).unwrap();

    if !tok_seq.is_empty() {
        panic!("parse error: cannot fully parse to the end")
    }

    AST { head: node }
}

fn parse_expr(tok_seq: TokenList) -> Result<(TokenList, Rc<RefCell<ASTNode>>), ParseError> {
    parse_add(tok_seq)
}

fn parse_add(mut tok_seq: TokenList) -> Result<(TokenList, Rc<RefCell<ASTNode>>), ParseError> {
    let mut lhs;
    (tok_seq, lhs) = parse_mul(tok_seq)?;

    while !tok_seq.is_empty() {
        if let TokenKind::TokenPunct(punct) = tok_seq.get_token() {
            let kind = match punct {
                PunctKind::PunctPlus => BinaryOpKind::BinaryOpPlus,
                PunctKind::PunctMinus => BinaryOpKind::BinaryOpMinus,
                _ => break,
            };

            tok_seq = tok_seq.next();

            let rhs;
            (tok_seq, rhs) = parse_mul(tok_seq)?;

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

fn parse_mul(mut tok_seq: TokenList) -> Result<(TokenList, Rc<RefCell<ASTNode>>), ParseError> {
    let mut lhs;
    (tok_seq, lhs) = parse_primary(tok_seq)?;

    while !tok_seq.is_empty() {
        if let TokenKind::TokenPunct(punct) = tok_seq.get_token() {
            let kind = match punct {
                PunctKind::PunctAsterisk => BinaryOpKind::BinaryOpMul,
                PunctKind::PunctSlash => BinaryOpKind::BinaryOpDiv,
                _ => break,
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

fn parse_primary(mut tok_seq: TokenList) -> Result<(TokenList, Rc<RefCell<ASTNode>>), ParseError> {
    if tok_seq.is_empty() {
        return Err(ParseError {});
    }

    if let TokenKind::TokenNumber(num) = tok_seq.get_token() {
        Ok((
            tok_seq.next(),
            Rc::new(RefCell::new(ASTNode::ASTNumber(num))),
        ))
    } else if tok_seq
        .expect_punct(PunctKind::PunctOpenParenthesis)
        .is_some()
    {
        tok_seq = tok_seq
            .expect_punct(PunctKind::PunctOpenParenthesis)
            .unwrap();
        let ret;
        (tok_seq, ret) = parse_expr(tok_seq)?;

        tok_seq = tok_seq
            .expect_punct(PunctKind::PunctCloseParenthesis)
            .ok_or(ParseError {})?;

        Ok((tok_seq, ret))
    } else {
        Err(ParseError {})
    }
}
