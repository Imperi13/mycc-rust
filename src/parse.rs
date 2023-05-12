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
    BinaryOpAdd,
    BinaryOpSub,
    BinaryOpMul,
    BinaryOpDiv,
}

pub struct BinaryOpNode {
    pub lhs: AST,
    pub rhs: AST,
    pub kind: BinaryOpKind,
}

#[derive(Debug)]
pub enum UnaryOpKind {
    UnaryOpPlus,
    UnaryOpMinus,
}

pub struct UnaryOpNode {
    pub expr: AST,
    pub kind: UnaryOpKind,
}

pub enum ASTNode {
    ASTBinaryOp(BinaryOpNode),
    ASTUnaryOp(UnaryOpNode),
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
            ASTNode::ASTUnaryOp(ref unary_node) => {
                writeln!(f, "{}UnaryOp {:?}", indent, unary_node.kind)?;
                writeln!(f, "{}expr:", indent)?;
                unary_node.expr.fmt_with_indent(f, &format!("{}\t", indent))
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
                PunctKind::PunctPlus => BinaryOpKind::BinaryOpAdd,
                PunctKind::PunctDash => BinaryOpKind::BinaryOpSub,
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
    (tok_seq, lhs) = parse_unary(tok_seq)?;

    while !tok_seq.is_empty() {
        if let TokenKind::TokenPunct(punct) = tok_seq.get_token() {
            let kind = match punct {
                PunctKind::PunctAsterisk => BinaryOpKind::BinaryOpMul,
                PunctKind::PunctSlash => BinaryOpKind::BinaryOpDiv,
                _ => break,
            };

            tok_seq = tok_seq.next();

            let rhs;
            (tok_seq, rhs) = parse_unary(tok_seq)?;

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

fn parse_unary(mut tok_seq: TokenList) -> Result<(TokenList, Rc<RefCell<ASTNode>>), ParseError> {
    if tok_seq.expect_punct(PunctKind::PunctPlus).is_some() {
        tok_seq = tok_seq
            .expect_punct(PunctKind::PunctPlus)
            .ok_or(ParseError {})?;
        let head;
        (tok_seq, head) = parse_unary(tok_seq)?;

        let node = Rc::new(RefCell::new(ASTNode::ASTUnaryOp(UnaryOpNode {
            expr: AST { head },
            kind: UnaryOpKind::UnaryOpPlus,
        })));

        Ok((tok_seq, node))
    } else if tok_seq.expect_punct(PunctKind::PunctDash).is_some() {
        tok_seq = tok_seq
            .expect_punct(PunctKind::PunctDash)
            .ok_or(ParseError {})?;
        let head;
        (tok_seq, head) = parse_unary(tok_seq)?;

        let node = Rc::new(RefCell::new(ASTNode::ASTUnaryOp(UnaryOpNode {
            expr: AST { head },
            kind: UnaryOpKind::UnaryOpMinus,
        })));

        Ok((tok_seq, node))
    } else {
        parse_primary(tok_seq)
    }
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
            .ok_or(ParseError {})?;
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
