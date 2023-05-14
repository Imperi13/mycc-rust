use super::tokenize::KeywordKind;
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
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

pub struct BinaryOpNode {
    pub lhs: ASTExpr,
    pub rhs: ASTExpr,
    pub kind: BinaryOpKind,
}

#[derive(Debug)]
pub enum UnaryOpKind {
    Plus,
    Minus,
}

pub struct UnaryOpNode {
    pub expr: ASTExpr,
    pub kind: UnaryOpKind,
}

pub enum ASTExprNode {
    BinaryOp(BinaryOpNode),
    UnaryOp(UnaryOpNode),
    Number(i64),
}

#[derive(Clone)]
pub struct ASTExpr {
    pub head: Rc<RefCell<ASTExprNode>>,
}

impl ASTExpr {
    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match *self.head.borrow() {
            ASTExprNode::BinaryOp(ref binary_node) => {
                writeln!(f, "{}BinaryOp {:?}", indent, binary_node.kind)?;
                writeln!(f, "{}lhs:", indent)?;
                binary_node
                    .lhs
                    .fmt_with_indent(f, &format!("{}\t", indent))?;
                writeln!(f, "{}rhs:", indent)?;
                binary_node.rhs.fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTExprNode::UnaryOp(ref unary_node) => {
                writeln!(f, "{}UnaryOp {:?}", indent, unary_node.kind)?;
                writeln!(f, "{}expr:", indent)?;
                unary_node.expr.fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTExprNode::Number(num) => {
                writeln!(f, "{}Number {}", indent, num)
            }
        }
    }
}

impl fmt::Debug for ASTExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, "")
    }
}

pub enum ASTStmtNode {
    Return(ASTExpr),
}

pub struct ASTStmt {
    pub head: Rc<RefCell<ASTStmtNode>>,
}

impl ASTStmt {
    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match *self.head.borrow() {
            ASTStmtNode::Return(ref expr) => {
                writeln!(f, "{}Return", indent)?;
                writeln!(f, "{}expr:", indent)?;
                expr.fmt_with_indent(f, &format!("{}\t", indent))
            }
        }
    }
}

impl fmt::Debug for ASTStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, "")
    }
}

pub fn parse_all(mut tok_seq: TokenList) -> ASTStmt {
    let node;
    (tok_seq, node) = parse_stmt(tok_seq).unwrap();

    if !tok_seq.is_empty() {
        panic!("parse error: cannot fully parse to the end")
    }

    ASTStmt { head: node }
}

fn parse_stmt(mut tok_seq: TokenList) -> Result<(TokenList, Rc<RefCell<ASTStmtNode>>), ParseError> {
    tok_seq = tok_seq
        .expect_keyword(KeywordKind::Return)
        .ok_or(ParseError {})?;

    let expr;
    (tok_seq, expr) = parse_expr(tok_seq)?;

    tok_seq = tok_seq
        .expect_punct(PunctKind::SemiColon)
        .ok_or(ParseError {})?;

    Ok((
        tok_seq,
        Rc::new(RefCell::new(ASTStmtNode::Return(ASTExpr { head: expr }))),
    ))
}

fn parse_expr(tok_seq: TokenList) -> Result<(TokenList, Rc<RefCell<ASTExprNode>>), ParseError> {
    parse_equality(tok_seq)
}

fn parse_equality(
    mut tok_seq: TokenList,
) -> Result<(TokenList, Rc<RefCell<ASTExprNode>>), ParseError> {
    let mut lhs;
    (tok_seq, lhs) = parse_relational(tok_seq)?;

    while !tok_seq.is_empty() {
        if let TokenKind::TokenPunct(punct) = tok_seq.get_token() {
            let kind = match punct {
                PunctKind::Equal => BinaryOpKind::Equal,
                PunctKind::NotEqual => BinaryOpKind::NotEqual,
                _ => break,
            };

            tok_seq = tok_seq.next();

            let rhs;
            (tok_seq, rhs) = parse_relational(tok_seq)?;

            lhs = Rc::new(RefCell::new(ASTExprNode::BinaryOp(BinaryOpNode {
                lhs: ASTExpr { head: lhs },
                rhs: ASTExpr { head: rhs },
                kind,
            })));
        } else {
            break;
        }
    }

    Ok((tok_seq, lhs))
}

fn parse_relational(
    mut tok_seq: TokenList,
) -> Result<(TokenList, Rc<RefCell<ASTExprNode>>), ParseError> {
    let mut lhs;
    (tok_seq, lhs) = parse_add(tok_seq)?;

    while !tok_seq.is_empty() {
        if let TokenKind::TokenPunct(punct) = tok_seq.get_token() {
            let kind = match punct {
                PunctKind::Less => BinaryOpKind::Less,
                PunctKind::LessEqual => BinaryOpKind::LessEqual,
                PunctKind::Greater => BinaryOpKind::Greater,
                PunctKind::GreaterEqual => BinaryOpKind::GreaterEqual,
                _ => break,
            };

            tok_seq = tok_seq.next();

            let rhs;
            (tok_seq, rhs) = parse_add(tok_seq)?;

            lhs = Rc::new(RefCell::new(ASTExprNode::BinaryOp(BinaryOpNode {
                lhs: ASTExpr { head: lhs },
                rhs: ASTExpr { head: rhs },
                kind,
            })));
        } else {
            break;
        }
    }

    Ok((tok_seq, lhs))
}

fn parse_add(mut tok_seq: TokenList) -> Result<(TokenList, Rc<RefCell<ASTExprNode>>), ParseError> {
    let mut lhs;
    (tok_seq, lhs) = parse_mul(tok_seq)?;

    while !tok_seq.is_empty() {
        if let TokenKind::TokenPunct(punct) = tok_seq.get_token() {
            let kind = match punct {
                PunctKind::Plus => BinaryOpKind::Add,
                PunctKind::Minus => BinaryOpKind::Sub,
                _ => break,
            };

            tok_seq = tok_seq.next();

            let rhs;
            (tok_seq, rhs) = parse_mul(tok_seq)?;

            lhs = Rc::new(RefCell::new(ASTExprNode::BinaryOp(BinaryOpNode {
                lhs: ASTExpr { head: lhs },
                rhs: ASTExpr { head: rhs },
                kind,
            })));
        } else {
            break;
        }
    }

    Ok((tok_seq, lhs))
}

fn parse_mul(mut tok_seq: TokenList) -> Result<(TokenList, Rc<RefCell<ASTExprNode>>), ParseError> {
    let mut lhs;
    (tok_seq, lhs) = parse_unary(tok_seq)?;

    while !tok_seq.is_empty() {
        if let TokenKind::TokenPunct(punct) = tok_seq.get_token() {
            let kind = match punct {
                PunctKind::Asterisk => BinaryOpKind::Mul,
                PunctKind::Slash => BinaryOpKind::Div,
                _ => break,
            };

            tok_seq = tok_seq.next();

            let rhs;
            (tok_seq, rhs) = parse_unary(tok_seq)?;

            lhs = Rc::new(RefCell::new(ASTExprNode::BinaryOp(BinaryOpNode {
                lhs: ASTExpr { head: lhs },
                rhs: ASTExpr { head: rhs },
                kind,
            })));
        } else {
            break;
        }
    }

    Ok((tok_seq, lhs))
}

fn parse_unary(
    mut tok_seq: TokenList,
) -> Result<(TokenList, Rc<RefCell<ASTExprNode>>), ParseError> {
    if tok_seq.expect_punct(PunctKind::Plus).is_some() {
        tok_seq = tok_seq
            .expect_punct(PunctKind::Plus)
            .ok_or(ParseError {})?;
        let head;
        (tok_seq, head) = parse_unary(tok_seq)?;

        let node = Rc::new(RefCell::new(ASTExprNode::UnaryOp(UnaryOpNode {
            expr: ASTExpr { head },
            kind: UnaryOpKind::Plus,
        })));

        Ok((tok_seq, node))
    } else if tok_seq.expect_punct(PunctKind::Minus).is_some() {
        tok_seq = tok_seq
            .expect_punct(PunctKind::Minus)
            .ok_or(ParseError {})?;
        let head;
        (tok_seq, head) = parse_unary(tok_seq)?;

        let node = Rc::new(RefCell::new(ASTExprNode::UnaryOp(UnaryOpNode {
            expr: ASTExpr { head },
            kind: UnaryOpKind::Minus,
        })));

        Ok((tok_seq, node))
    } else {
        parse_primary(tok_seq)
    }
}

fn parse_primary(
    mut tok_seq: TokenList,
) -> Result<(TokenList, Rc<RefCell<ASTExprNode>>), ParseError> {
    if tok_seq.is_empty() {
        return Err(ParseError {});
    }

    if let TokenKind::TokenNumber(num) = tok_seq.get_token() {
        Ok((
            tok_seq.next(),
            Rc::new(RefCell::new(ASTExprNode::Number(num))),
        ))
    } else if tok_seq
        .expect_punct(PunctKind::OpenParenthesis)
        .is_some()
    {
        tok_seq = tok_seq
            .expect_punct(PunctKind::OpenParenthesis)
            .ok_or(ParseError {})?;
        let ret;
        (tok_seq, ret) = parse_expr(tok_seq)?;

        tok_seq = tok_seq
            .expect_punct(PunctKind::CloseParenthesis)
            .ok_or(ParseError {})?;

        Ok((tok_seq, ret))
    } else {
        Err(ParseError {})
    }
}
