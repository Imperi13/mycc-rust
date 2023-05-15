use super::tokenize::KeywordKind;
use super::tokenize::PunctKind;
use super::tokenize::TokenKind;
use super::tokenize::TokenList;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use inkwell::values::PointerValue;

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
    Declaration(String),
}

#[derive(Clone)]
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
            ASTStmtNode::Declaration(ref var_name) => {
                writeln!(f, "{}Declaration :{}", indent, var_name)
            }
        }
    }
}

impl fmt::Debug for ASTStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, "")
    }
}

pub fn parse_all(mut tok_seq: TokenList) -> Vec<ASTStmt> {
    let mut ret = Vec::new();
    let arena = ParseArena {
        vars: HashMap::new(),
    };
    while !tok_seq.is_empty() {
        let node;
        (tok_seq, node) = arena.parse_stmt(tok_seq).unwrap();

        ret.push(ASTStmt { head: node });
    }

    ret
}

pub struct Obj<'ctx> {
    name: String,
    ptr: PointerValue<'ctx>,
}

pub struct ParseArena<'ctx> {
    vars: HashMap<String, Rc<RefCell<Obj<'ctx>>>>,
}

impl<'ctx> ParseArena<'ctx> {
    fn parse_stmt(
        &self,
        mut tok_seq: TokenList,
    ) -> Result<(TokenList, Rc<RefCell<ASTStmtNode>>), ParseError> {
        if tok_seq.expect_keyword(KeywordKind::Return).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::Return)
                .ok_or(ParseError {})?;

            let expr;
            (tok_seq, expr) = self.parse_expr(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError {})?;

            Ok((
                tok_seq,
                Rc::new(RefCell::new(ASTStmtNode::Return(ASTExpr { head: expr }))),
            ))
        } else if tok_seq.expect_keyword(KeywordKind::Int).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::Int)
                .ok_or(ParseError {})?;

            if let TokenKind::TokenIdent(ref var_name) = tok_seq.get_token() {
                tok_seq = tok_seq.next();

                tok_seq = tok_seq
                    .expect_punct(PunctKind::SemiColon)
                    .ok_or(ParseError {})?;
                Ok((
                    tok_seq,
                    Rc::new(RefCell::new(ASTStmtNode::Declaration(var_name.clone()))),
                ))
            } else {
                Err(ParseError {})
            }
        } else {
            Err(ParseError {})
        }
    }

    fn parse_expr(
        &self,
        tok_seq: TokenList,
    ) -> Result<(TokenList, Rc<RefCell<ASTExprNode>>), ParseError> {
        self.parse_equality(tok_seq)
    }

    fn parse_equality(
        &self,
        mut tok_seq: TokenList,
    ) -> Result<(TokenList, Rc<RefCell<ASTExprNode>>), ParseError> {
        let mut lhs;
        (tok_seq, lhs) = self.parse_relational(tok_seq)?;

        while !tok_seq.is_empty() {
            if let TokenKind::TokenPunct(punct) = tok_seq.get_token() {
                let kind = match punct {
                    PunctKind::Equal => BinaryOpKind::Equal,
                    PunctKind::NotEqual => BinaryOpKind::NotEqual,
                    _ => break,
                };

                tok_seq = tok_seq.next();

                let rhs;
                (tok_seq, rhs) = self.parse_relational(tok_seq)?;

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
        &self,
        mut tok_seq: TokenList,
    ) -> Result<(TokenList, Rc<RefCell<ASTExprNode>>), ParseError> {
        let mut lhs;
        (tok_seq, lhs) = self.parse_add(tok_seq)?;

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
                (tok_seq, rhs) = self.parse_add(tok_seq)?;

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

    fn parse_add(
        &self,
        mut tok_seq: TokenList,
    ) -> Result<(TokenList, Rc<RefCell<ASTExprNode>>), ParseError> {
        let mut lhs;
        (tok_seq, lhs) = self.parse_mul(tok_seq)?;

        while !tok_seq.is_empty() {
            if let TokenKind::TokenPunct(punct) = tok_seq.get_token() {
                let kind = match punct {
                    PunctKind::Plus => BinaryOpKind::Add,
                    PunctKind::Minus => BinaryOpKind::Sub,
                    _ => break,
                };

                tok_seq = tok_seq.next();

                let rhs;
                (tok_seq, rhs) = self.parse_mul(tok_seq)?;

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

    fn parse_mul(
        &self,
        mut tok_seq: TokenList,
    ) -> Result<(TokenList, Rc<RefCell<ASTExprNode>>), ParseError> {
        let mut lhs;
        (tok_seq, lhs) = self.parse_unary(tok_seq)?;

        while !tok_seq.is_empty() {
            if let TokenKind::TokenPunct(punct) = tok_seq.get_token() {
                let kind = match punct {
                    PunctKind::Asterisk => BinaryOpKind::Mul,
                    PunctKind::Slash => BinaryOpKind::Div,
                    _ => break,
                };

                tok_seq = tok_seq.next();

                let rhs;
                (tok_seq, rhs) = self.parse_unary(tok_seq)?;

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
        &self,
        mut tok_seq: TokenList,
    ) -> Result<(TokenList, Rc<RefCell<ASTExprNode>>), ParseError> {
        if tok_seq.expect_punct(PunctKind::Plus).is_some() {
            tok_seq = tok_seq.expect_punct(PunctKind::Plus).ok_or(ParseError {})?;
            let head;
            (tok_seq, head) = self.parse_unary(tok_seq)?;

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
            (tok_seq, head) = self.parse_unary(tok_seq)?;

            let node = Rc::new(RefCell::new(ASTExprNode::UnaryOp(UnaryOpNode {
                expr: ASTExpr { head },
                kind: UnaryOpKind::Minus,
            })));

            Ok((tok_seq, node))
        } else {
            self.parse_primary(tok_seq)
        }
    }

    fn parse_primary(
        &self,
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
        } else if tok_seq.expect_punct(PunctKind::OpenParenthesis).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenParenthesis)
                .ok_or(ParseError {})?;
            let ret;
            (tok_seq, ret) = self.parse_expr(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseParenthesis)
                .ok_or(ParseError {})?;

            Ok((tok_seq, ret))
        } else {
            Err(ParseError {})
        }
    }
}
