use super::tokenize::KeywordKind;
use super::tokenize::PunctKind;
use super::tokenize::TokenKind;
use super::tokenize::TokenList;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ParseError;

#[derive(Debug)]
pub enum BinaryOpKind {
    Assign,
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
    FuncCall(ASTExpr),
    Number(i64),
    Var(Rc<RefCell<Obj>>),
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
            ASTExprNode::FuncCall(ref func_expr) => {
                writeln!(f, "{}FuncCall", indent)?;
                writeln!(f, "{}func_expr:", indent)?;
                func_expr.fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTExprNode::Number(num) => {
                writeln!(f, "{}Number {}", indent, num)
            }
            ASTExprNode::Var(ref obj) => {
                writeln!(f, "{}Var {}", indent, &*obj.borrow().name)
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
    Declaration(Rc<RefCell<Obj>>),
    ExprStmt(ASTExpr),
    Block(Vec<ASTStmt>),
    If(ASTExpr, ASTStmt, Option<ASTStmt>),
    While(ASTExpr, ASTStmt),
    For(ASTExpr, ASTExpr, ASTExpr, ASTStmt),
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
            ASTStmtNode::Declaration(ref obj) => {
                writeln!(f, "{}Declaration :{}", indent, &*obj.borrow().name)
            }
            ASTStmtNode::ExprStmt(ref expr) => {
                writeln!(f, "{}ExprStmt", indent)?;
                writeln!(f, "{}expr:", indent)?;
                expr.fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTStmtNode::Block(ref stmts) => {
                writeln!(f, "{}Block", indent)?;
                for (i, stmt) in stmts.iter().enumerate() {
                    writeln!(f, "{} {}th expr:", indent, i)?;
                    stmt.fmt_with_indent(f, &format!("{}\t", indent))?;
                }
                Ok(())
            }
            ASTStmtNode::If(ref cond, ref if_stmt, ref else_stmt) => {
                writeln!(f, "{}If", indent)?;
                writeln!(f, "{}cond:", indent)?;
                cond.fmt_with_indent(f, &format!("{}\t", indent))?;
                writeln!(f, "{}if_stmt:", indent)?;
                if_stmt.fmt_with_indent(f, &format!("{}\t", indent))?;

                if else_stmt.is_some() {
                    let else_stmt = else_stmt.clone().unwrap();

                    writeln!(f, "{}else_stmt:", indent)?;
                    else_stmt.fmt_with_indent(f, &format!("{}\t", indent))
                } else {
                    Ok(())
                }
            }
            ASTStmtNode::While(ref cond, ref stmt) => {
                writeln!(f, "{}While", indent)?;
                writeln!(f, "{}cond:", indent)?;
                cond.fmt_with_indent(f, &format!("{}\t", indent))?;
                writeln!(f, "{}stmt:", indent)?;
                stmt.fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTStmtNode::For(ref start, ref cond, ref step, ref stmt) => {
                writeln!(f, "{}For", indent)?;
                writeln!(f, "{}start:", indent)?;
                start.fmt_with_indent(f, &format!("{}\t", indent))?;
                writeln!(f, "{}cond:", indent)?;
                cond.fmt_with_indent(f, &format!("{}\t", indent))?;
                writeln!(f, "{}step:", indent)?;
                step.fmt_with_indent(f, &format!("{}\t", indent))?;
                writeln!(f, "{}stmt:", indent)?;
                stmt.fmt_with_indent(f, &format!("{}\t", indent))
            }
        }
    }
}

impl fmt::Debug for ASTStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, "")
    }
}

pub enum ASTGlobalNode {
    Function(Rc<RefCell<Obj>>, Vec<ASTStmt>),
}

#[derive(Clone)]
pub struct ASTGlobal {
    pub head: Rc<RefCell<ASTGlobalNode>>,
}

impl ASTGlobal {
    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match *self.head.borrow() {
            ASTGlobalNode::Function(ref obj, ref stmts) => {
                writeln!(f, "{}Function {}", indent, &*obj.borrow().name)?;
                for (i, stmt) in stmts.iter().enumerate() {
                    writeln!(f, "{} {}th stmt:", indent, i)?;
                    stmt.fmt_with_indent(f, &format!("{}\t", indent))?;
                }
                Ok(())
            }
        }
    }
}

impl fmt::Debug for ASTGlobal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, "")
    }
}

pub fn parse_all(mut tok_seq: TokenList) -> Vec<ASTGlobal> {
    let mut ret = Vec::new();
    let mut arena = ParseArena {
        objs: HashMap::new(),
    };

    while !tok_seq.is_empty() {
        let node;
        (tok_seq, node) = arena.parse_global(tok_seq).unwrap();

        ret.push(ASTGlobal { head: node });
    }

    ret
}

pub enum TypeNode {
    Int,
    Func,
    Ptr(Type),
}

#[derive(Clone)]
pub struct Type {
    pub head: Rc<TypeNode>,
}

impl Type {
    pub fn new(kind: TypeNode) -> Type {
        Type {
            head: Rc::new(kind),
        }
    }

    pub fn is_function_type(&self) -> bool {
        matches!(*self.head, TypeNode::Func)
    }

    pub fn is_int_type(&self) -> bool {
        matches!(*self.head, TypeNode::Int)
    }

    pub fn is_ptr_type(&self) -> bool {
        matches!(*self.head, TypeNode::Ptr(_))
    }
}

pub struct Obj {
    pub id: usize,
    pub name: String,
    pub obj_type: Type,
}

pub struct ParseArena {
    objs: HashMap<String, Rc<RefCell<Obj>>>,
}

impl ParseArena {
    fn insert_obj(&mut self, obj_name: &str, obj_type: Type) -> Rc<RefCell<Obj>> {
        if self.objs.contains_key(obj_name) {
            panic!("insert_obj");
        }

        let obj_id = self.objs.len();
        let obj = Rc::new(RefCell::new(Obj {
            id: obj_id,
            name: String::from(obj_name),
            obj_type,
        }));

        self.objs.insert(String::from(obj_name), obj.clone());
        obj
    }

    fn get_obj(&self, obj_name: &str) -> Rc<RefCell<Obj>> {
        if !self.objs.contains_key(obj_name) {
            panic!("get_obj");
        }

        self.objs.get(obj_name).unwrap().clone()
    }

    fn parse_global(
        &mut self,
        mut tok_seq: TokenList,
    ) -> Result<(TokenList, Rc<RefCell<ASTGlobalNode>>), ParseError> {
        tok_seq = tok_seq
            .expect_keyword(KeywordKind::Int)
            .ok_or(ParseError {})?;

        let func_name = match tok_seq.get_token() {
            TokenKind::TokenIdent(ident) => ident,
            _ => return Err(ParseError {}),
        };

        tok_seq = tok_seq.next();

        tok_seq = tok_seq
            .expect_punct(PunctKind::OpenParenthesis)
            .ok_or(ParseError {})?;
        tok_seq = tok_seq
            .expect_punct(PunctKind::CloseParenthesis)
            .ok_or(ParseError {})?;

        let obj = self.insert_obj(&func_name, Type::new(TypeNode::Func));
        let mut stmts = Vec::new();

        tok_seq = tok_seq
            .expect_punct(PunctKind::OpenBrace)
            .ok_or(ParseError {})?;

        while tok_seq.expect_punct(PunctKind::CloseBrace).is_none() {
            let stmt;
            (tok_seq, stmt) = self.parse_stmt(tok_seq)?;
            stmts.push(ASTStmt { head: stmt });
        }

        tok_seq = tok_seq
            .expect_punct(PunctKind::CloseBrace)
            .ok_or(ParseError {})?;

        Ok((
            tok_seq,
            Rc::new(RefCell::new(ASTGlobalNode::Function(obj, stmts))),
        ))
    }

    fn parse_stmt(
        &mut self,
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

                let obj = self.insert_obj(var_name, Type::new(TypeNode::Int));
                Ok((
                    tok_seq,
                    Rc::new(RefCell::new(ASTStmtNode::Declaration(obj))),
                ))
            } else {
                Err(ParseError {})
            }
        } else if tok_seq.expect_keyword(KeywordKind::If).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::If)
                .ok_or(ParseError {})?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenParenthesis)
                .ok_or(ParseError {})?;

            let cond;
            (tok_seq, cond) = self.parse_expr(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseParenthesis)
                .ok_or(ParseError {})?;

            let if_stmt;
            (tok_seq, if_stmt) = self.parse_stmt(tok_seq)?;

            if tok_seq.expect_keyword(KeywordKind::Else).is_some() {
                tok_seq = tok_seq
                    .expect_keyword(KeywordKind::Else)
                    .ok_or(ParseError {})?;

                let else_stmt;
                (tok_seq, else_stmt) = self.parse_stmt(tok_seq)?;

                Ok((
                    tok_seq,
                    Rc::new(RefCell::new(ASTStmtNode::If(
                        ASTExpr { head: cond },
                        ASTStmt { head: if_stmt },
                        Some(ASTStmt { head: else_stmt }),
                    ))),
                ))
            } else {
                Ok((
                    tok_seq,
                    Rc::new(RefCell::new(ASTStmtNode::If(
                        ASTExpr { head: cond },
                        ASTStmt { head: if_stmt },
                        None,
                    ))),
                ))
            }
        } else if tok_seq.expect_keyword(KeywordKind::While).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::While)
                .ok_or(ParseError {})?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenParenthesis)
                .ok_or(ParseError {})?;

            let cond;
            (tok_seq, cond) = self.parse_expr(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseParenthesis)
                .ok_or(ParseError {})?;

            let stmt;
            (tok_seq, stmt) = self.parse_stmt(tok_seq)?;
            Ok((
                tok_seq,
                Rc::new(RefCell::new(ASTStmtNode::While(
                    ASTExpr { head: cond },
                    ASTStmt { head: stmt },
                ))),
            ))
        } else if tok_seq.expect_keyword(KeywordKind::For).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::For)
                .ok_or(ParseError {})?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenParenthesis)
                .ok_or(ParseError {})?;

            let start;
            (tok_seq, start) = self.parse_expr(tok_seq)?;
            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError {})?;

            let cond;
            (tok_seq, cond) = self.parse_expr(tok_seq)?;
            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError {})?;

            let step;
            (tok_seq, step) = self.parse_expr(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseParenthesis)
                .ok_or(ParseError {})?;

            let stmt;
            (tok_seq, stmt) = self.parse_stmt(tok_seq)?;
            Ok((
                tok_seq,
                Rc::new(RefCell::new(ASTStmtNode::For(
                    ASTExpr { head: start },
                    ASTExpr { head: cond },
                    ASTExpr { head: step },
                    ASTStmt { head: stmt },
                ))),
            ))
        } else if tok_seq.expect_punct(PunctKind::OpenBrace).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenBrace)
                .ok_or(ParseError {})?;

            let mut stmts = Vec::new();

            while tok_seq.expect_punct(PunctKind::CloseBrace).is_none() {
                let stmt;
                (tok_seq, stmt) = self.parse_stmt(tok_seq)?;
                stmts.push(ASTStmt { head: stmt });
            }

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseBrace)
                .ok_or(ParseError {})?;

            Ok((tok_seq, Rc::new(RefCell::new(ASTStmtNode::Block(stmts)))))
        } else {
            let expr;
            (tok_seq, expr) = self.parse_expr(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError {})?;

            Ok((
                tok_seq,
                Rc::new(RefCell::new(ASTStmtNode::ExprStmt(ASTExpr { head: expr }))),
            ))
        }
    }

    fn parse_expr(
        &self,
        tok_seq: TokenList,
    ) -> Result<(TokenList, Rc<RefCell<ASTExprNode>>), ParseError> {
        self.parse_assign(tok_seq)
    }

    fn parse_assign(
        &self,
        mut tok_seq: TokenList,
    ) -> Result<(TokenList, Rc<RefCell<ASTExprNode>>), ParseError> {
        let lhs;
        (tok_seq, lhs) = self.parse_equality(tok_seq)?;

        if tok_seq.expect_punct(PunctKind::Assign).is_some() {
            tok_seq = tok_seq.next();

            let rhs;
            (tok_seq, rhs) = self.parse_assign(tok_seq)?;

            Ok((
                tok_seq,
                Rc::new(RefCell::new(ASTExprNode::BinaryOp(BinaryOpNode {
                    lhs: ASTExpr { head: lhs },
                    rhs: ASTExpr { head: rhs },
                    kind: BinaryOpKind::Assign,
                }))),
            ))
        } else {
            Ok((tok_seq, lhs))
        }
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
            self.parse_postfix(tok_seq)
        }
    }

    fn parse_postfix(
        &self,
        mut tok_seq: TokenList,
    ) -> Result<(TokenList, Rc<RefCell<ASTExprNode>>), ParseError> {
        let expr;
        (tok_seq, expr) = self.parse_primary(tok_seq)?;

        if tok_seq.expect_punct(PunctKind::OpenParenthesis).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenParenthesis)
                .ok_or(ParseError {})?;
            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseParenthesis)
                .ok_or(ParseError {})?;

            Ok((
                tok_seq,
                Rc::new(RefCell::new(ASTExprNode::FuncCall(ASTExpr { head: expr }))),
            ))
        } else {
            Ok((tok_seq, expr))
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
        } else if let TokenKind::TokenIdent(ref var_name) = tok_seq.get_token() {
            let obj = self.get_obj(var_name);
            Ok((tok_seq.next(), Rc::new(RefCell::new(ASTExprNode::Var(obj)))))
        } else {
            Err(ParseError {})
        }
    }
}
