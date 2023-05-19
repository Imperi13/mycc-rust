use crate::ast::ASTExpr;
use crate::ast::ASTExprNode;
use crate::ast::ASTGlobal;
use crate::ast::ASTGlobalNode;
use crate::ast::ASTStmt;
use crate::ast::ASTStmtNode;
use crate::ast::BinaryOpKind;
use crate::ast::BinaryOpNode;
use crate::ast::UnaryOpKind;
use crate::ast::UnaryOpNode;
use crate::tokenize::KeywordKind;
use crate::tokenize::PunctKind;
use crate::tokenize::TokenKind;
use crate::tokenize::TokenList;
use crate::types::FunctionTypeNode;
use crate::types::Type;
use crate::types::TypeNode;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ParseError;

pub fn parse_all(mut tok_seq: TokenList) -> Vec<ASTGlobal> {
    let mut ret = Vec::new();
    let mut arena = ParseArena {
        objs: HashMap::new(),
    };

    while !tok_seq.is_empty() {
        let node;
        (tok_seq, node) = arena.parse_global(tok_seq).unwrap();

        ret.push(node);
    }

    ret
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

    fn parse_declarator(
        mut tok_seq: TokenList,
        decl_spec_type: Type,
    ) -> Result<(TokenList, String, Type), ParseError> {
        let mut ret_type = decl_spec_type;

        while tok_seq.expect_punct(PunctKind::Asterisk).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::Asterisk)
                .ok_or(ParseError {})?;
            ret_type = Type::new_ptr_type(ret_type);
        }

        if let TokenKind::TokenIdent(var_name) = tok_seq.get_token() {
            tok_seq = tok_seq.next();
            Ok((tok_seq, var_name, ret_type))
        } else {
            Err(ParseError {})
        }
    }

    fn parse_global(
        &mut self,
        mut tok_seq: TokenList,
    ) -> Result<(TokenList, ASTGlobal), ParseError> {
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

        let return_type = Type::new(TypeNode::Int);
        let obj = self.insert_obj(
            &func_name,
            Type::new(TypeNode::Func(FunctionTypeNode { return_type })),
        );
        let mut stmts = Vec::new();

        tok_seq = tok_seq
            .expect_punct(PunctKind::OpenBrace)
            .ok_or(ParseError {})?;

        while tok_seq.expect_punct(PunctKind::CloseBrace).is_none() {
            let stmt;
            (tok_seq, stmt) = self.parse_stmt(tok_seq)?;
            stmts.push(stmt);
        }

        tok_seq = tok_seq
            .expect_punct(PunctKind::CloseBrace)
            .ok_or(ParseError {})?;

        Ok((tok_seq, ASTGlobal::new(ASTGlobalNode::Function(obj, stmts))))
    }

    fn parse_stmt(&mut self, mut tok_seq: TokenList) -> Result<(TokenList, ASTStmt), ParseError> {
        if tok_seq.expect_keyword(KeywordKind::Return).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::Return)
                .ok_or(ParseError {})?;

            let expr;
            (tok_seq, expr) = self.parse_expr(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError {})?;

            Ok((tok_seq, ASTStmt::new(ASTStmtNode::Return(expr))))
        } else if tok_seq.expect_keyword(KeywordKind::Int).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::Int)
                .ok_or(ParseError {})?;

            let var_name;
            let var_type;
            (tok_seq, var_name, var_type) =
                ParseArena::parse_declarator(tok_seq, Type::new(TypeNode::Int))?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError {})?;

            let obj = self.insert_obj(&var_name, var_type);
            Ok((tok_seq, ASTStmt::new(ASTStmtNode::Declaration(obj))))
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
                    ASTStmt::new(ASTStmtNode::If(cond, if_stmt, Some(else_stmt))),
                ))
            } else {
                Ok((tok_seq, ASTStmt::new(ASTStmtNode::If(cond, if_stmt, None))))
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
            Ok((tok_seq, ASTStmt::new(ASTStmtNode::While(cond, stmt))))
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
                ASTStmt::new(ASTStmtNode::For(start, cond, step, stmt)),
            ))
        } else if tok_seq.expect_punct(PunctKind::OpenBrace).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenBrace)
                .ok_or(ParseError {})?;

            let mut stmts = Vec::new();

            while tok_seq.expect_punct(PunctKind::CloseBrace).is_none() {
                let stmt;
                (tok_seq, stmt) = self.parse_stmt(tok_seq)?;
                stmts.push(stmt);
            }

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseBrace)
                .ok_or(ParseError {})?;

            Ok((tok_seq, ASTStmt::new(ASTStmtNode::Block(stmts))))
        } else {
            let expr;
            (tok_seq, expr) = self.parse_expr(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError {})?;

            Ok((tok_seq, ASTStmt::new(ASTStmtNode::ExprStmt(expr))))
        }
    }

    fn parse_expr(&self, tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
        self.parse_assign(tok_seq)
    }

    fn parse_assign(&self, mut tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
        let lhs;
        (tok_seq, lhs) = self.parse_equality(tok_seq)?;

        if tok_seq.expect_punct(PunctKind::Assign).is_some() {
            tok_seq = tok_seq.next();

            let rhs;
            (tok_seq, rhs) = self.parse_assign(tok_seq)?;
            let expr_type = lhs.expr_type.clone();

            Ok((
                tok_seq,
                ASTExpr::new(
                    ASTExprNode::BinaryOp(BinaryOpNode {
                        lhs,
                        rhs,
                        kind: BinaryOpKind::Assign,
                    }),
                    expr_type,
                ),
            ))
        } else {
            Ok((tok_seq, lhs))
        }
    }

    fn parse_equality(&self, mut tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
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

                let expr_type = lhs.expr_type.clone();

                lhs = ASTExpr::new(
                    ASTExprNode::BinaryOp(BinaryOpNode { lhs, rhs, kind }),
                    expr_type,
                );
            } else {
                break;
            }
        }

        Ok((tok_seq, lhs))
    }

    fn parse_relational(&self, mut tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
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

                let expr_type = lhs.expr_type.clone();

                lhs = ASTExpr::new(
                    ASTExprNode::BinaryOp(BinaryOpNode { lhs, rhs, kind }),
                    expr_type,
                );
            } else {
                break;
            }
        }

        Ok((tok_seq, lhs))
    }

    fn parse_add(&self, mut tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
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

                let expr_type = lhs.expr_type.clone();

                lhs = ASTExpr::new(
                    ASTExprNode::BinaryOp(BinaryOpNode { lhs, rhs, kind }),
                    expr_type,
                );
            } else {
                break;
            }
        }

        Ok((tok_seq, lhs))
    }

    fn parse_mul(&self, mut tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
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
                let expr_type = lhs.expr_type.clone();

                lhs = ASTExpr::new(
                    ASTExprNode::BinaryOp(BinaryOpNode { lhs, rhs, kind }),
                    expr_type,
                );
            } else {
                break;
            }
        }

        Ok((tok_seq, lhs))
    }

    fn parse_unary(&self, mut tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
        if tok_seq.expect_punct(PunctKind::Plus).is_some() {
            tok_seq = tok_seq.expect_punct(PunctKind::Plus).ok_or(ParseError {})?;
            let expr;
            (tok_seq, expr) = self.parse_unary(tok_seq)?;
            let expr_type = expr.expr_type.clone();

            let node = ASTExpr::new(
                ASTExprNode::UnaryOp(UnaryOpNode {
                    expr,
                    kind: UnaryOpKind::Plus,
                }),
                expr_type,
            );

            Ok((tok_seq, node))
        } else if tok_seq.expect_punct(PunctKind::Minus).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::Minus)
                .ok_or(ParseError {})?;
            let expr;
            (tok_seq, expr) = self.parse_unary(tok_seq)?;
            let expr_type = expr.expr_type.clone();

            let node = ASTExpr::new(
                ASTExprNode::UnaryOp(UnaryOpNode {
                    expr,
                    kind: UnaryOpKind::Minus,
                }),
                expr_type,
            );

            Ok((tok_seq, node))
        } else if tok_seq.expect_punct(PunctKind::Ampersand).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::Ampersand)
                .ok_or(ParseError {})?;

            let expr;
            (tok_seq, expr) = self.parse_unary(tok_seq)?;
            let expr_type = expr.expr_type.clone();

            let node = ASTExpr::new(
                ASTExprNode::UnaryOp(UnaryOpNode {
                    expr,
                    kind: UnaryOpKind::Addr,
                }),
                Type::new_ptr_type(expr_type),
            );

            Ok((tok_seq, node))
        } else if tok_seq.expect_punct(PunctKind::Asterisk).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::Asterisk)
                .ok_or(ParseError {})?;

            let expr;
            (tok_seq, expr) = self.parse_unary(tok_seq)?;
            let expr_type = expr.expr_type.clone();

            let node = ASTExpr::new(
                ASTExprNode::UnaryOp(UnaryOpNode {
                    expr,
                    kind: UnaryOpKind::Deref,
                }),
                expr_type.get_ptr_to().unwrap(),
            );

            Ok((tok_seq, node))
        } else {
            self.parse_postfix(tok_seq)
        }
    }

    fn parse_postfix(&self, mut tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
        let expr;
        (tok_seq, expr) = self.parse_primary(tok_seq)?;

        if tok_seq.expect_punct(PunctKind::OpenParenthesis).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenParenthesis)
                .ok_or(ParseError {})?;
            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseParenthesis)
                .ok_or(ParseError {})?;

            let func_type = expr.expr_type.get_fn_type_node().unwrap();

            Ok((
                tok_seq,
                ASTExpr::new(ASTExprNode::FuncCall(expr), func_type.return_type),
            ))
        } else {
            Ok((tok_seq, expr))
        }
    }

    fn parse_primary(&self, mut tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
        if tok_seq.is_empty() {
            return Err(ParseError {});
        }

        if let TokenKind::TokenNumber(num) = tok_seq.get_token() {
            Ok((
                tok_seq.next(),
                ASTExpr::new(ASTExprNode::Number(num), Type::new(TypeNode::Int)),
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
            let expr_type = (*obj).borrow().obj_type.clone();
            Ok((
                tok_seq.next(),
                ASTExpr::new(ASTExprNode::Var(obj), expr_type),
            ))
        } else {
            Err(ParseError {})
        }
    }
}
