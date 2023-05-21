use crate::ast::ASTExpr;
use crate::ast::ASTExprNode;
use crate::ast::ASTGlobal;
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
pub enum ParseError {
    SyntaxError,
    SemanticError,
}

pub fn parse_all(mut tok_seq: TokenList) -> Result<Vec<ASTGlobal>, ParseError> {
    let mut ret = Vec::new();
    let mut arena = ParseArena {
        objs: HashMap::new(),
    };

    while !tok_seq.is_empty() {
        let node;
        (tok_seq, node) = arena.parse_global(tok_seq)?;

        ret.push(node);
    }

    Ok(ret)
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
    fn insert_obj(&mut self, obj_name: &str, obj_type: Type) -> Result<Rc<RefCell<Obj>>, ()> {
        if self.objs.contains_key(obj_name) {
            return Err(());
        }

        let obj_id = self.objs.len();
        let obj = Rc::new(RefCell::new(Obj {
            id: obj_id,
            name: String::from(obj_name),
            obj_type,
        }));

        self.objs.insert(String::from(obj_name), obj.clone());
        Ok(obj)
    }

    fn get_obj(&self, obj_name: &str) -> Result<Rc<RefCell<Obj>>, ()> {
        if !self.objs.contains_key(obj_name) {
            return Err(());
        }

        Ok(self.objs.get(obj_name).unwrap().clone())
    }

    fn parse_declarator(
        mut tok_seq: TokenList,
        decl_spec_type: Type,
    ) -> Result<(TokenList, String, Type), ParseError> {
        let mut ret_type = decl_spec_type;

        while tok_seq.expect_punct(PunctKind::Asterisk).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::Asterisk)
                .ok_or(ParseError::SyntaxError)?;
            ret_type = Type::new_ptr_type(ret_type);
        }

        let var_name = match tok_seq.get_token() {
            TokenKind::TokenIdent(var_name) => var_name,
            _ => return Err(ParseError::SyntaxError),
        };

        tok_seq = tok_seq.next();

        if tok_seq.expect_punct(PunctKind::OpenSquareBracket).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenSquareBracket)
                .ok_or(ParseError::SyntaxError)?;

            let len = match tok_seq.get_token() {
                TokenKind::TokenNumber(len) => len,
                _ => return Err(ParseError::SyntaxError),
            };

            tok_seq = tok_seq.next();

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseSquareBracket)
                .ok_or(ParseError::SyntaxError)?;

            Ok((
                tok_seq,
                var_name,
                Type::new_array_type(ret_type, len as u32),
            ))
        } else if tok_seq.expect_punct(PunctKind::OpenParenthesis).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenParenthesis)
                .ok_or(ParseError::SyntaxError)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseParenthesis)
                .ok_or(ParseError::SyntaxError)?;

            Ok((
                tok_seq,
                var_name,
                Type::new_fn_type(FunctionTypeNode {
                    return_type: ret_type,
                }),
            ))
        } else {
            Ok((tok_seq, var_name, ret_type))
        }
    }

    fn parse_global(
        &mut self,
        mut tok_seq: TokenList,
    ) -> Result<(TokenList, ASTGlobal), ParseError> {
        tok_seq = tok_seq
            .expect_keyword(KeywordKind::Int)
            .ok_or(ParseError::SyntaxError)?;

        let obj_type;
        let obj_name;

        (tok_seq, obj_name, obj_type) =
            ParseArena::parse_declarator(tok_seq, Type::new(TypeNode::Int))?;

        let obj = self
            .insert_obj(&obj_name, obj_type)
            .map_err(|()| ParseError::SemanticError)?;
        let mut stmts = Vec::new();

        tok_seq = tok_seq
            .expect_punct(PunctKind::OpenBrace)
            .ok_or(ParseError::SyntaxError)?;

        while tok_seq.expect_punct(PunctKind::CloseBrace).is_none() {
            let stmt;
            (tok_seq, stmt) = self.parse_stmt(tok_seq)?;
            stmts.push(stmt);
        }

        tok_seq = tok_seq
            .expect_punct(PunctKind::CloseBrace)
            .ok_or(ParseError::SyntaxError)?;

        Ok((tok_seq, ASTGlobal::Function(obj, stmts)))
    }

    fn parse_stmt(&mut self, mut tok_seq: TokenList) -> Result<(TokenList, ASTStmt), ParseError> {
        if tok_seq.expect_keyword(KeywordKind::Return).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::Return)
                .ok_or(ParseError::SyntaxError)?;

            let expr;
            (tok_seq, expr) = self.parse_expr(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError::SyntaxError)?;

            Ok((tok_seq, ASTStmt::new(ASTStmtNode::Return(expr))))
        } else if tok_seq.expect_keyword(KeywordKind::Int).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::Int)
                .ok_or(ParseError::SyntaxError)?;

            let var_name;
            let var_type;
            (tok_seq, var_name, var_type) =
                ParseArena::parse_declarator(tok_seq, Type::new(TypeNode::Int))?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError::SyntaxError)?;

            let obj = self
                .insert_obj(&var_name, var_type)
                .map_err(|()| ParseError::SemanticError)?;
            Ok((tok_seq, ASTStmt::new(ASTStmtNode::Declaration(obj))))
        } else if tok_seq.expect_keyword(KeywordKind::If).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::If)
                .ok_or(ParseError::SyntaxError)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenParenthesis)
                .ok_or(ParseError::SyntaxError)?;

            let cond;
            (tok_seq, cond) = self.parse_expr(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseParenthesis)
                .ok_or(ParseError::SyntaxError)?;

            let if_stmt;
            (tok_seq, if_stmt) = self.parse_stmt(tok_seq)?;

            if tok_seq.expect_keyword(KeywordKind::Else).is_some() {
                tok_seq = tok_seq
                    .expect_keyword(KeywordKind::Else)
                    .ok_or(ParseError::SyntaxError)?;

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
                .ok_or(ParseError::SyntaxError)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenParenthesis)
                .ok_or(ParseError::SyntaxError)?;

            let cond;
            (tok_seq, cond) = self.parse_expr(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseParenthesis)
                .ok_or(ParseError::SyntaxError)?;

            let stmt;
            (tok_seq, stmt) = self.parse_stmt(tok_seq)?;
            Ok((tok_seq, ASTStmt::new(ASTStmtNode::While(cond, stmt))))
        } else if tok_seq.expect_keyword(KeywordKind::For).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::For)
                .ok_or(ParseError::SyntaxError)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenParenthesis)
                .ok_or(ParseError::SyntaxError)?;

            let start;
            (tok_seq, start) = self.parse_expr(tok_seq)?;
            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError::SyntaxError)?;

            let cond;
            (tok_seq, cond) = self.parse_expr(tok_seq)?;
            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError::SyntaxError)?;

            let step;
            (tok_seq, step) = self.parse_expr(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseParenthesis)
                .ok_or(ParseError::SyntaxError)?;

            let stmt;
            (tok_seq, stmt) = self.parse_stmt(tok_seq)?;
            Ok((
                tok_seq,
                ASTStmt::new(ASTStmtNode::For(start, cond, step, stmt)),
            ))
        } else if tok_seq.expect_punct(PunctKind::OpenBrace).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenBrace)
                .ok_or(ParseError::SyntaxError)?;

            let mut stmts = Vec::new();

            while tok_seq.expect_punct(PunctKind::CloseBrace).is_none() {
                let stmt;
                (tok_seq, stmt) = self.parse_stmt(tok_seq)?;
                stmts.push(stmt);
            }

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseBrace)
                .ok_or(ParseError::SyntaxError)?;

            Ok((tok_seq, ASTStmt::new(ASTStmtNode::Block(stmts))))
        } else {
            let expr;
            (tok_seq, expr) = self.parse_expr(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError::SyntaxError)?;

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

                let lhs_type = lhs.expr_type.clone();
                let rhs_type = rhs.expr_type.clone();

                if !lhs_type.is_int_type() || !rhs_type.is_int_type() {
                    return Err(ParseError::SemanticError);
                }

                lhs = ASTExpr::new(
                    ASTExprNode::BinaryOp(BinaryOpNode { lhs, rhs, kind }),
                    lhs_type,
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

                let lhs_type = lhs.expr_type.clone();
                let rhs_type = rhs.expr_type.clone();

                if !lhs_type.is_int_type() || !rhs_type.is_int_type() {
                    return Err(ParseError::SemanticError);
                }

                lhs = ASTExpr::new(
                    ASTExprNode::BinaryOp(BinaryOpNode { lhs, rhs, kind }),
                    lhs_type,
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

                let lhs_type = lhs.expr_type.clone();
                let rhs_type = rhs.expr_type.clone();

                if lhs_type.is_int_type() && rhs_type.is_int_type() {
                    lhs = ASTExpr::new(
                        ASTExprNode::BinaryOp(BinaryOpNode { lhs, rhs, kind }),
                        lhs_type,
                    );
                } else if lhs_type.is_ptr_type() && rhs_type.is_int_type() {
                    lhs = ASTExpr::new(
                        ASTExprNode::BinaryOp(BinaryOpNode { lhs, rhs, kind }),
                        lhs_type,
                    );
                } else if lhs_type.is_int_type() && rhs_type.is_ptr_type() {
                    lhs = ASTExpr::new(
                        ASTExprNode::BinaryOp(BinaryOpNode { lhs, rhs, kind }),
                        rhs_type,
                    );
                } else {
                    return Err(ParseError::SemanticError);
                }
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
                let lhs_type = lhs.expr_type.clone();
                let rhs_type = rhs.expr_type.clone();

                if !lhs_type.is_int_type() || !rhs_type.is_int_type() {
                    return Err(ParseError::SemanticError);
                }

                lhs = ASTExpr::new(
                    ASTExprNode::BinaryOp(BinaryOpNode { lhs, rhs, kind }),
                    lhs_type,
                );
            } else {
                break;
            }
        }

        Ok((tok_seq, lhs))
    }

    fn parse_unary(&self, mut tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
        if tok_seq.expect_keyword(KeywordKind::Sizeof).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::Sizeof)
                .ok_or(ParseError::SyntaxError)?;

            let expr;
            (tok_seq, expr) = self.parse_unary(tok_seq)?;
            let expr_type = expr.expr_type.clone();

            let node = ASTExpr::new(
                ASTExprNode::UnaryOp(UnaryOpNode {
                    expr,
                    kind: UnaryOpKind::Sizeof,
                }),
                expr_type,
            );

            Ok((tok_seq, node))
        } else if tok_seq.expect_keyword(KeywordKind::Alignof).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::Alignof)
                .ok_or(ParseError::SyntaxError)?;

            let expr;
            (tok_seq, expr) = self.parse_unary(tok_seq)?;
            let expr_type = expr.expr_type.clone();

            let node = ASTExpr::new(
                ASTExprNode::UnaryOp(UnaryOpNode {
                    expr,
                    kind: UnaryOpKind::Alignof,
                }),
                expr_type,
            );

            Ok((tok_seq, node))
        } else if tok_seq.expect_punct(PunctKind::Plus).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::Plus)
                .ok_or(ParseError::SyntaxError)?;
            let expr;
            (tok_seq, expr) = self.parse_unary(tok_seq)?;
            let expr_type = expr.expr_type.clone();
            if !expr_type.is_int_type() {
                return Err(ParseError::SemanticError);
            }

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
                .ok_or(ParseError::SyntaxError)?;
            let expr;
            (tok_seq, expr) = self.parse_unary(tok_seq)?;
            let expr_type = expr.expr_type.clone();
            if !expr_type.is_int_type() {
                return Err(ParseError::SemanticError);
            }

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
                .ok_or(ParseError::SyntaxError)?;

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
                .ok_or(ParseError::SyntaxError)?;

            let expr;
            (tok_seq, expr) = self.parse_unary(tok_seq)?;
            let expr_type = expr
                .expr_type
                .clone()
                .get_ptr_to()
                .map_err(|()| ParseError::SemanticError)?;

            let node = ASTExpr::new(
                ASTExprNode::UnaryOp(UnaryOpNode {
                    expr,
                    kind: UnaryOpKind::Deref,
                }),
                expr_type,
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
                .ok_or(ParseError::SyntaxError)?;
            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseParenthesis)
                .ok_or(ParseError::SyntaxError)?;

            let func_type = expr
                .expr_type
                .get_fn_type_node()
                .map_err(|()| ParseError::SemanticError)?;

            Ok((
                tok_seq,
                ASTExpr::new(ASTExprNode::FuncCall(expr), func_type.return_type),
            ))
        } else if tok_seq.expect_punct(PunctKind::OpenSquareBracket).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenSquareBracket)
                .ok_or(ParseError::SyntaxError)?;

            let index;
            (tok_seq, index) = self.parse_expr(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseSquareBracket)
                .ok_or(ParseError::SyntaxError)?;

            let cast_type = if expr.expr_type.is_ptr_type() {
                expr.expr_type.clone()
            } else if expr.expr_type.is_array_type() {
                Type::new_ptr_type(expr.expr_type.get_array_to().unwrap())
            } else {
                return Err(ParseError::SemanticError);
            };

            let cast = ASTExpr::build_cast_node(cast_type, expr);

            let ptr_type = if cast.expr_type.is_ptr_type() && index.expr_type.is_int_type() {
                cast.expr_type.clone()
            } else if cast.expr_type.is_int_type() && index.expr_type.is_ptr_type() {
                index.expr_type.clone()
            } else {
                return Err(ParseError::SemanticError);
            };

            let plus = ASTExpr::new(
                ASTExprNode::BinaryOp(BinaryOpNode {
                    kind: BinaryOpKind::Add,
                    lhs: cast,
                    rhs: index,
                }),
                ptr_type.clone(),
            );

            Ok((
                tok_seq,
                ASTExpr::new(
                    ASTExprNode::UnaryOp(UnaryOpNode {
                        kind: UnaryOpKind::Deref,
                        expr: plus,
                    }),
                    ptr_type.get_ptr_to().unwrap(),
                ),
            ))
        } else {
            Ok((tok_seq, expr))
        }
    }

    fn parse_primary(&self, mut tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
        if tok_seq.is_empty() {
            return Err(ParseError::SyntaxError);
        }

        if let TokenKind::TokenNumber(num) = tok_seq.get_token() {
            Ok((
                tok_seq.next(),
                ASTExpr::new(ASTExprNode::Number(num), Type::new(TypeNode::Int)),
            ))
        } else if tok_seq.expect_punct(PunctKind::OpenParenthesis).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenParenthesis)
                .ok_or(ParseError::SyntaxError)?;
            let ret;
            (tok_seq, ret) = self.parse_expr(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseParenthesis)
                .ok_or(ParseError::SyntaxError)?;

            Ok((tok_seq, ret))
        } else if let TokenKind::TokenIdent(ref var_name) = tok_seq.get_token() {
            let obj = self
                .get_obj(var_name)
                .map_err(|()| ParseError::SemanticError)?;
            let expr_type = (*obj).borrow().obj_type.clone();
            Ok((
                tok_seq.next(),
                ASTExpr::new(ASTExprNode::Var(obj), expr_type),
            ))
        } else {
            Err(ParseError::SyntaxError)
        }
    }
}
