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
    let mut arena = ParseArena::new();

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

struct ParseArena {
    obj_id: usize,
    global_objs: HashMap<String, Rc<RefCell<Obj>>>,
    local_objs: HashMap<String, Rc<RefCell<Obj>>>,
    return_type: Option<Type>,
}

impl ParseArena {
    pub fn new() -> ParseArena {
        ParseArena {
            obj_id: 0,
            global_objs: HashMap::new(),
            local_objs: HashMap::new(),
            return_type: None,
        }
    }

    fn insert_global_obj(
        &mut self,
        obj_name: &str,
        obj_type: Type,
    ) -> Result<Rc<RefCell<Obj>>, ()> {
        if self.global_objs.contains_key(obj_name) {
            return Err(());
        }

        let obj = Rc::new(RefCell::new(Obj {
            id: self.obj_id,
            name: String::from(obj_name),
            obj_type,
        }));

        self.global_objs.insert(String::from(obj_name), obj.clone());
        self.obj_id += 1;

        Ok(obj)
    }

    fn insert_local_obj(&mut self, obj_name: &str, obj_type: Type) -> Result<Rc<RefCell<Obj>>, ()> {
        if self.local_objs.contains_key(obj_name) {
            return Err(());
        }

        let obj = Rc::new(RefCell::new(Obj {
            id: self.obj_id,
            name: String::from(obj_name),
            obj_type,
        }));

        self.local_objs.insert(String::from(obj_name), obj.clone());
        self.obj_id += 1;

        Ok(obj)
    }

    fn get_obj(&self, obj_name: &str) -> Result<Rc<RefCell<Obj>>, ()> {
        if self.local_objs.contains_key(obj_name) {
            return Ok(self.local_objs.get(obj_name).unwrap().clone());
        }

        if self.global_objs.contains_key(obj_name) {
            return Ok(self.global_objs.get(obj_name).unwrap().clone());
        }

        Err(())
    }

    fn is_type_token(&self, tok_seq: TokenList) -> bool {
        let TokenKind::Keyword(keyword) = tok_seq.get_token() else {return false;};
        matches!(keyword, KeywordKind::Int) || matches!(keyword, KeywordKind::Char)
    }

    fn parse_decl_spec(&self, tok_seq: TokenList) -> Result<(TokenList, Type), ParseError> {
        let TokenKind::Keyword(keyword) = tok_seq.get_token() else {return Err(ParseError::SyntaxError);};

        match keyword {
            KeywordKind::Int => Ok((tok_seq.next(), Type::new(TypeNode::Int))),
            KeywordKind::Char => Ok((tok_seq.next(), Type::new(TypeNode::Char))),
            _ => Err(ParseError::SyntaxError),
        }
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
            TokenKind::Ident(var_name) => var_name,
            _ => return Err(ParseError::SyntaxError),
        };

        tok_seq = tok_seq.next();

        if tok_seq.expect_punct(PunctKind::OpenSquareBracket).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenSquareBracket)
                .ok_or(ParseError::SyntaxError)?;

            let len = match tok_seq.get_token() {
                TokenKind::Number(len) => len,
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
        let decl_type;
        (tok_seq, decl_type) = self.parse_decl_spec(tok_seq)?;

        let obj_type;
        let obj_name;

        (tok_seq, obj_name, obj_type) = ParseArena::parse_declarator(tok_seq, decl_type)?;

        if tok_seq.expect_punct(PunctKind::OpenBrace).is_some() {
            let TypeNode::Func(func_node) = obj_type.get_node() else {return Err(ParseError::SemanticError);};
            self.return_type = Some(func_node.return_type);

            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenBrace)
                .ok_or(ParseError::SyntaxError)?;

            let obj = self
                .insert_global_obj(&obj_name, obj_type)
                .map_err(|()| ParseError::SemanticError)?;

            // prepare parsing stmts
            let mut stmts = Vec::new();
            self.local_objs = HashMap::new();

            while tok_seq.expect_punct(PunctKind::CloseBrace).is_none() {
                let stmt;
                (tok_seq, stmt) = self.parse_stmt(tok_seq)?;
                stmts.push(stmt);
            }

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseBrace)
                .ok_or(ParseError::SyntaxError)?;

            self.return_type = None;

            Ok((tok_seq, ASTGlobal::Function(obj, stmts)))
        } else if tok_seq.expect_punct(PunctKind::SemiColon).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError::SyntaxError)?;

            let obj = self
                .insert_global_obj(&obj_name, obj_type)
                .map_err(|()| ParseError::SemanticError)?;

            Ok((tok_seq, ASTGlobal::Variable(obj)))
        } else {
            Err(ParseError::SyntaxError)
        }
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

            let return_type = self.return_type.clone().unwrap();

            let cast = ASTExpr::build_cast_node(return_type, expr);

            Ok((tok_seq, ASTStmt::new(ASTStmtNode::Return(cast))))
        } else if self.is_type_token(tok_seq.clone()) {
            let decl_type;
            (tok_seq, decl_type) = self.parse_decl_spec(tok_seq)?;

            let var_name;
            let var_type;
            (tok_seq, var_name, var_type) = ParseArena::parse_declarator(tok_seq, decl_type)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError::SyntaxError)?;

            let obj = self
                .insert_local_obj(&var_name, var_type)
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

    fn parse_expr(&self, mut tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
        let mut lhs;
        (tok_seq, lhs) = self.parse_assign(tok_seq)?;

        while !tok_seq.is_empty() {
            if let TokenKind::Punct(punct) = tok_seq.get_token() {
                let kind = match punct {
                    PunctKind::Comma => BinaryOpKind::Comma,
                    _ => break,
                };

                tok_seq = tok_seq.next();

                let rhs;
                (tok_seq, rhs) = self.parse_assign(tok_seq)?;

                let rhs_type = rhs.expr_type.clone();

                lhs = ASTExpr::new(
                    ASTExprNode::BinaryOp(BinaryOpNode { lhs, rhs, kind }),
                    rhs_type,
                );
            } else {
                break;
            }
        }

        Ok((tok_seq, lhs))
    }

    fn parse_assign(&self, mut tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
        let lhs;
        (tok_seq, lhs) = self.parse_conditional(tok_seq)?;

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

    fn parse_conditional(
        &self,
        mut tok_seq: TokenList,
    ) -> Result<(TokenList, ASTExpr), ParseError> {
        let cond;
        (tok_seq, cond) = self.parse_logical_or(tok_seq)?;

        if tok_seq.expect_punct(PunctKind::Question).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::Question)
                .ok_or(ParseError::SyntaxError)?;

            let then_expr;
            let else_expr;
            (tok_seq, then_expr) = self.parse_logical_or(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::Colon)
                .ok_or(ParseError::SyntaxError)?;

            (tok_seq, else_expr) = self.parse_logical_or(tok_seq)?;

            let expr_type = then_expr.expr_type.clone();

            Ok((
                tok_seq,
                ASTExpr::new(
                    ASTExprNode::Conditional(cond, then_expr, else_expr),
                    expr_type,
                ),
            ))
        } else {
            Ok((tok_seq, cond))
        }
    }

    fn parse_logical_or(&self, mut tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
        let mut lhs;
        (tok_seq, lhs) = self.parse_logical_and(tok_seq)?;

        while !tok_seq.is_empty() {
            if let TokenKind::Punct(punct) = tok_seq.get_token() {
                let kind = match punct {
                    PunctKind::LogicalOr => BinaryOpKind::LogicalOr,
                    _ => break,
                };

                tok_seq = tok_seq.next();

                let rhs;
                (tok_seq, rhs) = self.parse_logical_and(tok_seq)?;

                let lhs_type = lhs.expr_type.clone();
                let rhs_type = rhs.expr_type.clone();

                if !lhs_type.is_int_type() || !rhs_type.is_int_type() {
                    return Err(ParseError::SemanticError);
                }

                lhs = ASTExpr::new(
                    ASTExprNode::BinaryOp(BinaryOpNode { lhs, rhs, kind }),
                    Type::new(TypeNode::Int),
                );
            } else {
                break;
            }
        }

        Ok((tok_seq, lhs))
    }

    fn parse_logical_and(&self, tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
        self.parse_bit_or(tok_seq)
    }

    fn parse_bit_or(&self, mut tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
        let mut lhs;
        (tok_seq, lhs) = self.parse_bit_xor(tok_seq)?;

        while !tok_seq.is_empty() {
            if let TokenKind::Punct(punct) = tok_seq.get_token() {
                let kind = match punct {
                    PunctKind::BitOr => BinaryOpKind::BitOr,
                    _ => break,
                };

                tok_seq = tok_seq.next();

                let rhs;
                (tok_seq, rhs) = self.parse_bit_xor(tok_seq)?;

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
    fn parse_bit_xor(&self, mut tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
        let mut lhs;
        (tok_seq, lhs) = self.parse_bit_and(tok_seq)?;

        while !tok_seq.is_empty() {
            if let TokenKind::Punct(punct) = tok_seq.get_token() {
                let kind = match punct {
                    PunctKind::Caret => BinaryOpKind::BitXor,
                    _ => break,
                };

                tok_seq = tok_seq.next();

                let rhs;
                (tok_seq, rhs) = self.parse_bit_and(tok_seq)?;

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

    fn parse_bit_and(&self, mut tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
        let mut lhs;
        (tok_seq, lhs) = self.parse_equality(tok_seq)?;

        while !tok_seq.is_empty() {
            if let TokenKind::Punct(punct) = tok_seq.get_token() {
                let kind = match punct {
                    PunctKind::Ampersand => BinaryOpKind::BitAnd,
                    _ => break,
                };

                tok_seq = tok_seq.next();

                let rhs;
                (tok_seq, rhs) = self.parse_equality(tok_seq)?;

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

    fn parse_equality(&self, mut tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
        let mut lhs;
        (tok_seq, lhs) = self.parse_relational(tok_seq)?;

        while !tok_seq.is_empty() {
            if let TokenKind::Punct(punct) = tok_seq.get_token() {
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
        (tok_seq, lhs) = self.parse_shift(tok_seq)?;

        while !tok_seq.is_empty() {
            if let TokenKind::Punct(punct) = tok_seq.get_token() {
                let kind = match punct {
                    PunctKind::Less => BinaryOpKind::Less,
                    PunctKind::LessEqual => BinaryOpKind::LessEqual,
                    PunctKind::Greater => BinaryOpKind::Greater,
                    PunctKind::GreaterEqual => BinaryOpKind::GreaterEqual,
                    _ => break,
                };

                tok_seq = tok_seq.next();

                let rhs;
                (tok_seq, rhs) = self.parse_shift(tok_seq)?;

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

    fn parse_shift(&self, mut tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
        let mut lhs;
        (tok_seq, lhs) = self.parse_add(tok_seq)?;

        while !tok_seq.is_empty() {
            if let TokenKind::Punct(punct) = tok_seq.get_token() {
                let kind = match punct {
                    PunctKind::LeftShift => BinaryOpKind::LeftShift,
                    PunctKind::RightShift => BinaryOpKind::RightShift,
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
            if let TokenKind::Punct(punct) = tok_seq.get_token() {
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
        (tok_seq, lhs) = self.parse_cast(tok_seq)?;

        while !tok_seq.is_empty() {
            if let TokenKind::Punct(punct) = tok_seq.get_token() {
                let kind = match punct {
                    PunctKind::Asterisk => BinaryOpKind::Mul,
                    PunctKind::Slash => BinaryOpKind::Div,
                    PunctKind::Percent => BinaryOpKind::Mod,
                    _ => break,
                };

                tok_seq = tok_seq.next();

                let rhs;
                (tok_seq, rhs) = self.parse_cast(tok_seq)?;
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

    fn parse_cast(&self, tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
        self.parse_unary(tok_seq)
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
        } else if tok_seq.expect_punct(PunctKind::Exclamation).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::Exclamation)
                .ok_or(ParseError::SyntaxError)?;

            let expr;
            (tok_seq, expr) = self.parse_unary(tok_seq)?;

            let node = ASTExpr::new(
                ASTExprNode::UnaryOp(UnaryOpNode {
                    expr,
                    kind: UnaryOpKind::LogicalNot,
                }),
                Type::new(TypeNode::Int),
            );

            Ok((tok_seq, node))
        } else if tok_seq.expect_punct(PunctKind::Tilde).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::Tilde)
                .ok_or(ParseError::SyntaxError)?;

            let expr;
            (tok_seq, expr) = self.parse_unary(tok_seq)?;
            let expr_type = expr.expr_type.clone();

            let node = ASTExpr::new(
                ASTExprNode::UnaryOp(UnaryOpNode {
                    expr,
                    kind: UnaryOpKind::BitNot,
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

        if let TokenKind::Number(num) = tok_seq.get_token() {
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
        } else if let TokenKind::Ident(ref var_name) = tok_seq.get_token() {
            let obj = self
                .get_obj(var_name)
                .map_err(|()| ParseError::SemanticError)?;
            let expr_type = (*obj).borrow().obj_type.clone();
            Ok((
                tok_seq.next(),
                ASTExpr::new(ASTExprNode::Var(obj), expr_type),
            ))
        } else if let TokenKind::StrLiteral(ref text) = tok_seq.get_token() {
            Ok((
                tok_seq.next(),
                ASTExpr::new(
                    ASTExprNode::StrLiteral(text.clone()),
                    Type::new_ptr_type(Type::new(TypeNode::Char)),
                ),
            ))
        } else {
            Err(ParseError::SyntaxError)
        }
    }
}
