mod parse_decl;

use crate::ast::ASTExpr;
use crate::ast::ASTExprNode;
use crate::ast::ASTGlobal;
use crate::ast::ASTStmt;
use crate::ast::ASTStmtNode;
use crate::ast::AssignKind;
use crate::ast::AssignNode;
use crate::ast::BinaryOpKind;
use crate::ast::BinaryOpNode;
use crate::ast::UnaryOpKind;
use crate::ast::UnaryOpNode;
use crate::error::ParseError;
use crate::obj::Obj;
use crate::obj::ObjNode;
use crate::tokenize::KeywordKind;
use crate::tokenize::PunctKind;
use crate::tokenize::TokenKind;
use crate::tokenize::TokenList;
use crate::types::Type;
use crate::types::TypeNode;

use std::collections::HashMap;
use std::collections::VecDeque;

pub fn parse_all(mut tok_seq: TokenList) -> Result<Vec<ASTGlobal>, ParseError> {
    let mut ret = Vec::new();
    let mut arena = ParseArena::new();

    while !tok_seq.is_empty() {
        let node;
        (tok_seq, node) = arena.parse_global(tok_seq)?;
        if node.is_some() {
            ret.push(node.unwrap());
        }
    }

    Ok(ret)
}

struct ParseArena {
    obj_id: usize,
    global_objs: HashMap<String, Obj>,
    local_objs: VecDeque<HashMap<String, Obj>>,
    return_type: Option<Type>,

    stmt_id: usize,
    break_stack: VecDeque<usize>,
    continue_stack: VecDeque<usize>,
}

impl ParseArena {
    pub fn new() -> ParseArena {
        ParseArena {
            obj_id: 0,
            global_objs: HashMap::new(),
            local_objs: VecDeque::new(),
            return_type: None,
            stmt_id: 0,
            break_stack: VecDeque::new(),
            continue_stack: VecDeque::new(),
        }
    }

    fn insert_global_obj(&mut self, obj_name: &str, obj_type: Type) -> Result<Obj, ()> {
        if self.global_objs.contains_key(obj_name) {
            return Err(());
        }

        let obj = Obj::new(ObjNode {
            id: self.obj_id,
            name: String::from(obj_name),
            obj_type,
        });

        self.global_objs.insert(String::from(obj_name), obj.clone());
        self.obj_id += 1;

        Ok(obj)
    }

    fn insert_local_obj(&mut self, obj_name: &str, obj_type: Type) -> Result<Obj, ()> {
        if self.local_objs.back_mut().unwrap().contains_key(obj_name) {
            return Err(());
        }

        let obj = Obj::new(ObjNode {
            id: self.obj_id,
            name: String::from(obj_name),
            obj_type,
        });

        self.local_objs
            .back_mut()
            .unwrap()
            .insert(String::from(obj_name), obj.clone());
        self.obj_id += 1;

        Ok(obj)
    }

    fn initialize_local_scope(&mut self) {
        self.local_objs = VecDeque::new();
        self.push_local_scope();
    }

    fn push_local_scope(&mut self) {
        self.local_objs.push_back(HashMap::new());
    }

    fn pop_local_scope(&mut self) {
        self.local_objs.pop_back();
    }

    fn search_obj(&self, obj_name: &str) -> Option<Obj> {
        for map in self.local_objs.iter().rev() {
            if map.contains_key(obj_name) {
                return Some(map.get(obj_name).unwrap().clone());
            }
        }

        if self.global_objs.contains_key(obj_name) {
            return Some(self.global_objs.get(obj_name).unwrap().clone());
        }

        None
    }

    fn is_type_token(&self, tok_seq: TokenList) -> bool {
        let TokenKind::Keyword(keyword) = tok_seq.get_token() else {return false;};
        matches!(keyword, KeywordKind::Int) || matches!(keyword, KeywordKind::Char)
    }

    fn parse_decl_spec(&self, tok_seq: TokenList) -> Result<(TokenList, Type), ParseError> {
        let TokenKind::Keyword(keyword) = tok_seq.get_token() else {return Err(ParseError::SyntaxError(tok_seq));};

        match keyword {
            KeywordKind::Int => Ok((tok_seq.next(), Type::new(TypeNode::Int))),
            KeywordKind::Char => Ok((tok_seq.next(), Type::new(TypeNode::Char))),
            _ => Err(ParseError::SyntaxError(tok_seq)),
        }
    }

    fn parse_global(
        &mut self,
        mut tok_seq: TokenList,
    ) -> Result<(TokenList, Option<ASTGlobal>), ParseError> {
        let decl_type;
        (tok_seq, decl_type) = self.parse_decl_spec(tok_seq)?;

        if tok_seq.expect_punct(PunctKind::SemiColon).is_some() {
            tok_seq = tok_seq.next();

            return Ok((tok_seq, None));
        }

        let declarator;

        (tok_seq, declarator) = self.parse_declarator(tok_seq)?;
        let obj_name = declarator.get_name();
        let obj_type = declarator.get_type(decl_type);

        if tok_seq.expect_punct(PunctKind::OpenBrace).is_some() {
            let TypeNode::Func(return_type,_) = obj_type.get_node() else {return Err(ParseError::SemanticError(tok_seq));};
            self.return_type = Some(return_type);

            let obj = self
                .insert_global_obj(&obj_name, obj_type)
                .map_err(|()| ParseError::SemanticError(tok_seq.clone()))?;

            // prepare parsing stmts
            let mut args = Vec::new();
            let mut stmts = Vec::new();
            self.initialize_local_scope();

            let declarator_args = declarator.get_args();

            if declarator_args.is_some() {
                for (ref ty, ref decl) in declarator_args.unwrap() {
                    let arg_name = decl.get_name();
                    let arg_type = decl.get_type(ty.clone());

                    let arg_type = if arg_type.is_array_type() {
                        Type::new_ptr_type(arg_type.get_array_to().unwrap())
                    } else {
                        arg_type
                    };

                    let arg_obj = self
                        .insert_local_obj(&arg_name, arg_type)
                        .map_err(|()| ParseError::SemanticError(tok_seq.clone()))?;

                    args.push(arg_obj);
                }
            }

            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenBrace)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            while tok_seq.expect_punct(PunctKind::CloseBrace).is_none() {
                let stmt;
                (tok_seq, stmt) = self.parse_stmt(tok_seq)?;
                stmts.push(stmt);
            }

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseBrace)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            self.return_type = None;

            Ok((tok_seq, Some(ASTGlobal::Function(obj, args, stmts))))
        } else if tok_seq.expect_punct(PunctKind::SemiColon).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let obj = self
                .insert_global_obj(&obj_name, obj_type)
                .map_err(|()| ParseError::SemanticError(tok_seq.clone()))?;

            Ok((tok_seq, Some(ASTGlobal::Variable(obj))))
        } else {
            Err(ParseError::SyntaxError(tok_seq))
        }
    }

    fn parse_stmt(&mut self, mut tok_seq: TokenList) -> Result<(TokenList, ASTStmt), ParseError> {
        if tok_seq.expect_keyword(KeywordKind::Return).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::Return)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let expr;
            (tok_seq, expr) = self.parse_expr(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let return_type = self.return_type.clone().unwrap();

            let cast = expr.cast_to(&return_type);

            Ok((tok_seq, ASTStmt::new(ASTStmtNode::Return(cast))))
        } else if tok_seq.expect_keyword(KeywordKind::Break).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::Break)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let stmt_id = self
                .break_stack
                .back()
                .ok_or(ParseError::SemanticError(tok_seq.clone()))?;

            Ok((tok_seq, ASTStmt::new(ASTStmtNode::Break(stmt_id.clone()))))
        } else if tok_seq.expect_keyword(KeywordKind::Continue).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::Continue)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let stmt_id = self
                .continue_stack
                .back()
                .ok_or(ParseError::SemanticError(tok_seq.clone()))?;

            Ok((
                tok_seq,
                ASTStmt::new(ASTStmtNode::Continue(stmt_id.clone())),
            ))
        } else if self.is_type_token(tok_seq.clone()) {
            let decl_type;
            (tok_seq, decl_type) = self.parse_decl_spec(tok_seq)?;

            let declarator;
            (tok_seq, declarator) = self.parse_declarator(tok_seq)?;
            let var_name = declarator.get_name();
            let var_type = declarator.get_type(decl_type);

            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let obj = self
                .insert_local_obj(&var_name, var_type)
                .map_err(|()| ParseError::SemanticError(tok_seq.clone()))?;
            Ok((tok_seq, ASTStmt::new(ASTStmtNode::Declaration(obj))))
        } else if tok_seq.expect_keyword(KeywordKind::If).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::If)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenParenthesis)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let cond;
            (tok_seq, cond) = self.parse_expr(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseParenthesis)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let if_stmt;
            (tok_seq, if_stmt) = self.parse_stmt(tok_seq)?;

            if tok_seq.expect_keyword(KeywordKind::Else).is_some() {
                tok_seq = tok_seq
                    .expect_keyword(KeywordKind::Else)
                    .ok_or(ParseError::SyntaxError(tok_seq))?;

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
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenParenthesis)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let cond;
            (tok_seq, cond) = self.parse_expr(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseParenthesis)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            // push stmt_id
            let stmt_id = self.stmt_id;
            self.stmt_id += 1;

            self.break_stack.push_back(stmt_id);
            self.continue_stack.push_back(stmt_id);

            let stmt;
            (tok_seq, stmt) = self.parse_stmt(tok_seq)?;

            self.break_stack.pop_back();
            self.continue_stack.pop_back();

            Ok((
                tok_seq,
                ASTStmt::new(ASTStmtNode::While(cond, stmt, stmt_id)),
            ))
        } else if tok_seq.expect_keyword(KeywordKind::Do).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::Do)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            // push stmt_id
            let stmt_id = self.stmt_id;
            self.stmt_id += 1;

            self.break_stack.push_back(stmt_id);
            self.continue_stack.push_back(stmt_id);

            let stmt;
            (tok_seq, stmt) = self.parse_stmt(tok_seq)?;

            self.break_stack.pop_back();
            self.continue_stack.pop_back();

            tok_seq = tok_seq
                .expect_keyword(KeywordKind::While)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenParenthesis)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let cond;
            (tok_seq, cond) = self.parse_expr(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseParenthesis)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            Ok((
                tok_seq,
                ASTStmt::new(ASTStmtNode::DoWhile(cond, stmt, stmt_id)),
            ))
        } else if tok_seq.expect_keyword(KeywordKind::For).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::For)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenParenthesis)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let start = if tok_seq.expect_punct(PunctKind::SemiColon).is_some() {
                None
            } else {
                let tmp;
                (tok_seq, tmp) = self.parse_expr(tok_seq)?;
                Some(tmp)
            };

            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let cond = if tok_seq.expect_punct(PunctKind::SemiColon).is_some() {
                None
            } else {
                let tmp;
                (tok_seq, tmp) = self.parse_expr(tok_seq)?;
                Some(tmp)
            };

            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let step = if tok_seq.expect_punct(PunctKind::CloseParenthesis).is_some() {
                None
            } else {
                let tmp;
                (tok_seq, tmp) = self.parse_expr(tok_seq)?;
                Some(tmp)
            };

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseParenthesis)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            // push stmt_id
            let stmt_id = self.stmt_id;
            self.stmt_id += 1;

            self.break_stack.push_back(stmt_id);
            self.continue_stack.push_back(stmt_id);

            let stmt;
            (tok_seq, stmt) = self.parse_stmt(tok_seq)?;

            self.break_stack.pop_back();
            self.continue_stack.pop_back();

            Ok((
                tok_seq,
                ASTStmt::new(ASTStmtNode::For(start, cond, step, stmt, stmt_id)),
            ))
        } else if tok_seq.expect_punct(PunctKind::OpenBrace).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenBrace)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            self.push_local_scope();

            let mut stmts = Vec::new();

            while tok_seq.expect_punct(PunctKind::CloseBrace).is_none() {
                let stmt;
                (tok_seq, stmt) = self.parse_stmt(tok_seq)?;
                stmts.push(stmt);
            }

            self.pop_local_scope();

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseBrace)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            Ok((tok_seq, ASTStmt::new(ASTStmtNode::Block(stmts))))
        } else {
            let expr;
            (tok_seq, expr) = self.parse_expr(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

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
            let lhs_type = lhs.expr_type.clone();
            let rhs = rhs.cast_to(&lhs_type);

            Ok((
                tok_seq,
                ASTExpr::new(
                    ASTExprNode::Assign(AssignNode {
                        lhs,
                        rhs,
                        kind: AssignKind::Assign,
                    }),
                    lhs_type,
                ),
            ))
        } else if tok_seq.expect_punct(PunctKind::LeftShiftAssign).is_some() {
            tok_seq = tok_seq.next();

            let rhs;
            (tok_seq, rhs) = self.parse_assign(tok_seq)?;
            let expr_type = lhs.expr_type.clone();

            Ok((
                tok_seq,
                ASTExpr::new(
                    ASTExprNode::Assign(AssignNode {
                        lhs,
                        rhs,
                        kind: AssignKind::LeftShiftAssign,
                    }),
                    expr_type,
                ),
            ))
        } else if tok_seq.expect_punct(PunctKind::RightShiftAssign).is_some() {
            tok_seq = tok_seq.next();

            let rhs;
            (tok_seq, rhs) = self.parse_assign(tok_seq)?;
            let expr_type = lhs.expr_type.clone();

            Ok((
                tok_seq,
                ASTExpr::new(
                    ASTExprNode::Assign(AssignNode {
                        lhs,
                        rhs,
                        kind: AssignKind::RightShiftAssign,
                    }),
                    expr_type,
                ),
            ))
        } else if tok_seq.expect_punct(PunctKind::OrAssign).is_some() {
            tok_seq = tok_seq.next();

            let rhs;
            (tok_seq, rhs) = self.parse_assign(tok_seq)?;
            let expr_type = lhs.expr_type.clone();

            Ok((
                tok_seq,
                ASTExpr::new(
                    ASTExprNode::Assign(AssignNode {
                        lhs,
                        rhs,
                        kind: AssignKind::OrAssign,
                    }),
                    expr_type,
                ),
            ))
        } else if tok_seq.expect_punct(PunctKind::XorAssign).is_some() {
            tok_seq = tok_seq.next();

            let rhs;
            (tok_seq, rhs) = self.parse_assign(tok_seq)?;
            let expr_type = lhs.expr_type.clone();

            Ok((
                tok_seq,
                ASTExpr::new(
                    ASTExprNode::Assign(AssignNode {
                        lhs,
                        rhs,
                        kind: AssignKind::XorAssign,
                    }),
                    expr_type,
                ),
            ))
        } else if tok_seq.expect_punct(PunctKind::AndAssign).is_some() {
            tok_seq = tok_seq.next();

            let rhs;
            (tok_seq, rhs) = self.parse_assign(tok_seq)?;
            let expr_type = lhs.expr_type.clone();

            Ok((
                tok_seq,
                ASTExpr::new(
                    ASTExprNode::Assign(AssignNode {
                        lhs,
                        rhs,
                        kind: AssignKind::AndAssign,
                    }),
                    expr_type,
                ),
            ))
        } else if tok_seq.expect_punct(PunctKind::AddAssign).is_some() {
            tok_seq = tok_seq.next();

            let rhs;
            (tok_seq, rhs) = self.parse_assign(tok_seq)?;
            let expr_type = lhs.expr_type.clone();

            Ok((
                tok_seq,
                ASTExpr::new(
                    ASTExprNode::Assign(AssignNode {
                        lhs,
                        rhs,
                        kind: AssignKind::AddAssign,
                    }),
                    expr_type,
                ),
            ))
        } else if tok_seq.expect_punct(PunctKind::SubAssign).is_some() {
            tok_seq = tok_seq.next();

            let rhs;
            (tok_seq, rhs) = self.parse_assign(tok_seq)?;
            let expr_type = lhs.expr_type.clone();

            Ok((
                tok_seq,
                ASTExpr::new(
                    ASTExprNode::Assign(AssignNode {
                        lhs,
                        rhs,
                        kind: AssignKind::SubAssign,
                    }),
                    expr_type,
                ),
            ))
        } else if tok_seq.expect_punct(PunctKind::MulAssign).is_some() {
            tok_seq = tok_seq.next();

            let rhs;
            (tok_seq, rhs) = self.parse_assign(tok_seq)?;
            let expr_type = lhs.expr_type.clone();

            Ok((
                tok_seq,
                ASTExpr::new(
                    ASTExprNode::Assign(AssignNode {
                        lhs,
                        rhs,
                        kind: AssignKind::MulAssign,
                    }),
                    expr_type,
                ),
            ))
        } else if tok_seq.expect_punct(PunctKind::DivAssign).is_some() {
            tok_seq = tok_seq.next();

            let rhs;
            (tok_seq, rhs) = self.parse_assign(tok_seq)?;
            let expr_type = lhs.expr_type.clone();

            Ok((
                tok_seq,
                ASTExpr::new(
                    ASTExprNode::Assign(AssignNode {
                        lhs,
                        rhs,
                        kind: AssignKind::DivAssign,
                    }),
                    expr_type,
                ),
            ))
        } else if tok_seq.expect_punct(PunctKind::ModAssign).is_some() {
            tok_seq = tok_seq.next();

            let rhs;
            (tok_seq, rhs) = self.parse_assign(tok_seq)?;
            let expr_type = lhs.expr_type.clone();

            Ok((
                tok_seq,
                ASTExpr::new(
                    ASTExprNode::Assign(AssignNode {
                        lhs,
                        rhs,
                        kind: AssignKind::ModAssign,
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
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let then_expr;
            let else_expr;
            (tok_seq, then_expr) = self.parse_logical_or(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::Colon)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

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
                    return Err(ParseError::SemanticError(tok_seq));
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

    fn parse_logical_and(
        &self,
        mut tok_seq: TokenList,
    ) -> Result<(TokenList, ASTExpr), ParseError> {
        let mut lhs;
        (tok_seq, lhs) = self.parse_bit_or(tok_seq)?;

        while !tok_seq.is_empty() {
            if let TokenKind::Punct(punct) = tok_seq.get_token() {
                let kind = match punct {
                    PunctKind::LogicalAnd => BinaryOpKind::LogicalAnd,
                    _ => break,
                };

                tok_seq = tok_seq.next();

                let rhs;
                (tok_seq, rhs) = self.parse_bit_or(tok_seq)?;

                let lhs_type = lhs.expr_type.clone();
                let rhs_type = rhs.expr_type.clone();

                if !lhs_type.is_int_type() || !rhs_type.is_int_type() {
                    return Err(ParseError::SemanticError(tok_seq));
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
                    return Err(ParseError::SemanticError(tok_seq));
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
                    return Err(ParseError::SemanticError(tok_seq));
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
                    return Err(ParseError::SemanticError(tok_seq));
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
                    return Err(ParseError::SemanticError(tok_seq));
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
                    return Err(ParseError::SemanticError(tok_seq));
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
                    return Err(ParseError::SemanticError(tok_seq));
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
        let mut node;
        (tok_seq, node) = self.parse_mul(tok_seq)?;

        while !tok_seq.is_empty() {
            if let TokenKind::Punct(punct) = tok_seq.get_token() {
                match punct {
                    PunctKind::Plus => {
                        let kind = BinaryOpKind::Add;
                        tok_seq = tok_seq.next();

                        let rhs;
                        (tok_seq, rhs) = self.parse_mul(tok_seq)?;

                        let lhs = node.cast_array();
                        let rhs = rhs.cast_array();

                        let lhs_type = lhs.expr_type.clone();
                        let rhs_type = rhs.expr_type.clone();

                        if lhs_type.is_int_type() && rhs_type.is_int_type() {
                            let math_type = Type::get_math_binaryop_type(lhs_type, rhs_type);
                            let lhs = lhs.cast_to(&math_type);
                            let rhs = rhs.cast_to(&math_type);
                            node = ASTExpr::new(
                                ASTExprNode::BinaryOp(BinaryOpNode { lhs, rhs, kind }),
                                math_type,
                            );
                        } else if lhs_type.is_ptr_type() && rhs_type.is_int_type() {
                            node = ASTExpr::new(
                                ASTExprNode::BinaryOp(BinaryOpNode { lhs, rhs, kind }),
                                lhs_type,
                            );
                        } else if lhs_type.is_int_type() && rhs_type.is_ptr_type() {
                            node = ASTExpr::new(
                                ASTExprNode::BinaryOp(BinaryOpNode { lhs, rhs, kind }),
                                rhs_type,
                            );
                        } else {
                            return Err(ParseError::SemanticError(tok_seq));
                        }
                    }
                    PunctKind::Minus => {
                        let kind = BinaryOpKind::Sub;
                        tok_seq = tok_seq.next();

                        let rhs;
                        (tok_seq, rhs) = self.parse_mul(tok_seq)?;

                        let lhs = node.cast_array();
                        let rhs = rhs.cast_array();

                        let lhs_type = lhs.expr_type.clone();
                        let rhs_type = rhs.expr_type.clone();

                        if lhs_type.is_int_type() && rhs_type.is_int_type() {
                            let math_type = Type::get_math_binaryop_type(lhs_type, rhs_type);
                            let lhs = lhs.cast_to(&math_type);
                            let rhs = rhs.cast_to(&math_type);
                            node = ASTExpr::new(
                                ASTExprNode::BinaryOp(BinaryOpNode { lhs, rhs, kind }),
                                math_type,
                            );
                        } else if lhs_type.is_ptr_type() && rhs_type.is_int_type() {
                            node = ASTExpr::new(
                                ASTExprNode::BinaryOp(BinaryOpNode { lhs, rhs, kind }),
                                lhs_type,
                            );
                        } else {
                            return Err(ParseError::SemanticError(tok_seq));
                        }
                    }
                    _ => break,
                };
            } else {
                break;
            }
        }

        Ok((tok_seq, node))
    }

    fn parse_mul(&self, mut tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
        let mut node;
        (tok_seq, node) = self.parse_cast(tok_seq)?;

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
                let lhs = node;
                let lhs_type = lhs.expr_type.clone();
                let rhs_type = rhs.expr_type.clone();

                if !lhs_type.is_int_type() || !rhs_type.is_int_type() {
                    return Err(ParseError::SemanticError(tok_seq));
                }

                let math_type = Type::get_math_binaryop_type(lhs_type, rhs_type);
                let lhs = lhs.cast_to(&math_type);
                let rhs = rhs.cast_to(&math_type);

                node = ASTExpr::new(
                    ASTExprNode::BinaryOp(BinaryOpNode { lhs, rhs, kind }),
                    math_type,
                );
            } else {
                break;
            }
        }

        Ok((tok_seq, node))
    }

    fn parse_cast(&self, tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
        self.parse_unary(tok_seq)
    }

    fn parse_unary(&self, mut tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
        if tok_seq.expect_keyword(KeywordKind::Sizeof).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::Sizeof)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let expr;
            (tok_seq, expr) = self.parse_unary(tok_seq)?;

            let node = ASTExpr::new(
                ASTExprNode::UnaryOp(UnaryOpNode {
                    expr,
                    kind: UnaryOpKind::Sizeof,
                }),
                Type::new(TypeNode::Int),
            );

            Ok((tok_seq, node))
        } else if tok_seq.expect_keyword(KeywordKind::Alignof).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::Alignof)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let expr;
            (tok_seq, expr) = self.parse_unary(tok_seq)?;

            let node = ASTExpr::new(
                ASTExprNode::UnaryOp(UnaryOpNode {
                    expr,
                    kind: UnaryOpKind::Alignof,
                }),
                Type::new(TypeNode::Int),
            );

            Ok((tok_seq, node))
        } else if tok_seq.expect_punct(PunctKind::Plus).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::Plus)
                .ok_or(ParseError::SyntaxError(tok_seq))?;
            let expr;
            (tok_seq, expr) = self.parse_unary(tok_seq)?;
            let expr_type = expr.expr_type.clone();
            if !expr_type.is_int_type() {
                return Err(ParseError::SemanticError(tok_seq));
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
                .ok_or(ParseError::SyntaxError(tok_seq))?;
            let expr;
            (tok_seq, expr) = self.parse_unary(tok_seq)?;
            let expr_type = expr.expr_type.clone();
            if !expr_type.is_int_type() {
                return Err(ParseError::SemanticError(tok_seq));
            }

            let node = ASTExpr::new(
                ASTExprNode::UnaryOp(UnaryOpNode {
                    expr,
                    kind: UnaryOpKind::Minus,
                }),
                expr_type,
            );

            Ok((tok_seq, node))
        } else if tok_seq.expect_punct(PunctKind::Increment).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::Increment)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let lhs;
            (tok_seq, lhs) = self.parse_unary(tok_seq)?;
            let expr_type = lhs.expr_type.clone();

            if !expr_type.is_int_type() {
                return Err(ParseError::SemanticError(tok_seq));
            }

            let node = ASTExpr::new(
                ASTExprNode::Assign(AssignNode {
                    lhs,
                    rhs: ASTExpr::new(ASTExprNode::Number(1), Type::new(TypeNode::Int)),
                    kind: AssignKind::AddAssign,
                }),
                expr_type,
            );

            Ok((tok_seq, node))
        } else if tok_seq.expect_punct(PunctKind::Decrement).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::Decrement)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let lhs;
            (tok_seq, lhs) = self.parse_unary(tok_seq)?;
            let expr_type = lhs.expr_type.clone();

            if !expr_type.is_int_type() {
                return Err(ParseError::SemanticError(tok_seq));
            }

            let node = ASTExpr::new(
                ASTExprNode::Assign(AssignNode {
                    lhs,
                    rhs: ASTExpr::new(ASTExprNode::Number(1), Type::new(TypeNode::Int)),
                    kind: AssignKind::SubAssign,
                }),
                expr_type,
            );

            Ok((tok_seq, node))
        } else if tok_seq.expect_punct(PunctKind::Ampersand).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::Ampersand)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

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
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let expr;
            (tok_seq, expr) = self.parse_unary(tok_seq)?;
            let expr = expr.cast_array();

            let expr_type = expr
                .expr_type
                .clone()
                .get_ptr_to()
                .map_err(|()| ParseError::SemanticError(tok_seq.clone()))?;

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
                .ok_or(ParseError::SyntaxError(tok_seq))?;

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
                .ok_or(ParseError::SyntaxError(tok_seq))?;

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
        let mut node;
        (tok_seq, node) = self.parse_primary(tok_seq)?;

        loop {
            if tok_seq.expect_punct(PunctKind::OpenParenthesis).is_some() {
                tok_seq = tok_seq
                    .expect_punct(PunctKind::OpenParenthesis)
                    .ok_or(ParseError::SyntaxError(tok_seq))?;

                if !node.expr_type.is_function_type() {
                    return Err(ParseError::SemanticError(tok_seq));
                }

                let mut args = Vec::new();

                while tok_seq.expect_punct(PunctKind::CloseParenthesis).is_none() {
                    let arg;
                    (tok_seq, arg) = self.parse_assign(tok_seq)?;
                    args.push(arg);

                    if tok_seq.expect_punct(PunctKind::Comma).is_some() {
                        tok_seq = tok_seq.next();
                    }
                }

                tok_seq = tok_seq
                    .expect_punct(PunctKind::CloseParenthesis)
                    .ok_or(ParseError::SyntaxError(tok_seq))?;

                let arg_types = node.expr_type.get_arg_types().unwrap();

                if arg_types.is_some() {
                    let arg_types = arg_types.unwrap();
                    if args.len() != arg_types.len() {
                        return Err(ParseError::SemanticError(tok_seq));
                    }

                    for (i, ty) in arg_types.iter().enumerate() {
                        let arg = &args[i];
                        args[i] = arg.cast_to(ty);
                    }
                }

                let expr = node;

                let TypeNode::Func(return_type,_) = expr.expr_type.get_node() else{return Err(ParseError::SemanticError(tok_seq));};

                node = ASTExpr::new(ASTExprNode::FuncCall(expr, args), return_type);
            } else if tok_seq.expect_punct(PunctKind::OpenSquareBracket).is_some() {
                tok_seq = tok_seq
                    .expect_punct(PunctKind::OpenSquareBracket)
                    .ok_or(ParseError::SyntaxError(tok_seq))?;

                let index;
                (tok_seq, index) = self.parse_expr(tok_seq)?;

                tok_seq = tok_seq
                    .expect_punct(PunctKind::CloseSquareBracket)
                    .ok_or(ParseError::SyntaxError(tok_seq))?;

                let cast = node.cast_array();
                let index = index.cast_array();

                let ptr_type = if cast.expr_type.is_ptr_type() && index.expr_type.is_int_type() {
                    cast.expr_type.clone()
                } else if cast.expr_type.is_int_type() && index.expr_type.is_ptr_type() {
                    index.expr_type.clone()
                } else {
                    return Err(ParseError::SemanticError(tok_seq));
                };

                let plus = ASTExpr::new(
                    ASTExprNode::BinaryOp(BinaryOpNode {
                        kind: BinaryOpKind::Add,
                        lhs: cast,
                        rhs: index,
                    }),
                    ptr_type.clone(),
                );

                node = ASTExpr::new(
                    ASTExprNode::UnaryOp(UnaryOpNode {
                        kind: UnaryOpKind::Deref,
                        expr: plus,
                    }),
                    ptr_type.get_ptr_to().unwrap(),
                );
            } else if tok_seq.expect_punct(PunctKind::Increment).is_some() {
                tok_seq = tok_seq
                    .expect_punct(PunctKind::Increment)
                    .ok_or(ParseError::SyntaxError(tok_seq))?;
                let expr = node;
                let expr_type = expr.expr_type.clone();

                if !expr_type.is_int_type() {
                    return Err(ParseError::SemanticError(tok_seq));
                }

                node = ASTExpr::new(ASTExprNode::PostIncrement(expr), expr_type);
            } else if tok_seq.expect_punct(PunctKind::Decrement).is_some() {
                tok_seq = tok_seq
                    .expect_punct(PunctKind::Decrement)
                    .ok_or(ParseError::SyntaxError(tok_seq))?;
                let expr = node;
                let expr_type = expr.expr_type.clone();

                if !expr_type.is_int_type() {
                    return Err(ParseError::SemanticError(tok_seq));
                }

                node = ASTExpr::new(ASTExprNode::PostDecrement(expr), expr_type);
            } else {
                break;
            }
        }
        Ok((tok_seq, node))
    }

    fn parse_primary(&self, mut tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
        if tok_seq.is_empty() {
            return Err(ParseError::SyntaxError(tok_seq));
        }

        if let TokenKind::Number(num) = tok_seq.get_token() {
            Ok((
                tok_seq.next(),
                ASTExpr::new(ASTExprNode::Number(num), Type::new(TypeNode::Int)),
            ))
        } else if tok_seq.expect_punct(PunctKind::OpenParenthesis).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenParenthesis)
                .ok_or(ParseError::SyntaxError(tok_seq))?;
            let ret;
            (tok_seq, ret) = self.parse_expr(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseParenthesis)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            Ok((tok_seq, ret))
        } else if let TokenKind::Ident(ref var_name) = tok_seq.get_token() {
            let obj = self
                .search_obj(var_name)
                .ok_or(ParseError::SemanticError(tok_seq.clone()))?;
            let expr_type = obj.borrow().obj_type.clone();
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
            Err(ParseError::SyntaxError(tok_seq))
        }
    }
}
