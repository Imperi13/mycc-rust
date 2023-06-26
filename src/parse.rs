mod decl_spec;
mod declarator;

use crate::ast::expr::ASTAssignKind;
use crate::ast::expr::ASTAssignNode;
use crate::ast::expr::ASTBinaryOpKind;
use crate::ast::expr::ASTBinaryOpNode;
use crate::ast::expr::ASTExpr;
use crate::ast::expr::ASTExprNode;
use crate::ast::expr::ASTUnaryOpKind;
use crate::ast::expr::ASTUnaryOpNode;
use crate::ast::stmt::ASTStmt;
use crate::ast::stmt::ASTStmtNode;
use crate::ast::stmt::DoWhileStmt;
use crate::ast::stmt::ForStmt;
use crate::ast::stmt::SwitchStmt;
use crate::ast::stmt::WhileStmt;
use crate::ast::ASTBlockStmt;
use crate::ast::ASTFunction;
use crate::ast::AST;
use crate::error::ParseError;
use crate::obj::GlobalObjArena;
use crate::obj::LocalObjArena;
use crate::obj::Obj;
use crate::tokenize::KeywordKind;
use crate::tokenize::PunctKind;
use crate::tokenize::TokenKind;
use crate::tokenize::TokenList;
use crate::types::Type;
use crate::types::TypeNode;

use std::collections::HashMap;
use std::collections::VecDeque;

pub fn parse_all(mut tok_seq: TokenList) -> Result<AST, ParseError> {
    let mut arena = ParseArena::new();

    while !tok_seq.is_empty() {
        tok_seq = arena.parse_global(tok_seq)?;
    }

    Ok(arena.gen_ast())
}

struct IDStack {
    id: usize,
    id_stack: VecDeque<usize>,
}

impl IDStack {
    pub fn new() -> Self {
        IDStack {
            id: 0,
            id_stack: VecDeque::new(),
        }
    }

    pub fn get_current_id(&self) -> usize {
        self.id_stack.back().unwrap().clone()
    }

    pub fn push_id(&mut self) -> usize {
        let id = self.id;
        self.id += 1;
        self.id_stack.push_back(id);
        id
    }

    pub fn pop_id(&mut self) {
        self.id_stack.pop_back();
    }
}

struct ParseArena {
    obj_arena: GlobalObjArena,
    variables: Vec<Obj>,
    functions: Vec<ASTFunction>,

    global_obj_map: HashMap<String, Obj>,

    struct_id: usize,
    global_structs: HashMap<String, Type>,

    func_arena: Option<ParseFunctionArena>,
}

struct ParseFunctionArena {
    func_obj: Obj,

    obj_arena: LocalObjArena,

    local_objs: VecDeque<HashMap<String, Obj>>,

    switch_id: IDStack,
    break_id: IDStack,
    continue_id: IDStack,
}

impl ParseFunctionArena {
    pub fn new(func_obj: Obj) -> Self {
        ParseFunctionArena {
            func_obj,
            obj_arena: LocalObjArena::new(),
            local_objs: VecDeque::from([HashMap::new()]),
            switch_id: IDStack::new(),
            break_id: IDStack::new(),
            continue_id: IDStack::new(),
        }
    }
}

impl ParseArena {
    pub fn new() -> Self {
        ParseArena {
            obj_arena: GlobalObjArena::new(),
            variables: Vec::new(),
            functions: Vec::new(),
            global_obj_map: HashMap::new(),
            struct_id: 0,
            global_structs: HashMap::new(),
            func_arena: None,
        }
    }
    pub fn gen_ast(self) -> AST {
        AST {
            global_objs: self.global_obj_map.into_values().collect(),
            variables: self.variables,
            functions: self.functions,
            obj_arena: self.obj_arena,
        }
    }

    fn insert_local_obj(&mut self, obj_name: &str, obj_type: Type) -> Result<Obj, ()> {
        assert!(self.func_arena.is_some());

        if self
            .func_arena
            .as_mut()
            .unwrap()
            .local_objs
            .back_mut()
            .unwrap()
            .contains_key(obj_name)
        {
            return Err(());
        }

        let obj = self
            .func_arena
            .as_mut()
            .unwrap()
            .obj_arena
            .publish_obj(obj_name, obj_type);

        self.func_arena
            .as_mut()
            .unwrap()
            .local_objs
            .back_mut()
            .unwrap()
            .insert(String::from(obj_name), obj.clone());

        Ok(obj)
    }

    fn push_local_scope(&mut self) {
        self.func_arena
            .as_mut()
            .unwrap()
            .local_objs
            .push_back(HashMap::new());
    }

    fn pop_local_scope(&mut self) {
        self.func_arena.as_mut().unwrap().local_objs.pop_back();
    }

    fn search_obj(&self, obj_name: &str) -> Option<Obj> {
        for map in self.func_arena.as_ref().unwrap().local_objs.iter().rev() {
            if map.contains_key(obj_name) {
                return Some(map.get(obj_name).unwrap().clone());
            }
        }

        if self.global_obj_map.contains_key(obj_name) {
            return Some(self.global_obj_map.get(obj_name).unwrap().clone());
        }

        None
    }

    fn insert_global_obj(&mut self, obj_name: &str, obj_type: Type) -> Result<Obj, ()> {
        if self.global_obj_map.contains_key(obj_name) {
            return Err(());
        }

        let obj = self.obj_arena.publish_obj(obj_name, obj_type);

        self.global_obj_map
            .insert(String::from(obj_name), obj.clone());

        Ok(obj)
    }

    fn insert_global_struct(&mut self, struct_type: Type) -> Type {
        let TypeNode::Struct(ref st_decl) = *struct_type.borrow() else {panic!() };

        if !self.global_structs.contains_key(&st_decl.tag) {
            self.global_structs
                .insert(st_decl.tag.clone(), struct_type.clone());
        }

        struct_type.clone()
    }

    fn parse_global(&mut self, mut tok_seq: TokenList) -> Result<TokenList, ParseError> {
        let decl_spec_type;
        (tok_seq, decl_spec_type) = self.parse_decl_spec(tok_seq)?;

        if decl_spec_type.is_struct_type() {
            self.insert_global_struct(decl_spec_type.clone());
        }

        if tok_seq.expect_punct(PunctKind::SemiColon).is_some() {
            tok_seq = tok_seq.next();

            return Ok(tok_seq);
        }

        let declarator;

        (tok_seq, declarator) = self.parse_declarator(tok_seq)?;
        let obj_name = declarator.get_name();
        let obj_type = declarator.get_type(decl_spec_type);

        // todo!
        // check conflict previous global_obj
        //

        let obj = self
            .insert_global_obj(&obj_name, obj_type.clone())
            .map_err(|()| ParseError::SemanticError(tok_seq.clone()))?;

        if tok_seq.equal_punct(PunctKind::OpenBrace) {
            // args
            self.func_arena = Some(ParseFunctionArena::new(obj.clone()));

            let mut args_obj = Vec::new();

            let declarator_args = declarator.get_args();

            if declarator_args.is_some() {
                for (ref decl_spec_type, ref declarator) in declarator_args.unwrap() {
                    let arg_name = declarator.get_name();
                    let arg_type = declarator.get_type(decl_spec_type.clone());

                    let arg_type = if arg_type.is_array_type() {
                        Type::new_ptr_type(arg_type.get_array_to().unwrap())
                    } else {
                        arg_type
                    };

                    let arg_obj = self
                        .insert_local_obj(&arg_name, arg_type)
                        .map_err(|()| ParseError::SemanticError(tok_seq.clone()))?;

                    args_obj.push(arg_obj);
                }
            }

            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenBrace)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let mut stmts = Vec::new();

            while tok_seq.expect_punct(PunctKind::CloseBrace).is_none() {
                if self.is_type_token(tok_seq.clone()) {
                    let decl_spec_type;
                    (tok_seq, decl_spec_type) = self.parse_decl_spec(tok_seq)?;

                    if tok_seq.expect_punct(PunctKind::SemiColon).is_some() {
                        tok_seq = tok_seq.next();
                        continue;
                    }

                    let declarator;
                    (tok_seq, declarator) = self.parse_declarator(tok_seq)?;
                    let var_name = declarator.get_name();
                    let var_type = declarator.get_type(decl_spec_type);

                    if !var_type.is_complete_type() {
                        return Err(ParseError::SemanticError(tok_seq));
                    }

                    tok_seq = tok_seq
                        .expect_punct(PunctKind::SemiColon)
                        .ok_or(ParseError::SyntaxError(tok_seq))?;

                    let obj = self
                        .insert_local_obj(&var_name, var_type)
                        .map_err(|()| ParseError::SemanticError(tok_seq.clone()))?;
                    stmts.push(ASTBlockStmt::Declaration(obj));
                } else {
                    let stmt;
                    (tok_seq, stmt) = self.parse_stmt(tok_seq)?;
                    stmts.push(ASTBlockStmt::Stmt(stmt));
                }
            }

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseBrace)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let ast_function = ASTFunction {
                func_obj: self.func_arena.as_ref().unwrap().func_obj.clone(),
                args_obj,
                stmts,

                obj_arena: self.func_arena.as_ref().unwrap().obj_arena.clone(),
            };
            self.functions.push(ast_function);

            self.func_arena = None;
            Ok(tok_seq)
        } else if tok_seq.equal_punct(PunctKind::SemiColon) {
            tok_seq = tok_seq.next();

            if !obj_type.is_function_type() {
                self.variables.push(obj);
            }
            Ok(tok_seq)
        } else if tok_seq.equal_punct(PunctKind::Equal) {
            todo!();
        } else {
            Err(ParseError::SyntaxError(tok_seq))
        }
    }

    fn parse_stmt(&mut self, mut tok_seq: TokenList) -> Result<(TokenList, ASTStmt), ParseError> {
        if tok_seq.expect_keyword(KeywordKind::Return).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::Return)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            if tok_seq.equal_punct(PunctKind::SemiColon) {
                tok_seq = tok_seq.next();
                Ok((tok_seq, ASTStmt::new(ASTStmtNode::Return(None))))
            } else {
                let expr;
                (tok_seq, expr) = self.parse_expr(tok_seq)?;

                tok_seq = tok_seq
                    .expect_punct(PunctKind::SemiColon)
                    .ok_or(ParseError::SyntaxError(tok_seq))?;

                let return_type = self
                    .func_arena
                    .as_mut()
                    .unwrap()
                    .func_obj
                    .borrow()
                    .obj_type
                    .get_return_type()
                    .unwrap();

                let cast = expr.cast_to(&return_type);

                Ok((tok_seq, ASTStmt::new(ASTStmtNode::Return(Some(cast)))))
            }
        } else if tok_seq.equal_keyword(KeywordKind::Default) {
            tok_seq = tok_seq.next();
            tok_seq = tok_seq
                .expect_punct(PunctKind::Colon)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let switch_id = self.func_arena.as_mut().unwrap().switch_id.get_current_id();

            let stmt;
            (tok_seq, stmt) = self.parse_stmt(tok_seq)?;

            Ok((tok_seq, ASTStmt::new(ASTStmtNode::Default(stmt, switch_id))))
        } else if tok_seq.equal_keyword(KeywordKind::Case) {
            tok_seq = tok_seq.next();

            let expr;
            (tok_seq, expr) = self.parse_expr(tok_seq)?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::Colon)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let switch_id = self.func_arena.as_mut().unwrap().switch_id.get_current_id();

            let stmt;
            (tok_seq, stmt) = self.parse_stmt(tok_seq)?;

            Ok((
                tok_seq,
                ASTStmt::new(ASTStmtNode::Case(expr, stmt, switch_id)),
            ))
        } else if tok_seq.expect_keyword(KeywordKind::Break).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::Break)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let break_id = self.func_arena.as_mut().unwrap().break_id.get_current_id();

            Ok((tok_seq, ASTStmt::new(ASTStmtNode::Break(break_id))))
        } else if tok_seq.expect_keyword(KeywordKind::Continue).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::Continue)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let continue_id = self
                .func_arena
                .as_mut()
                .unwrap()
                .continue_id
                .get_current_id();

            Ok((tok_seq, ASTStmt::new(ASTStmtNode::Continue(continue_id))))
        } else if tok_seq.expect_keyword(KeywordKind::If).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::If)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenParenthesis)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let cond;
            (tok_seq, cond) = self.parse_expr(tok_seq)?;
            let cond = cond.cast_to(&Type::new(TypeNode::Bool));

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
        } else if tok_seq.equal_keyword(KeywordKind::Switch) {
            tok_seq = tok_seq.next();

            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenParenthesis)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let cond;
            (tok_seq, cond) = self.parse_expr(tok_seq)?;
            let cond = cond.cast_to(&Type::new(TypeNode::Int));

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseParenthesis)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let break_id = self.func_arena.as_mut().unwrap().break_id.push_id();
            let switch_id = self.func_arena.as_mut().unwrap().switch_id.push_id();

            let stmt;
            (tok_seq, stmt) = self.parse_stmt(tok_seq)?;

            self.func_arena.as_mut().unwrap().break_id.pop_id();
            self.func_arena.as_mut().unwrap().switch_id.pop_id();

            Ok((
                tok_seq,
                ASTStmt::new(ASTStmtNode::Switch(SwitchStmt {
                    cond,
                    stmt,
                    switch_id,
                    break_id,
                })),
            ))
        } else if tok_seq.expect_keyword(KeywordKind::While).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::While)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenParenthesis)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let cond;
            (tok_seq, cond) = self.parse_expr(tok_seq)?;
            let cond = cond.cast_to(&Type::new(TypeNode::Bool));

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseParenthesis)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            // push stmt_id
            let break_id = self.func_arena.as_mut().unwrap().break_id.push_id();
            let continue_id = self.func_arena.as_mut().unwrap().continue_id.push_id();

            let loop_stmt;
            (tok_seq, loop_stmt) = self.parse_stmt(tok_seq)?;

            self.func_arena.as_mut().unwrap().break_id.pop_id();
            self.func_arena.as_mut().unwrap().continue_id.pop_id();

            Ok((
                tok_seq,
                ASTStmt::new(ASTStmtNode::While(WhileStmt {
                    cond,
                    loop_stmt,
                    break_id,
                    continue_id,
                })),
            ))
        } else if tok_seq.expect_keyword(KeywordKind::Do).is_some() {
            tok_seq = tok_seq
                .expect_keyword(KeywordKind::Do)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            // push stmt_id
            let break_id = self.func_arena.as_mut().unwrap().break_id.push_id();
            let continue_id = self.func_arena.as_mut().unwrap().continue_id.push_id();

            let loop_stmt;
            (tok_seq, loop_stmt) = self.parse_stmt(tok_seq)?;

            self.func_arena.as_mut().unwrap().break_id.pop_id();
            self.func_arena.as_mut().unwrap().continue_id.pop_id();

            tok_seq = tok_seq
                .expect_keyword(KeywordKind::While)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenParenthesis)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let cond;
            (tok_seq, cond) = self.parse_expr(tok_seq)?;
            let cond = cond.cast_to(&Type::new(TypeNode::Bool));

            tok_seq = tok_seq
                .expect_punct(PunctKind::CloseParenthesis)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            tok_seq = tok_seq
                .expect_punct(PunctKind::SemiColon)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            Ok((
                tok_seq,
                ASTStmt::new(ASTStmtNode::DoWhile(DoWhileStmt {
                    cond,
                    loop_stmt,
                    break_id,
                    continue_id,
                })),
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
                let tmp = tmp.cast_to(&Type::new(TypeNode::Bool));
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
            let break_id = self.func_arena.as_mut().unwrap().break_id.push_id();
            let continue_id = self.func_arena.as_mut().unwrap().continue_id.push_id();

            let loop_stmt;
            (tok_seq, loop_stmt) = self.parse_stmt(tok_seq)?;

            self.func_arena.as_mut().unwrap().break_id.pop_id();
            self.func_arena.as_mut().unwrap().continue_id.pop_id();

            Ok((
                tok_seq,
                ASTStmt::new(ASTStmtNode::For(ForStmt {
                    start,
                    cond,
                    step,
                    loop_stmt,
                    break_id,
                    continue_id,
                })),
            ))
        } else if tok_seq.expect_punct(PunctKind::OpenBrace).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::OpenBrace)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            self.push_local_scope();

            let mut stmts = Vec::new();

            while tok_seq.expect_punct(PunctKind::CloseBrace).is_none() {
                if self.is_type_token(tok_seq.clone()) {
                    let decl_spec_type;
                    (tok_seq, decl_spec_type) = self.parse_decl_spec(tok_seq)?;

                    if tok_seq.expect_punct(PunctKind::SemiColon).is_some() {
                        tok_seq = tok_seq.next();
                        continue;
                    }

                    let declarator;
                    (tok_seq, declarator) = self.parse_declarator(tok_seq)?;
                    let var_name = declarator.get_name();
                    let var_type = declarator.get_type(decl_spec_type);

                    if !var_type.is_complete_type() {
                        return Err(ParseError::SemanticError(tok_seq));
                    }

                    tok_seq = tok_seq
                        .expect_punct(PunctKind::SemiColon)
                        .ok_or(ParseError::SyntaxError(tok_seq))?;

                    let obj = self
                        .insert_local_obj(&var_name, var_type)
                        .map_err(|()| ParseError::SemanticError(tok_seq.clone()))?;
                    stmts.push(ASTBlockStmt::Declaration(obj));
                } else {
                    let stmt;
                    (tok_seq, stmt) = self.parse_stmt(tok_seq)?;
                    stmts.push(ASTBlockStmt::Stmt(stmt));
                }
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
                    PunctKind::Comma => ASTBinaryOpKind::Comma,
                    _ => break,
                };

                tok_seq = tok_seq.next();

                let rhs;
                (tok_seq, rhs) = self.parse_assign(tok_seq)?;

                let rhs_type = rhs.expr_type.clone();

                lhs = ASTExpr::new(
                    ASTExprNode::BinaryOp(ASTBinaryOpNode { lhs, rhs, kind }),
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
                    ASTExprNode::Assign(ASTAssignNode {
                        lhs,
                        rhs,
                        kind: ASTAssignKind::Assign,
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
                    ASTExprNode::Assign(ASTAssignNode {
                        lhs,
                        rhs,
                        kind: ASTAssignKind::LeftShiftAssign,
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
                    ASTExprNode::Assign(ASTAssignNode {
                        lhs,
                        rhs,
                        kind: ASTAssignKind::RightShiftAssign,
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
                    ASTExprNode::Assign(ASTAssignNode {
                        lhs,
                        rhs,
                        kind: ASTAssignKind::OrAssign,
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
                    ASTExprNode::Assign(ASTAssignNode {
                        lhs,
                        rhs,
                        kind: ASTAssignKind::XorAssign,
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
                    ASTExprNode::Assign(ASTAssignNode {
                        lhs,
                        rhs,
                        kind: ASTAssignKind::AndAssign,
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
                    ASTExprNode::Assign(ASTAssignNode {
                        lhs,
                        rhs,
                        kind: ASTAssignKind::AddAssign,
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
                    ASTExprNode::Assign(ASTAssignNode {
                        lhs,
                        rhs,
                        kind: ASTAssignKind::SubAssign,
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
                    ASTExprNode::Assign(ASTAssignNode {
                        lhs,
                        rhs,
                        kind: ASTAssignKind::MulAssign,
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
                    ASTExprNode::Assign(ASTAssignNode {
                        lhs,
                        rhs,
                        kind: ASTAssignKind::DivAssign,
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
                    ASTExprNode::Assign(ASTAssignNode {
                        lhs,
                        rhs,
                        kind: ASTAssignKind::ModAssign,
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
        let node;
        (tok_seq, node) = self.parse_logical_or(tok_seq)?;

        if tok_seq.expect_punct(PunctKind::Question).is_some() {
            tok_seq = tok_seq
                .expect_punct(PunctKind::Question)
                .ok_or(ParseError::SyntaxError(tok_seq))?;

            let cond = node.cast_to(&Type::new(TypeNode::Bool));

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
            Ok((tok_seq, node))
        }
    }

    fn parse_logical_or(&self, mut tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
        let mut node;
        (tok_seq, node) = self.parse_logical_and(tok_seq)?;

        while !tok_seq.is_empty() {
            if let TokenKind::Punct(punct) = tok_seq.get_token() {
                let kind = match punct {
                    PunctKind::LogicalOr => ASTBinaryOpKind::LogicalOr,
                    _ => break,
                };

                tok_seq = tok_seq.next();

                let lhs = node.cast_to(&Type::new(TypeNode::Bool));

                let rhs;
                (tok_seq, rhs) = self.parse_logical_and(tok_seq)?;
                let rhs = rhs.cast_to(&Type::new(TypeNode::Bool));

                node = ASTExpr::new(
                    ASTExprNode::BinaryOp(ASTBinaryOpNode { lhs, rhs, kind }),
                    Type::new(TypeNode::Int),
                );
            } else {
                break;
            }
        }

        Ok((tok_seq, node))
    }

    fn parse_logical_and(
        &self,
        mut tok_seq: TokenList,
    ) -> Result<(TokenList, ASTExpr), ParseError> {
        let mut node;
        (tok_seq, node) = self.parse_bit_or(tok_seq)?;

        while !tok_seq.is_empty() {
            if let TokenKind::Punct(punct) = tok_seq.get_token() {
                let kind = match punct {
                    PunctKind::LogicalAnd => ASTBinaryOpKind::LogicalAnd,
                    _ => break,
                };

                tok_seq = tok_seq.next();

                let lhs = node.cast_to(&Type::new(TypeNode::Bool));

                let rhs;
                (tok_seq, rhs) = self.parse_bit_or(tok_seq)?;
                let rhs = rhs.cast_to(&Type::new(TypeNode::Bool));

                node = ASTExpr::new(
                    ASTExprNode::BinaryOp(ASTBinaryOpNode { lhs, rhs, kind }),
                    Type::new(TypeNode::Int),
                );
            } else {
                break;
            }
        }

        Ok((tok_seq, node))
    }

    fn parse_bit_or(&self, mut tok_seq: TokenList) -> Result<(TokenList, ASTExpr), ParseError> {
        let mut lhs;
        (tok_seq, lhs) = self.parse_bit_xor(tok_seq)?;

        while !tok_seq.is_empty() {
            if let TokenKind::Punct(punct) = tok_seq.get_token() {
                let kind = match punct {
                    PunctKind::BitOr => ASTBinaryOpKind::BitOr,
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
                    ASTExprNode::BinaryOp(ASTBinaryOpNode { lhs, rhs, kind }),
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
                    PunctKind::Caret => ASTBinaryOpKind::BitXor,
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
                    ASTExprNode::BinaryOp(ASTBinaryOpNode { lhs, rhs, kind }),
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
                    PunctKind::Ampersand => ASTBinaryOpKind::BitAnd,
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
                    ASTExprNode::BinaryOp(ASTBinaryOpNode { lhs, rhs, kind }),
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
                    PunctKind::Equal => ASTBinaryOpKind::Equal,
                    PunctKind::NotEqual => ASTBinaryOpKind::NotEqual,
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
                    ASTExprNode::BinaryOp(ASTBinaryOpNode { lhs, rhs, kind }),
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
                    PunctKind::Less => ASTBinaryOpKind::Less,
                    PunctKind::LessEqual => ASTBinaryOpKind::LessEqual,
                    PunctKind::Greater => ASTBinaryOpKind::Greater,
                    PunctKind::GreaterEqual => ASTBinaryOpKind::GreaterEqual,
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
                    ASTExprNode::BinaryOp(ASTBinaryOpNode { lhs, rhs, kind }),
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
                    PunctKind::LeftShift => ASTBinaryOpKind::LeftShift,
                    PunctKind::RightShift => ASTBinaryOpKind::RightShift,
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
                    ASTExprNode::BinaryOp(ASTBinaryOpNode { lhs, rhs, kind }),
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
                        let kind = ASTBinaryOpKind::Add;
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
                                ASTExprNode::BinaryOp(ASTBinaryOpNode { lhs, rhs, kind }),
                                math_type,
                            );
                        } else if lhs_type.is_ptr_type() && rhs_type.is_int_type() {
                            node = ASTExpr::new(
                                ASTExprNode::BinaryOp(ASTBinaryOpNode { lhs, rhs, kind }),
                                lhs_type,
                            );
                        } else if lhs_type.is_int_type() && rhs_type.is_ptr_type() {
                            node = ASTExpr::new(
                                ASTExprNode::BinaryOp(ASTBinaryOpNode { lhs, rhs, kind }),
                                rhs_type,
                            );
                        } else {
                            return Err(ParseError::SemanticError(tok_seq));
                        }
                    }
                    PunctKind::Minus => {
                        let kind = ASTBinaryOpKind::Sub;
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
                                ASTExprNode::BinaryOp(ASTBinaryOpNode { lhs, rhs, kind }),
                                math_type,
                            );
                        } else if lhs_type.is_ptr_type() && rhs_type.is_int_type() {
                            node = ASTExpr::new(
                                ASTExprNode::BinaryOp(ASTBinaryOpNode { lhs, rhs, kind }),
                                lhs_type,
                            );
                        } else if lhs_type.is_ptr_type() && rhs_type.is_ptr_type() {
                            node = ASTExpr::new(
                                ASTExprNode::BinaryOp(ASTBinaryOpNode { lhs, rhs, kind }),
                                Type::new(TypeNode::Int),
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
                    PunctKind::Asterisk => ASTBinaryOpKind::Mul,
                    PunctKind::Slash => ASTBinaryOpKind::Div,
                    PunctKind::Percent => ASTBinaryOpKind::Mod,
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
                    ASTExprNode::BinaryOp(ASTBinaryOpNode { lhs, rhs, kind }),
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
                ASTExprNode::UnaryOp(ASTUnaryOpNode {
                    expr,
                    kind: ASTUnaryOpKind::Sizeof,
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
                ASTExprNode::UnaryOp(ASTUnaryOpNode {
                    expr,
                    kind: ASTUnaryOpKind::Alignof,
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
                ASTExprNode::UnaryOp(ASTUnaryOpNode {
                    expr,
                    kind: ASTUnaryOpKind::Plus,
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
                ASTExprNode::UnaryOp(ASTUnaryOpNode {
                    expr,
                    kind: ASTUnaryOpKind::Minus,
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
                ASTExprNode::Assign(ASTAssignNode {
                    lhs,
                    rhs: ASTExpr::new(ASTExprNode::Number(1), Type::new(TypeNode::Int)),
                    kind: ASTAssignKind::AddAssign,
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
                ASTExprNode::Assign(ASTAssignNode {
                    lhs,
                    rhs: ASTExpr::new(ASTExprNode::Number(1), Type::new(TypeNode::Int)),
                    kind: ASTAssignKind::SubAssign,
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
                ASTExprNode::UnaryOp(ASTUnaryOpNode {
                    expr,
                    kind: ASTUnaryOpKind::Addr,
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
                ASTExprNode::UnaryOp(ASTUnaryOpNode {
                    expr,
                    kind: ASTUnaryOpKind::Deref,
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
                ASTExprNode::UnaryOp(ASTUnaryOpNode {
                    expr,
                    kind: ASTUnaryOpKind::LogicalNot,
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
                ASTExprNode::UnaryOp(ASTUnaryOpNode {
                    expr,
                    kind: ASTUnaryOpKind::BitNot,
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

                let TypeNode::Func(ref return_type,_) = *expr.expr_type.borrow() else{return Err(ParseError::SemanticError(tok_seq));};

                node = ASTExpr::new(
                    ASTExprNode::FuncCall(expr.clone(), args),
                    return_type.clone(),
                );
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
                    ASTExprNode::BinaryOp(ASTBinaryOpNode {
                        kind: ASTBinaryOpKind::Add,
                        lhs: cast,
                        rhs: index,
                    }),
                    ptr_type.clone(),
                );

                node = ASTExpr::new(
                    ASTExprNode::UnaryOp(ASTUnaryOpNode {
                        kind: ASTUnaryOpKind::Deref,
                        expr: plus,
                    }),
                    ptr_type.get_ptr_to().unwrap(),
                );
            } else if tok_seq.expect_punct(PunctKind::Dot).is_some() {
                tok_seq = tok_seq.next();
                let TokenKind::Ident(member) = tok_seq.get_token() else {return Err(ParseError::SyntaxError(tok_seq));};

                tok_seq = tok_seq.next();
                if !node.expr_type.is_struct_type() {
                    return Err(ParseError::SemanticError(tok_seq));
                }

                let (index, expr_type) = node.expr_type.get_struct_member(&member);

                node = ASTExpr::new(ASTExprNode::Dot(node, index), expr_type);
            } else if tok_seq.expect_punct(PunctKind::Arrow).is_some() {
                tok_seq = tok_seq.next();

                let TokenKind::Ident(member) = tok_seq.get_token() else {return Err(ParseError::SyntaxError(tok_seq));};

                tok_seq = tok_seq.next();

                if !node.expr_type.is_ptr_type() {
                    return Err(ParseError::SemanticError(tok_seq));
                }

                let st_type = node.expr_type.get_ptr_to().unwrap();
                if !st_type.is_struct_type() {
                    return Err(ParseError::SemanticError(tok_seq));
                }

                let (index, expr_type) = st_type.get_struct_member(&member);

                node = ASTExpr::new(ASTExprNode::Arrow(node, index), expr_type);
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
