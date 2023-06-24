pub mod expr;

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
use crate::ast::ASTBlockStmt;
use crate::ast::ASTGlobal;
use crate::obj::Obj;
use crate::obj::ObjArena;
use crate::types::Type;

use expr::CFGBinaryOpKind;
use expr::CFGBinaryOpNode;
use expr::CFGExpr;
use expr::CFGExprNode;
use expr::CFGUnaryOpKind;
use expr::CFGUnaryOpNode;

use std::collections::HashMap;
use std::collections::VecDeque;

#[derive(Clone)]
pub enum CFGStmt {
    Decl(Obj),
    Assign(CFGExpr, CFGExpr),
    FuncCall(Option<Obj>, CFGExpr, Vec<CFGExpr>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BlockID {
    Entry,
    Return,
    Block(usize),
}

#[derive(Clone)]
pub enum CFGJump {
    None,
    Return,
    Unconditional(BlockID),
    Conditional(CFGExpr, BlockID, BlockID),
    Switch(CFGExpr, Vec<(CFGExpr, BlockID)>, BlockID),
}

#[derive(Clone)]
pub struct CFGBlock {
    pub id: BlockID,
    pub stmts: Vec<CFGStmt>,
    pub jump_to: CFGJump,
}

impl CFGBlock {
    pub fn new(id: BlockID) -> Self {
        CFGBlock {
            id,
            stmts: Vec::new(),
            jump_to: CFGJump::None,
        }
    }
}

#[derive(Clone)]
pub struct CFGFunction {
    pub func_obj: Obj,
    pub args: Vec<Obj>,
    pub retval: Option<Obj>,

    pub entry_block: CFGBlock,
    pub return_block: CFGBlock,
    pub blocks: HashMap<usize, CFGBlock>,
}

impl CFGFunction {
    fn rename_jump(block: &mut CFGBlock, old_id: usize, new_id: usize) {
        match block.jump_to {
            CFGJump::Unconditional(ref mut id) => {
                if id == &BlockID::Block(old_id) {
                    *id = BlockID::Block(new_id);
                }
            }
            CFGJump::Conditional(_, ref mut then_id, ref mut else_id) => {
                if then_id == &BlockID::Block(old_id) {
                    *then_id = BlockID::Block(new_id);
                }
                if else_id == &BlockID::Block(old_id) {
                    *else_id = BlockID::Block(new_id);
                }
            }
            CFGJump::Switch(_, ref mut cases, ref mut else_id) => {
                for (_, case) in cases.iter_mut() {
                    if case == &BlockID::Block(old_id) {
                        *case = BlockID::Block(new_id);
                    }
                }
                if else_id == &BlockID::Block(old_id) {
                    *else_id = BlockID::Block(new_id);
                }
            }
            _ => (),
        }
    }

    fn rename_block_id(&mut self, old_id: usize, new_id: usize) {
        assert!(self.blocks.contains_key(&old_id));
        assert!(!self.blocks.contains_key(&new_id));

        for (_, block) in self.blocks.iter_mut() {
            CFGFunction::rename_jump(block, old_id, new_id);
        }

        let mut new_block = self.blocks.remove(&old_id).unwrap();
        new_block.id = BlockID::Block(new_id);

        self.blocks.insert(new_id, new_block);
    }

    pub fn cleanup_unreachable_block(&mut self) {
        let size = self.blocks.len();

        let mut visit = vec![false; size];
        let mut queue = VecDeque::new();

        match self.entry_block.jump_to {
            CFGJump::Unconditional(ref id) => match id.clone() {
                BlockID::Block(num) => {
                    visit[num] = true;
                    queue.push_back(num);
                }
                _ => (),
            },
            CFGJump::Conditional(_, ref then_id, ref else_id) => {
                match then_id.clone() {
                    BlockID::Block(num) => {
                        visit[num] = true;
                        queue.push_back(num);
                    }
                    _ => (),
                }
                match else_id.clone() {
                    BlockID::Block(num) => {
                        visit[num] = true;
                        queue.push_back(num);
                    }
                    _ => (),
                }
            }
            _ => panic!(),
        }

        // bfs

        while !queue.is_empty() {
            let now = queue.pop_front().unwrap();

            let now_block = self.blocks.get(&now).unwrap();

            match now_block.jump_to {
                CFGJump::Unconditional(ref id) => match id.clone() {
                    BlockID::Block(num) => {
                        if !visit[num] {
                            visit[num] = true;
                            queue.push_back(num);
                        }
                    }
                    _ => (),
                },
                CFGJump::Conditional(_, ref then_id, ref else_id) => {
                    match then_id.clone() {
                        BlockID::Block(num) => {
                            if !visit[num] {
                                visit[num] = true;
                                queue.push_back(num);
                            }
                        }
                        _ => (),
                    }
                    match else_id.clone() {
                        BlockID::Block(num) => {
                            if !visit[num] {
                                visit[num] = true;
                                queue.push_back(num);
                            }
                        }
                        _ => (),
                    }
                }
                CFGJump::Switch(_, ref cases, ref default) => {
                    for (_, case_to) in cases.iter() {
                        match case_to.clone() {
                            BlockID::Block(num) => {
                                if !visit[num] {
                                    visit[num] = true;
                                    queue.push_back(num);
                                }
                            }
                            _ => (),
                        }
                    }
                    match default.clone() {
                        BlockID::Block(num) => {
                            if !visit[num] {
                                visit[num] = true;
                                queue.push_back(num);
                            }
                        }
                        _ => (),
                    }
                }
                _ => panic!(),
            }
        }

        // remove unreachable block

        let mut next_id = 0;

        for index in 0..size {
            if !visit[index] {
                self.blocks.remove(&index);
            } else {
                if index != next_id {
                    self.rename_block_id(index, next_id);
                }
                next_id += 1;
            }
        }
    }
}

#[derive(Clone)]
pub enum CFGGlobal {
    Function(CFGFunction),
    Variable(Obj),
}

pub fn gen_cfg_all(obj_arena: &mut ObjArena, ast_all: &Vec<ASTGlobal>) -> Vec<CFGGlobal> {
    let mut cfg_globals = Vec::new();
    for ast in ast_all.iter() {
        let cfg_global = match ast {
            ASTGlobal::Function(ref func_obj, ref args, ref stmts) => {
                let mut arena = CFGArena::new(obj_arena);
                CFGGlobal::Function(arena.gen_cfg_function(func_obj, args, stmts))
            }
            ASTGlobal::Variable(ref obj) => CFGGlobal::Variable(obj.clone()),
        };

        cfg_globals.push(cfg_global);
    }

    cfg_globals
}

struct CFGArena<'a> {
    obj_arena: &'a mut ObjArena,
    retval: Option<Obj>,

    entry_block: CFGBlock,
    return_block: CFGBlock,
    blocks: HashMap<usize, CFGBlock>,

    break_map: HashMap<usize, BlockID>,
    continue_map: HashMap<usize, BlockID>,
    default_map: HashMap<usize, BlockID>,
    case_map: HashMap<usize, Vec<(CFGExpr, BlockID)>>,

    current_id: usize,
    next_id: usize,
    current_stmts: Vec<CFGStmt>,
}

impl<'a> CFGArena<'a> {
    pub fn new(obj_arena: &'a mut ObjArena) -> Self {
        CFGArena {
            obj_arena,
            retval: None,
            entry_block: CFGBlock::new(BlockID::Entry),
            return_block: CFGBlock::new(BlockID::Return),
            blocks: HashMap::new(),
            break_map: HashMap::new(),
            continue_map: HashMap::new(),
            default_map: HashMap::new(),
            case_map: HashMap::new(),
            current_id: 0,
            next_id: 1,
            current_stmts: Vec::new(),
        }
    }

    pub fn gen_cfg_function(
        &mut self,
        func_obj: &Obj,
        args: &Vec<Obj>,
        stmts: &Vec<ASTBlockStmt>,
    ) -> CFGFunction {
        if !func_obj
            .borrow()
            .obj_type
            .get_return_type()
            .unwrap()
            .is_void_type()
        {
            let retval = self.obj_arena.publish_obj(
                "retval",
                false,
                func_obj.borrow().obj_type.get_return_type().unwrap(),
            );

            self.retval = Some(retval);
        }

        self.entry_block.jump_to = CFGJump::Unconditional(BlockID::Block(self.current_id));

        for block_stmt in stmts.iter() {
            match block_stmt {
                ASTBlockStmt::Stmt(ref stmt) => self.push_stmt(stmt),
                ASTBlockStmt::Declaration(ref obj) => {
                    self.entry_block.stmts.push(CFGStmt::Decl(obj.clone()));
                }
            }
        }

        let last_block = CFGBlock {
            id: BlockID::Block(self.current_id),
            stmts: self.current_stmts.clone(),
            jump_to: CFGJump::Unconditional(BlockID::Return),
        };

        self.blocks.insert(self.current_id, last_block);
        self.current_id = self.next_id;
        self.next_id += 1;
        self.current_stmts = Vec::new();

        self.return_block.jump_to = CFGJump::Return;

        let mut cfg_func = CFGFunction {
            func_obj: func_obj.clone(),
            args: args.clone(),
            retval: self.retval.clone(),
            entry_block: self.entry_block.clone(),
            return_block: self.return_block.clone(),
            blocks: self.blocks.clone(),
        };

        cfg_func.cleanup_unreachable_block();

        cfg_func
    }

    pub fn push_expr(&mut self, expr: &ASTExpr) -> CFGExpr {
        match expr.get_node() {
            ASTExprNode::Cast(ref ty, ref cast_expr) => {
                let evaluated_expr = self.push_expr(cast_expr);
                CFGExpr::new(CFGExprNode::Cast(ty.clone(), evaluated_expr), ty.clone())
            }
            ASTExprNode::Number(num) => {
                CFGExpr::new(CFGExprNode::Number(num), expr.expr_type.clone())
            }
            ASTExprNode::StrLiteral(ref s) => {
                CFGExpr::new(CFGExprNode::StrLiteral(s.clone()), expr.expr_type.clone())
            }
            ASTExprNode::Var(ref obj) => {
                CFGExpr::new(CFGExprNode::Var(obj.clone()), expr.expr_type.clone())
            }
            ASTExprNode::Dot(ref dot_expr, index) => {
                let evaluated_expr = self.push_expr(dot_expr);
                CFGExpr::new(
                    CFGExprNode::Dot(evaluated_expr, index),
                    expr.expr_type.clone(),
                )
            }
            ASTExprNode::Arrow(ref arrow_expr, index) => {
                let evaluated_expr = self.push_expr(arrow_expr);
                CFGExpr::new(
                    CFGExprNode::Arrow(evaluated_expr, index),
                    expr.expr_type.clone(),
                )
            }
            ASTExprNode::Assign(ref node) => self.push_assign(node, &expr.expr_type),
            ASTExprNode::UnaryOp(ref node) => self.push_unary_op(node, &expr.expr_type),
            ASTExprNode::BinaryOp(ref node) => self.push_binary_op(node, &expr.expr_type),
            _ => todo!(),
        }
    }

    pub fn push_assign(&mut self, node: &ASTAssignNode, _expr_type: &Type) -> CFGExpr {
        match node.kind {
            ASTAssignKind::Assign => {
                let lhs = self.push_expr(&node.lhs);
                let rhs = self.push_expr(&node.rhs);

                self.current_stmts.push(CFGStmt::Assign(lhs.clone(), rhs));
                lhs
            }
            _ => todo!(),
        }
    }

    pub fn push_unary_op(&mut self, node: &ASTUnaryOpNode, expr_type: &Type) -> CFGExpr {
        match node.kind {
            ASTUnaryOpKind::Plus
            | ASTUnaryOpKind::Minus
            | ASTUnaryOpKind::Addr
            | ASTUnaryOpKind::Deref
            | ASTUnaryOpKind::LogicalNot
            | ASTUnaryOpKind::BitNot => {
                let expr = self.push_expr(&node.expr);

                let kind = match node.kind {
                    ASTUnaryOpKind::Plus => CFGUnaryOpKind::Plus,
                    ASTUnaryOpKind::Minus => CFGUnaryOpKind::Minus,
                    ASTUnaryOpKind::Addr => CFGUnaryOpKind::Addr,
                    ASTUnaryOpKind::Deref => CFGUnaryOpKind::Deref,
                    ASTUnaryOpKind::LogicalNot => CFGUnaryOpKind::LogicalNot,
                    ASTUnaryOpKind::BitNot => CFGUnaryOpKind::BitNot,
                    _ => panic!(),
                };

                CFGExpr::new(
                    CFGExprNode::UnaryOp(CFGUnaryOpNode { expr, kind }),
                    expr_type.clone(),
                )
            }
            ASTUnaryOpKind::Sizeof => CFGExpr::new(
                CFGExprNode::Sizeof(node.expr.expr_type.clone()),
                expr_type.clone(),
            ),
            ASTUnaryOpKind::Alignof => CFGExpr::new(
                CFGExprNode::Alignof(node.expr.expr_type.clone()),
                expr_type.clone(),
            ),
        }
    }

    pub fn push_binary_op(&mut self, node: &ASTBinaryOpNode, expr_type: &Type) -> CFGExpr {
        match node.kind {
            ASTBinaryOpKind::Add
            | ASTBinaryOpKind::Sub
            | ASTBinaryOpKind::Mul
            | ASTBinaryOpKind::Div
            | ASTBinaryOpKind::Mod
            | ASTBinaryOpKind::BitOr
            | ASTBinaryOpKind::BitXor
            | ASTBinaryOpKind::BitAnd
            | ASTBinaryOpKind::Equal
            | ASTBinaryOpKind::NotEqual
            | ASTBinaryOpKind::Less
            | ASTBinaryOpKind::LessEqual
            | ASTBinaryOpKind::Greater
            | ASTBinaryOpKind::GreaterEqual
            | ASTBinaryOpKind::LeftShift
            | ASTBinaryOpKind::RightShift => {
                let lhs = self.push_expr(&node.lhs);
                let rhs = self.push_expr(&node.rhs);

                let kind = match node.kind {
                    ASTBinaryOpKind::Add => CFGBinaryOpKind::Add,
                    ASTBinaryOpKind::Sub => CFGBinaryOpKind::Sub,
                    ASTBinaryOpKind::Mul => CFGBinaryOpKind::Mul,
                    ASTBinaryOpKind::Div => CFGBinaryOpKind::Div,
                    ASTBinaryOpKind::Mod => CFGBinaryOpKind::Mod,
                    ASTBinaryOpKind::BitOr => CFGBinaryOpKind::BitOr,
                    ASTBinaryOpKind::BitXor => CFGBinaryOpKind::BitXor,
                    ASTBinaryOpKind::BitAnd => CFGBinaryOpKind::BitAnd,
                    ASTBinaryOpKind::Equal => CFGBinaryOpKind::Equal,
                    ASTBinaryOpKind::NotEqual => CFGBinaryOpKind::NotEqual,
                    ASTBinaryOpKind::Less => CFGBinaryOpKind::Less,
                    ASTBinaryOpKind::LessEqual => CFGBinaryOpKind::LessEqual,
                    ASTBinaryOpKind::Greater => CFGBinaryOpKind::Greater,
                    ASTBinaryOpKind::GreaterEqual => CFGBinaryOpKind::GreaterEqual,
                    ASTBinaryOpKind::LeftShift => CFGBinaryOpKind::LeftShift,
                    ASTBinaryOpKind::RightShift => CFGBinaryOpKind::RightShift,
                    _ => panic!(),
                };

                CFGExpr::new(
                    CFGExprNode::BinaryOp(CFGBinaryOpNode { lhs, rhs, kind }),
                    expr_type.clone(),
                )
            }
            _ => todo!(),
        }
    }

    pub fn push_stmt(&mut self, stmt: &ASTStmt) {
        match stmt.get_node() {
            ASTStmtNode::ExprStmt(ref expr) => {
                self.push_expr(expr);
            }
            ASTStmtNode::Default(ref stmt, switch_id) => {
                let block_id = self.next_id;
                self.next_id += 1;

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Block(block_id)),
                };

                self.blocks.insert(self.current_id, block);
                self.default_map.insert(switch_id, BlockID::Block(block_id));

                // stmt
                self.current_id = block_id;
                self.current_stmts = Vec::new();

                self.push_stmt(stmt);
            }
            ASTStmtNode::Case(ref expr, ref stmt, switch_id) => {
                let evaluated_expr = self.push_expr(expr);

                let block_id = self.next_id;
                self.next_id += 1;

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Block(block_id)),
                };

                self.blocks.insert(self.current_id, block);
                self.case_map
                    .get_mut(&switch_id)
                    .unwrap()
                    .push((evaluated_expr, BlockID::Block(block_id)));

                // stmt
                self.current_id = block_id;
                self.current_stmts = Vec::new();

                self.push_stmt(stmt);
            }
            ASTStmtNode::Block(ref stmts) => {
                for block_stmt in stmts.iter() {
                    match block_stmt {
                        ASTBlockStmt::Stmt(ref stmt) => self.push_stmt(stmt),
                        ASTBlockStmt::Declaration(ref obj) => {
                            self.entry_block.stmts.push(CFGStmt::Decl(obj.clone()));
                        }
                    }
                }
            }
            ASTStmtNode::Return(ref expr) => {
                if expr.is_some() {
                    let expr = expr.as_ref().unwrap();
                    let retval = self.retval.clone().unwrap();

                    let retval_expr = CFGExpr::new(
                        CFGExprNode::Var(retval.clone()),
                        retval.borrow().obj_type.clone(),
                    );

                    let evaluated_expr = self.push_expr(expr);

                    self.current_stmts
                        .push(CFGStmt::Assign(retval_expr, evaluated_expr));
                }

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Return),
                };

                self.blocks.insert(self.current_id, block);
                self.current_id = self.next_id;
                self.next_id += 1;
                self.current_stmts = Vec::new();
            }
            ASTStmtNode::If(ref cond, ref then_stmt, ref else_stmt) => {
                let evaluated_cond = self.push_expr(cond);

                if else_stmt.is_some() {
                    let else_stmt = else_stmt.as_ref().unwrap();

                    let then_id = self.next_id;
                    let else_id = self.next_id + 1;
                    let after_id = self.next_id + 2;
                    self.next_id += 3;

                    let block = CFGBlock {
                        id: BlockID::Block(self.current_id),
                        stmts: self.current_stmts.clone(),
                        jump_to: CFGJump::Conditional(
                            evaluated_cond,
                            BlockID::Block(then_id),
                            BlockID::Block(else_id),
                        ),
                    };

                    self.blocks.insert(self.current_id, block);

                    // then_stmt
                    self.current_id = then_id;
                    self.current_stmts = Vec::new();

                    self.push_stmt(then_stmt);

                    let block = CFGBlock {
                        id: BlockID::Block(self.current_id),
                        stmts: self.current_stmts.clone(),
                        jump_to: CFGJump::Unconditional(BlockID::Block(after_id)),
                    };

                    self.blocks.insert(self.current_id, block);

                    // else_stmt
                    self.current_id = else_id;
                    self.current_stmts = Vec::new();

                    self.push_stmt(else_stmt);

                    let block = CFGBlock {
                        id: BlockID::Block(self.current_id),
                        stmts: self.current_stmts.clone(),
                        jump_to: CFGJump::Unconditional(BlockID::Block(after_id)),
                    };

                    self.blocks.insert(self.current_id, block);

                    // after
                    self.current_id = after_id;
                    self.current_stmts = Vec::new();
                } else {
                    let then_id = self.next_id;
                    let after_id = self.next_id + 1;
                    self.next_id += 2;

                    let block = CFGBlock {
                        id: BlockID::Block(self.current_id),
                        stmts: self.current_stmts.clone(),
                        jump_to: CFGJump::Conditional(
                            evaluated_cond,
                            BlockID::Block(then_id),
                            BlockID::Block(after_id),
                        ),
                    };

                    self.blocks.insert(self.current_id, block);

                    // then_stmt
                    self.current_id = then_id;
                    self.current_stmts = Vec::new();

                    self.push_stmt(then_stmt);

                    let block = CFGBlock {
                        id: BlockID::Block(self.current_id),
                        stmts: self.current_stmts.clone(),
                        jump_to: CFGJump::Unconditional(BlockID::Block(after_id)),
                    };

                    self.blocks.insert(self.current_id, block);

                    // after
                    self.current_id = after_id;
                    self.current_stmts = Vec::new();
                }
            }
            ASTStmtNode::Switch(ref switch_node) => {
                let evaluated_cond = self.push_expr(&switch_node.cond);

                let stmt_id = self.next_id;
                let after_id = self.next_id + 1;
                self.next_id += 2;

                self.break_map
                    .insert(switch_node.break_id, BlockID::Block(after_id));
                self.case_map.insert(switch_node.switch_id, Vec::new());

                let previous_id = self.current_id;
                let previous_stmts = self.current_stmts.clone();

                // stmt
                self.current_id = stmt_id;
                self.current_stmts = Vec::new();

                self.push_stmt(&switch_node.stmt);

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Block(after_id)),
                };
                self.blocks.insert(self.current_id, block);

                // switch jump

                let cases = self.case_map.remove(&switch_node.switch_id).unwrap();
                let default_block = self
                    .default_map
                    .remove(&switch_node.switch_id)
                    .unwrap_or(BlockID::Block(after_id));

                let block = CFGBlock {
                    id: BlockID::Block(previous_id),
                    stmts: previous_stmts,
                    jump_to: CFGJump::Switch(evaluated_cond, cases, default_block),
                };

                self.blocks.insert(previous_id, block);

                // after
                self.current_id = after_id;
                self.current_stmts = Vec::new();
            }
            ASTStmtNode::While(ref while_node) => {
                let evaluated_cond = self.push_expr(&while_node.cond);

                let cond_id = self.next_id;
                let loop_id = self.next_id + 1;
                let after_id = self.next_id + 2;
                self.next_id += 3;

                self.break_map
                    .insert(while_node.break_id, BlockID::Block(after_id));
                self.continue_map
                    .insert(while_node.continue_id, BlockID::Block(cond_id));

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Block(cond_id)),
                };

                self.blocks.insert(self.current_id, block);

                // cond
                self.current_id = cond_id;
                self.current_stmts = Vec::new();

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Conditional(
                        evaluated_cond,
                        BlockID::Block(loop_id),
                        BlockID::Block(after_id),
                    ),
                };

                self.blocks.insert(self.current_id, block);

                // loop
                self.current_id = loop_id;
                self.current_stmts = Vec::new();

                self.push_stmt(&while_node.loop_stmt);

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Block(cond_id)),
                };
                self.blocks.insert(self.current_id, block);

                // after
                self.current_id = after_id;
                self.current_stmts = Vec::new();
            }
            ASTStmtNode::DoWhile(ref dowhile_node) => {
                let evaluated_cond = self.push_expr(&dowhile_node.cond);

                let loop_id = self.next_id;
                let cond_id = self.next_id + 1;
                let after_id = self.next_id + 2;
                self.next_id += 3;

                self.break_map
                    .insert(dowhile_node.break_id, BlockID::Block(after_id));
                self.continue_map
                    .insert(dowhile_node.continue_id, BlockID::Block(cond_id));

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Block(loop_id)),
                };

                self.blocks.insert(self.current_id, block);

                // loop
                self.current_id = loop_id;
                self.current_stmts = Vec::new();

                self.push_stmt(&dowhile_node.loop_stmt);

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Block(cond_id)),
                };
                self.blocks.insert(self.current_id, block);

                // cond
                self.current_id = cond_id;
                self.current_stmts = Vec::new();

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Conditional(
                        evaluated_cond,
                        BlockID::Block(loop_id),
                        BlockID::Block(after_id),
                    ),
                };

                self.blocks.insert(self.current_id, block);

                // after
                self.current_id = after_id;
                self.current_stmts = Vec::new();
            }
            ASTStmtNode::For(ref for_node) => {
                let cond_id = self.next_id;
                let step_id = self.next_id + 1;
                let loop_id = self.next_id + 2;
                let after_id = self.next_id + 3;
                self.next_id += 4;

                self.break_map
                    .insert(for_node.break_id, BlockID::Block(after_id));
                self.continue_map
                    .insert(for_node.continue_id, BlockID::Block(step_id));

                if for_node.start.is_some() {
                    let start = for_node.start.as_ref().unwrap();
                    self.push_expr(start);
                }

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Block(cond_id)),
                };

                self.blocks.insert(self.current_id, block);

                // cond
                self.current_id = cond_id;
                self.current_stmts = Vec::new();

                let jump_to = if for_node.cond.is_some() {
                    let evaluated_cond = self.push_expr(for_node.cond.as_ref().unwrap());

                    CFGJump::Conditional(
                        evaluated_cond,
                        BlockID::Block(loop_id),
                        BlockID::Block(after_id),
                    )
                } else {
                    CFGJump::Unconditional(BlockID::Block(loop_id))
                };

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to,
                };

                self.blocks.insert(self.current_id, block);

                // loop
                self.current_id = loop_id;
                self.current_stmts = Vec::new();

                self.push_stmt(&for_node.loop_stmt);

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Block(step_id)),
                };
                self.blocks.insert(self.current_id, block);

                // step
                self.current_id = step_id;
                self.current_stmts = Vec::new();

                if for_node.step.is_some() {
                    let step = for_node.step.as_ref().unwrap();
                    self.push_expr(step);
                }

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Block(cond_id)),
                };
                self.blocks.insert(self.current_id, block);

                // after
                self.current_id = after_id;
                self.current_stmts = Vec::new();
            }
            ASTStmtNode::Break(ref stmt_id) => {
                let break_block = self.break_map.get(stmt_id).unwrap().clone();
                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(break_block),
                };

                self.blocks.insert(self.current_id, block);
                self.current_id = self.next_id;
                self.next_id += 1;
                self.current_stmts = Vec::new();
            }
            ASTStmtNode::Continue(ref stmt_id) => {
                let continue_block = self.continue_map.get(stmt_id).unwrap().clone();

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(continue_block),
                };

                self.blocks.insert(self.current_id, block);
                self.current_id = self.next_id;
                self.next_id += 1;
                self.current_stmts = Vec::new();
            }
        }
    }
}
