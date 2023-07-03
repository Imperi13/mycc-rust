use crate::cfg::expr::CFGBinaryOpKind;
use crate::cfg::expr::CFGBinaryOpNode;
use crate::cfg::expr::CFGExpr;
use crate::cfg::expr::CFGExprNode;
use crate::cfg::expr::CFGUnaryOpKind;
use crate::cfg::expr::CFGUnaryOpNode;
use crate::cfg::CFGFunction;
use crate::cfg::CFGJump;
use crate::cfg::CFGStmt;
use crate::cfg::CFG;
use crate::const_value::ConstValue;
use crate::obj::ObjID;

use std::collections::HashMap;

impl CFG {
    pub(crate) fn eval_constant_propagation(&mut self) {
        for func in self.functions.iter_mut() {
            func.eval_constant_propagation();
        }
    }
}

impl CFGFunction {
    pub fn eval_constant_propagation(&mut self) {
        let extended_blocks = self.extended_basic_block();
        for extended_block in extended_blocks.iter() {
            let mut arena = ConstArena::new();

            for block_id in extended_block.iter() {
                let block = self.blocks.get_mut(&block_id).unwrap();

                for stmt in block.stmts.iter_mut() {
                    match stmt {
                        CFGStmt::Decl(_) => (),
                        CFGStmt::Arg(_, _) => (),
                        CFGStmt::Assign(ref lhs, ref mut rhs) => {
                            *rhs = rhs.eval_cfg_with_arena(&arena);
                            match lhs.get_node() {
                                CFGExprNode::Var(ref obj) => {
                                    if rhs.is_consteval() {
                                        arena
                                            .const_objs
                                            .insert(obj.borrow().id.clone(), rhs.eval_const());
                                    }
                                }
                                CFGExprNode::Dot(_, _) => {
                                    arena.reset();
                                }
                                CFGExprNode::Arrow(_, _) => {
                                    arena.reset();
                                }
                                CFGExprNode::Deref(_) => {
                                    arena.reset();
                                }
                                _ => panic!(),
                            }
                        }
                        CFGStmt::FuncCall(_, _, _) => {
                            arena.reset();
                        }
                    }
                }

                match block.jump_to.clone() {
                    CFGJump::Return(ret_expr) => {
                        if ret_expr.is_some() {
                            let ret_expr = ret_expr.unwrap();
                            if ret_expr.is_consteval_with_arena(&arena) {
                                block.jump_to = CFGJump::Return(Some(
                                    ret_expr.eval_const_with_arena(&arena).to_cfg(),
                                ));
                            }
                        }
                    }
                    _ => (),
                }
            }
        }
    }
}

struct ConstArena {
    pub const_objs: HashMap<ObjID, ConstValue>,
}

impl ConstArena {
    pub fn new() -> Self {
        ConstArena {
            const_objs: HashMap::new(),
        }
    }

    pub fn reset(&mut self) {
        self.const_objs = HashMap::new();
    }
}

impl CFGExpr {
    fn is_consteval_with_arena(&self, arena: &ConstArena) -> bool {
        match self.get_node() {
            CFGExprNode::Number(_) => true,
            CFGExprNode::Var(ref obj) => arena.const_objs.contains_key(&obj.borrow().id),
            CFGExprNode::BinaryOp(ref node) => match node.kind {
                CFGBinaryOpKind::Add
                | CFGBinaryOpKind::Sub
                | CFGBinaryOpKind::Mul
                | CFGBinaryOpKind::Div => {
                    node.lhs.is_consteval_with_arena(arena)
                        && node.rhs.is_consteval_with_arena(arena)
                }
                _ => false,
            },
            CFGExprNode::UnaryOp(ref node) => match node.kind {
                CFGUnaryOpKind::Plus | CFGUnaryOpKind::Minus | CFGUnaryOpKind::LogicalNot => {
                    node.expr.is_consteval_with_arena(arena)
                }
                _ => false,
            },
            _ => false,
        }
    }

    fn eval_const_with_arena(&self, arena: &ConstArena) -> ConstValue {
        assert!(self.is_consteval_with_arena(arena));
        match self.get_node() {
            CFGExprNode::Number(num) => ConstValue::Integer(num as i64),
            CFGExprNode::Var(ref obj) => arena.const_objs.get(&obj.borrow().id).unwrap().clone(),
            CFGExprNode::BinaryOp(ref node) => node.eval_const_with_arena(arena),
            CFGExprNode::UnaryOp(ref node) => node.eval_const_with_arena(arena),
            _ => panic!(),
        }
    }

    fn eval_cfg_with_arena(&self, arena: &ConstArena) -> CFGExpr {
        if self.is_consteval_with_arena(arena) {
            let const_val = self.eval_const_with_arena(arena);
            const_val.to_cfg()
        } else {
            match self.get_node() {
                CFGExprNode::Number(num) => {
                    CFGExpr::new(CFGExprNode::Number(num), self.expr_type.clone())
                }
                CFGExprNode::Var(ref obj) => {
                    CFGExpr::new(CFGExprNode::Var(obj.clone()), self.expr_type.clone())
                }
                CFGExprNode::Sizeof(ref ty) => {
                    CFGExpr::new(CFGExprNode::Sizeof(ty.clone()), self.expr_type.clone())
                }
                CFGExprNode::Alignof(ref ty) => {
                    CFGExpr::new(CFGExprNode::Alignof(ty.clone()), self.expr_type.clone())
                }
                CFGExprNode::StrLiteral(ref text) => CFGExpr::new(
                    CFGExprNode::StrLiteral(text.clone()),
                    self.expr_type.clone(),
                ),
                CFGExprNode::Cast(ref ty, ref expr) => CFGExpr::new(
                    CFGExprNode::Cast(ty.clone(), expr.eval_cfg_with_arena(arena)),
                    self.expr_type.clone(),
                ),
                CFGExprNode::Deref(ref expr) => CFGExpr::new(
                    CFGExprNode::Deref(expr.eval_cfg_with_arena(arena)),
                    self.expr_type.clone(),
                ),
                CFGExprNode::Dot(ref expr, index) => CFGExpr::new(
                    CFGExprNode::Dot(expr.eval_cfg_with_arena(arena), index),
                    self.expr_type.clone(),
                ),
                CFGExprNode::Arrow(ref expr, index) => CFGExpr::new(
                    CFGExprNode::Arrow(expr.eval_cfg_with_arena(arena), index),
                    self.expr_type.clone(),
                ),
                CFGExprNode::UnaryOp(ref node) => CFGExpr::new(
                    CFGExprNode::UnaryOp(CFGUnaryOpNode {
                        kind: node.kind.clone(),
                        expr: node.expr.eval_cfg_with_arena(arena),
                    }),
                    self.expr_type.clone(),
                ),
                CFGExprNode::BinaryOp(ref node) => CFGExpr::new(
                    CFGExprNode::BinaryOp(CFGBinaryOpNode {
                        kind: node.kind.clone(),
                        lhs: node.lhs.eval_cfg_with_arena(arena),
                        rhs: node.rhs.eval_cfg_with_arena(arena),
                    }),
                    self.expr_type.clone(),
                ),
            }
        }
    }
}

impl CFGBinaryOpNode {
    fn eval_const_with_arena(&self, arena: &ConstArena) -> ConstValue {
        let lhs = self.lhs.eval_const_with_arena(arena);
        let rhs = self.rhs.eval_const_with_arena(arena);
        match self.kind {
            CFGBinaryOpKind::Add => ConstValue::const_value_add(lhs, rhs),
            CFGBinaryOpKind::Sub => ConstValue::const_value_sub(lhs, rhs),
            CFGBinaryOpKind::Mul => ConstValue::const_value_mul(lhs, rhs),
            CFGBinaryOpKind::Div => ConstValue::const_value_div(lhs, rhs),
            _ => todo!(),
        }
    }
}

impl CFGUnaryOpNode {
    fn eval_const_with_arena(&self, arena: &ConstArena) -> ConstValue {
        let expr = self.expr.eval_const_with_arena(arena);
        match self.kind {
            CFGUnaryOpKind::Plus => ConstValue::const_value_plus(expr),
            CFGUnaryOpKind::Minus => ConstValue::const_value_minus(expr),
            CFGUnaryOpKind::LogicalNot => ConstValue::const_value_logical_not(expr),
            _ => todo!(),
        }
    }
}
