use crate::cfg::expr::CFGBinaryOpKind;
use crate::cfg::expr::CFGBinaryOpNode;
use crate::cfg::expr::CFGExpr;
use crate::cfg::expr::CFGExprNode;
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
            CFGExprNode::BinaryOp(ref node) => {
                node.lhs.is_consteval_with_arena(arena) && node.rhs.is_consteval_with_arena(arena)
            }
            _ => false,
        }
    }

    fn eval_const_with_arena(&self, arena: &ConstArena) -> ConstValue {
        assert!(self.is_consteval_with_arena(arena));
        match self.get_node() {
            CFGExprNode::Number(num) => ConstValue::Integer(num as i64),
            CFGExprNode::Var(ref obj) => arena.const_objs.get(&obj.borrow().id).unwrap().clone(),
            CFGExprNode::BinaryOp(ref node) => node.eval_const_with_arena(arena),
            _ => panic!(),
        }
    }

    fn is_const_zero_with_arena(&self, arena: &ConstArena) -> bool {
        self.is_consteval_with_arena(arena)
            && (self.eval_const_with_arena(arena) == ConstValue::Integer(0))
    }

    fn eval_cfg_with_arena(&self, arena: &ConstArena) -> CFGExpr {
        if self.is_consteval_with_arena(arena) {
            let const_val = self.eval_const_with_arena(arena);
            const_val.to_cfg()
        } else {
            todo!()
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
