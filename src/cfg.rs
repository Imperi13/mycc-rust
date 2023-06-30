pub mod expr;
mod optimize;

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
use crate::ast::AST;
use crate::obj::GlobalObjArena;
use crate::obj::LocalObjArena;
use crate::obj::Obj;
use crate::types::Type;
use crate::types::TypeNode;

use expr::CFGBinaryOpKind;
use expr::CFGBinaryOpNode;
use expr::CFGExpr;
use expr::CFGExprNode;
use expr::CFGUnaryOpKind;
use expr::CFGUnaryOpNode;

use std::collections::HashMap;

use std::fmt;

#[derive(Clone)]
pub enum CFGStmt {
    Decl(Obj),
    Arg(Obj, usize),
    Assign(CFGExpr, CFGExpr),
    FuncCall(Option<Obj>, CFGExpr, Vec<CFGExpr>),
}

impl fmt::Debug for CFGStmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            CFGStmt::Decl(ref obj) => write!(f, "Decl {:?}", obj),
            CFGStmt::Arg(ref obj, arg_index) => write!(f, "Arg {:?} = #arg_{arg_index}", obj),
            CFGStmt::Assign(ref lhs, ref rhs) => write!(f, "{:?} = {:?}", lhs, rhs),
            CFGStmt::FuncCall(ref ret_obj, ref func_expr, ref args) => {
                if ret_obj.is_some() {
                    let ret_obj = ret_obj.as_ref().unwrap();
                    write!(f, "{:?} = {:?}(", ret_obj, func_expr)?;

                    for arg in args.iter() {
                        write!(f, "{:?},", arg)?;
                    }
                    write!(f, ")")
                } else {
                    write!(f, "{:?}( args )", func_expr)
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BlockKind {
    Entry,
    Return,
    Node,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BlockID(usize);
impl BlockID {
    pub fn new(i: usize) -> Self {
        Self(i)
    }
    pub fn to_usize(&self) -> usize {
        self.0
    }
}

#[derive(Clone)]
pub enum CFGJump {
    Return(Option<Obj>),
    Unconditional(BlockID),
    Conditional(CFGExpr, BlockID, BlockID),
    Switch(CFGExpr, Vec<(CFGExpr, BlockID)>, BlockID),
}

impl fmt::Debug for CFGJump {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            CFGJump::Return(ref retval) => write!(f, "Return({:?})", retval),
            CFGJump::Unconditional(ref block) => write!(f, "Unconditional {:?}", block),
            CFGJump::Conditional(ref cond, ref then_block, ref else_block) => write!(
                f,
                "Conditional ({:?}) then: {:?} else: {:?}",
                cond, then_block, else_block
            ),
            CFGJump::Switch(ref cond, ref cases, ref else_block) => {
                write!(f, "Switch ({:?}) cases: ", cond)?;
                for (case, block) in cases.iter() {
                    write!(f, "({:?},{:?}) ", case, block)?;
                }
                write!(f, "else: {:?}", else_block)
            }
        }
    }
}

#[derive(Clone)]
pub struct CFGBlock {
    pub kind: BlockKind,
    pub id: BlockID,
    pub stmts: Vec<CFGStmt>,
    pub jump_to: CFGJump,
}

impl fmt::Debug for CFGBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Block {:?}:{:?}", self.id, self.kind)?;
        writeln!(f, "Stmts:")?;
        for stmt in self.stmts.iter() {
            writeln!(f, "\t{:?}", stmt)?;
        }

        writeln!(f, "Jump: {:?}", self.jump_to)
    }
}

#[derive(Clone)]
pub struct CFGFunction {
    pub func_obj: Obj,
    pub args: Vec<Obj>,
    pub retval: Option<Obj>,

    pub entry_id: BlockID,
    pub return_id: BlockID,
    pub blocks: HashMap<BlockID, CFGBlock>,

    pub obj_arena: LocalObjArena,
}

impl fmt::Debug for CFGFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "--------------")?;
        writeln!(f, "Function {}:", self.func_obj.borrow().name)?;
        write!(f, "args: ")?;
        for arg in self.args.iter() {
            write!(f, "{:?},", arg)?;
        }
        writeln!(f, "")?;
        if self.retval.is_some() {
            let retval = self.retval.as_ref().unwrap();
            writeln!(f, "return: {:?}", retval)?;
        }
        writeln!(f, "")?;

        for (_, block) in self.blocks.iter() {
            writeln!(f, "{:?}", block)?;
        }
        writeln!(f, "--------------")
    }
}

#[derive(Clone)]
pub struct CFG {
    pub global_objs: Vec<Obj>,
    pub variables: Vec<Obj>,
    pub functions: Vec<CFGFunction>,

    pub obj_arena: GlobalObjArena,
}

impl fmt::Debug for CFG {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for func in self.functions.iter() {
            writeln!(f, "{:?}", func)?;
        }
        Ok(())
    }
}

pub fn gen_cfg_all(ast: &AST) -> CFG {
    let global_objs = ast.global_objs.clone();
    let variables = ast.variables.clone();
    let obj_arena = ast.obj_arena.clone();
    let mut functions = Vec::new();
    for ast_func in ast.functions.iter() {
        let func_obj = ast_func.func_obj.clone();
        let args_obj = ast_func.args_obj.clone();
        let obj_arena = ast_func.obj_arena.clone();

        let mut arena = CFGArena::new(obj_arena);
        functions.push(arena.gen_cfg_function(&func_obj, &args_obj, &ast_func.stmts));
    }

    CFG {
        global_objs,
        variables,
        functions,
        obj_arena,
    }
}

struct CFGArena {
    obj_arena: LocalObjArena,
    retval: Option<Obj>,

    entry_id: BlockID,
    return_id: BlockID,
    blocks: HashMap<BlockID, CFGBlock>,

    break_map: HashMap<usize, BlockID>,
    continue_map: HashMap<usize, BlockID>,
    default_map: HashMap<usize, BlockID>,
    case_map: HashMap<usize, Vec<(CFGExpr, BlockID)>>,

    current_id: usize,
    next_id: usize,
    current_stmts: Vec<CFGStmt>,
}

impl CFGArena {
    pub fn new(obj_arena: LocalObjArena) -> Self {
        CFGArena {
            obj_arena,
            retval: None,
            entry_id: BlockID::new(0),
            return_id: BlockID::new(1),
            blocks: HashMap::new(),
            break_map: HashMap::new(),
            continue_map: HashMap::new(),
            default_map: HashMap::new(),
            case_map: HashMap::new(),
            current_id: 2,
            next_id: 3,
            current_stmts: Vec::new(),
        }
    }

    pub fn gen_cfg_function(
        &mut self,
        func_obj: &Obj,
        args: &Vec<Obj>,
        stmts: &Vec<ASTBlockStmt>,
    ) -> CFGFunction {
        self.entry_id = BlockID::new(0);
        self.return_id = BlockID::new(1);
        self.current_stmts = Vec::new();
        self.current_id = 2;
        self.next_id = 3;

        if !func_obj
            .borrow()
            .obj_type
            .get_return_type()
            .unwrap()
            .is_void_type()
        {
            let retval = self.obj_arena.publish_obj(
                "retval",
                func_obj.borrow().obj_type.get_return_type().unwrap(),
            );

            self.current_stmts.push(CFGStmt::Decl(retval.clone()));
            self.retval = Some(retval);
        }

        for (arg_index, arg_obj) in args.iter().enumerate() {
            self.current_stmts.push(CFGStmt::Decl(arg_obj.clone()));
            self.current_stmts
                .push(CFGStmt::Arg(arg_obj.clone(), arg_index));
        }

        let entry_block = CFGBlock {
            kind: BlockKind::Entry,
            id: self.entry_id.clone(),
            stmts: self.current_stmts.clone(),
            jump_to: CFGJump::Unconditional(BlockID(self.current_id)),
        };
        self.blocks.insert(self.entry_id.clone(), entry_block);

        self.current_stmts = Vec::new();

        let return_block = CFGBlock {
            kind: BlockKind::Return,
            id: self.return_id.clone(),
            stmts: self.current_stmts.clone(),
            jump_to: CFGJump::Return(self.retval.clone()),
        };
        self.blocks.insert(self.return_id.clone(), return_block);

        for block_stmt in stmts.iter() {
            match block_stmt {
                ASTBlockStmt::Stmt(ref stmt) => self.push_stmt(stmt),
                ASTBlockStmt::Declaration(ref obj) => {
                    self.current_stmts.push(CFGStmt::Decl(obj.clone()));
                }
            }
        }

        let last_block = CFGBlock {
            kind: BlockKind::Node,
            id: BlockID::new(self.current_id),
            stmts: self.current_stmts.clone(),
            jump_to: CFGJump::Unconditional(self.return_id.clone()),
        };

        self.blocks
            .insert(BlockID::new(self.current_id), last_block);

        let mut cfg_func = CFGFunction {
            func_obj: func_obj.clone(),
            args: args.clone(),
            retval: self.retval.clone(),
            entry_id: self.entry_id.clone(),
            return_id: self.return_id.clone(),
            blocks: self.blocks.clone(),
            obj_arena: self.obj_arena.clone(),
        };

        cfg_func.cleanup_unreachable_block();
        cfg_func.move_declaration_to_entry();

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
            ASTExprNode::Conditional(ref cond, ref then_expr, ref else_expr) => {
                let conditional_obj = self
                    .obj_arena
                    .publish_obj("conditional", expr.expr_type.clone());
                let conditional_var = CFGExpr::new(
                    CFGExprNode::Var(conditional_obj.clone()),
                    expr.expr_type.clone(),
                );

                self.current_stmts
                    .push(CFGStmt::Decl(conditional_obj.clone()));

                let evaluated_cond = self.push_expr(cond);

                let then_id = self.next_id;
                let else_id = self.next_id + 1;
                let after_id = self.next_id + 2;
                self.next_id += 3;

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Conditional(
                        evaluated_cond,
                        BlockID::new(then_id),
                        BlockID::new(else_id),
                    ),
                };

                self.blocks.insert(BlockID::new(self.current_id), block);

                // then_stmt
                self.current_id = then_id;
                self.current_stmts = Vec::new();

                let evaluated_then_expr = self.push_expr(then_expr);
                self.current_stmts.push(CFGStmt::Assign(
                    conditional_var.clone(),
                    evaluated_then_expr,
                ));

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::new(after_id)),
                };

                self.blocks.insert(BlockID::new(self.current_id), block);

                // else_stmt
                self.current_id = else_id;
                self.current_stmts = Vec::new();

                let evaluated_else_expr = self.push_expr(else_expr);
                self.current_stmts.push(CFGStmt::Assign(
                    conditional_var.clone(),
                    evaluated_else_expr,
                ));

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::new(after_id)),
                };

                self.blocks.insert(BlockID::new(self.current_id), block);

                // after
                self.current_id = after_id;
                self.current_stmts = Vec::new();

                conditional_var
            }
            ASTExprNode::PostIncrement(ref lhs) => {
                let saved_obj = self
                    .obj_arena
                    .publish_obj("post_increment", expr.expr_type.clone());
                let saved_var =
                    CFGExpr::new(CFGExprNode::Var(saved_obj.clone()), expr.expr_type.clone());

                self.current_stmts.push(CFGStmt::Decl(saved_obj.clone()));
                let lhs = self.push_expr(lhs);
                self.current_stmts
                    .push(CFGStmt::Assign(saved_var.clone(), lhs.clone()));
                let one = CFGExpr::new(CFGExprNode::Number(1), Type::new(TypeNode::Int));
                let binary_expr = CFGExpr::new(
                    CFGExprNode::BinaryOp(CFGBinaryOpNode {
                        lhs: lhs.clone(),
                        rhs: one,
                        kind: CFGBinaryOpKind::Add,
                    }),
                    expr.expr_type.clone(),
                );

                self.current_stmts.push(CFGStmt::Assign(lhs, binary_expr));
                saved_var
            }
            ASTExprNode::PostDecrement(ref lhs) => {
                let saved_obj = self
                    .obj_arena
                    .publish_obj("post_decrement", expr.expr_type.clone());
                let saved_var =
                    CFGExpr::new(CFGExprNode::Var(saved_obj.clone()), expr.expr_type.clone());

                self.current_stmts.push(CFGStmt::Decl(saved_obj.clone()));
                let lhs = self.push_expr(lhs);
                self.current_stmts
                    .push(CFGStmt::Assign(saved_var.clone(), lhs.clone()));
                let one = CFGExpr::new(CFGExprNode::Number(1), Type::new(TypeNode::Int));
                let binary_expr = CFGExpr::new(
                    CFGExprNode::BinaryOp(CFGBinaryOpNode {
                        lhs: lhs.clone(),
                        rhs: one,
                        kind: CFGBinaryOpKind::Sub,
                    }),
                    expr.expr_type.clone(),
                );

                self.current_stmts.push(CFGStmt::Assign(lhs, binary_expr));
                saved_var
            }
            ASTExprNode::FuncCall(ref func_expr, ref args) => {
                let evaluated_func_expr = self.push_expr(func_expr);
                let evaluated_args = args.iter().map(|e| self.push_expr(e)).collect();

                let return_type = func_expr.expr_type.get_return_type().unwrap();

                if return_type.is_void_type() {
                    self.current_stmts.push(CFGStmt::FuncCall(
                        None,
                        evaluated_func_expr,
                        evaluated_args,
                    ));

                    CFGExpr::new(CFGExprNode::Number(0), expr.expr_type.clone())
                } else {
                    let return_obj = self.obj_arena.publish_obj("func_call", return_type.clone());
                    self.current_stmts.push(CFGStmt::Decl(return_obj.clone()));

                    self.current_stmts.push(CFGStmt::FuncCall(
                        Some(return_obj.clone()),
                        evaluated_func_expr,
                        evaluated_args,
                    ));

                    CFGExpr::new(CFGExprNode::Var(return_obj), return_type)
                }
            }
            ASTExprNode::Assign(ref node) => self.push_assign(node, &expr.expr_type),
            ASTExprNode::UnaryOp(ref node) => self.push_unary_op(node, &expr.expr_type),
            ASTExprNode::BinaryOp(ref node) => self.push_binary_op(node, &expr.expr_type),
        }
    }

    pub fn push_assign(&mut self, node: &ASTAssignNode, expr_type: &Type) -> CFGExpr {
        match node.kind {
            ASTAssignKind::Assign => {
                let lhs = self.push_expr(&node.lhs);
                let rhs = self.push_expr(&node.rhs);

                self.current_stmts.push(CFGStmt::Assign(lhs, rhs.clone()));
                rhs
            }
            ASTAssignKind::OrAssign
            | ASTAssignKind::XorAssign
            | ASTAssignKind::AndAssign
            | ASTAssignKind::AddAssign
            | ASTAssignKind::SubAssign
            | ASTAssignKind::MulAssign
            | ASTAssignKind::DivAssign
            | ASTAssignKind::ModAssign
            | ASTAssignKind::LeftShiftAssign
            | ASTAssignKind::RightShiftAssign => {
                let compound_obj = self
                    .obj_arena
                    .publish_obj("compound_assign", expr_type.clone());
                let compound_var =
                    CFGExpr::new(CFGExprNode::Var(compound_obj.clone()), expr_type.clone());

                let lhs = self.push_expr(&node.lhs);
                let rhs = self.push_expr(&node.rhs);
                let kind = match node.kind {
                    ASTAssignKind::OrAssign => CFGBinaryOpKind::BitOr,
                    ASTAssignKind::XorAssign => CFGBinaryOpKind::BitXor,
                    ASTAssignKind::AndAssign => CFGBinaryOpKind::BitAnd,
                    ASTAssignKind::AddAssign => CFGBinaryOpKind::Add,
                    ASTAssignKind::SubAssign => CFGBinaryOpKind::Sub,
                    ASTAssignKind::MulAssign => CFGBinaryOpKind::Mul,
                    ASTAssignKind::DivAssign => CFGBinaryOpKind::Div,
                    ASTAssignKind::ModAssign => CFGBinaryOpKind::Mod,
                    ASTAssignKind::LeftShiftAssign => CFGBinaryOpKind::LeftShift,
                    ASTAssignKind::RightShiftAssign => CFGBinaryOpKind::RightShift,
                    _ => panic!(),
                };

                self.current_stmts.push(CFGStmt::Decl(compound_obj.clone()));

                let binary_expr = CFGExpr::new(
                    CFGExprNode::BinaryOp(CFGBinaryOpNode {
                        lhs: lhs.clone(),
                        rhs,
                        kind,
                    }),
                    expr_type.clone(),
                );

                self.current_stmts
                    .push(CFGStmt::Assign(compound_var.clone(), binary_expr));
                self.current_stmts
                    .push(CFGStmt::Assign(lhs, compound_var.clone()));

                compound_var
            }
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
            ASTBinaryOpKind::Comma => {
                self.push_expr(&node.lhs);
                self.push_expr(&node.rhs)
            }
            ASTBinaryOpKind::LogicalOr => {
                let or_obj = self.obj_arena.publish_obj("logical_or", expr_type.clone());
                let or_var = CFGExpr::new(CFGExprNode::Var(or_obj.clone()), expr_type.clone());

                self.current_stmts.push(CFGStmt::Decl(or_obj.clone()));

                let lhs = self.push_expr(&node.lhs);

                let rhs_id = self.next_id;
                let then_id = self.next_id + 1;
                let else_id = self.next_id + 2;
                let after_id = self.next_id + 3;
                self.next_id += 4;

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Conditional(lhs, BlockID::new(then_id), BlockID::new(rhs_id)),
                };
                self.blocks.insert(BlockID(self.current_id), block);

                // rhs
                self.current_id = rhs_id;
                self.current_stmts = Vec::new();

                let rhs = self.push_expr(&node.rhs);

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Conditional(
                        rhs,
                        BlockID::new(then_id),
                        BlockID::new(else_id),
                    ),
                };

                self.blocks.insert(BlockID::new(self.current_id), block);

                // then
                self.current_id = then_id;
                self.current_stmts = Vec::new();

                self.current_stmts.push(CFGStmt::Assign(
                    or_var.clone(),
                    CFGExpr::new(CFGExprNode::Number(1), Type::new(TypeNode::Int)),
                ));

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::new(after_id)),
                };

                self.blocks.insert(BlockID::new(self.current_id), block);

                // else
                self.current_id = else_id;
                self.current_stmts = Vec::new();

                self.current_stmts.push(CFGStmt::Assign(
                    or_var.clone(),
                    CFGExpr::new(CFGExprNode::Number(0), Type::new(TypeNode::Int)),
                ));

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::new(after_id)),
                };

                self.blocks.insert(BlockID::new(self.current_id), block);

                // after
                self.current_id = after_id;
                self.current_stmts = Vec::new();

                or_var
            }
            ASTBinaryOpKind::LogicalAnd => {
                let and_obj = self.obj_arena.publish_obj("logical_and", expr_type.clone());
                let and_var = CFGExpr::new(CFGExprNode::Var(and_obj.clone()), expr_type.clone());

                self.current_stmts.push(CFGStmt::Decl(and_obj.clone()));

                let lhs = self.push_expr(&node.lhs);

                let rhs_id = self.next_id;
                let then_id = self.next_id + 1;
                let else_id = self.next_id + 2;
                let after_id = self.next_id + 3;
                self.next_id += 4;

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Conditional(lhs, BlockID::new(rhs_id), BlockID::new(else_id)),
                };
                self.blocks.insert(BlockID::new(self.current_id), block);

                // rhs
                self.current_id = rhs_id;
                self.current_stmts = Vec::new();

                let rhs = self.push_expr(&node.rhs);

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Conditional(
                        rhs,
                        BlockID::new(then_id),
                        BlockID::new(else_id),
                    ),
                };

                self.blocks.insert(BlockID::new(self.current_id), block);

                // then
                self.current_id = then_id;
                self.current_stmts = Vec::new();

                self.current_stmts.push(CFGStmt::Assign(
                    and_var.clone(),
                    CFGExpr::new(CFGExprNode::Number(1), Type::new(TypeNode::Int)),
                ));

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::new(after_id)),
                };

                self.blocks.insert(BlockID::new(self.current_id), block);

                // else
                self.current_id = else_id;
                self.current_stmts = Vec::new();

                self.current_stmts.push(CFGStmt::Assign(
                    and_var.clone(),
                    CFGExpr::new(CFGExprNode::Number(0), Type::new(TypeNode::Int)),
                ));

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::new(after_id)),
                };

                self.blocks.insert(BlockID::new(self.current_id), block);

                // after
                self.current_id = after_id;
                self.current_stmts = Vec::new();

                and_var
            }
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
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::new(block_id)),
                };

                self.blocks.insert(BlockID::new(self.current_id), block);
                self.default_map.insert(switch_id, BlockID::new(block_id));

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
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::new(block_id)),
                };

                self.blocks.insert(BlockID::new(self.current_id), block);
                self.case_map
                    .get_mut(&switch_id)
                    .unwrap()
                    .push((evaluated_expr, BlockID::new(block_id)));

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
                            self.current_stmts.push(CFGStmt::Decl(obj.clone()));
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
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(self.return_id.clone()),
                };

                self.blocks.insert(BlockID::new(self.current_id), block);
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
                        kind: BlockKind::Node,
                        id: BlockID::new(self.current_id),
                        stmts: self.current_stmts.clone(),
                        jump_to: CFGJump::Conditional(
                            evaluated_cond,
                            BlockID::new(then_id),
                            BlockID::new(else_id),
                        ),
                    };

                    self.blocks.insert(BlockID::new(self.current_id), block);

                    // then_stmt
                    self.current_id = then_id;
                    self.current_stmts = Vec::new();

                    self.push_stmt(then_stmt);

                    let block = CFGBlock {
                        kind: BlockKind::Node,
                        id: BlockID::new(self.current_id),
                        stmts: self.current_stmts.clone(),
                        jump_to: CFGJump::Unconditional(BlockID::new(after_id)),
                    };

                    self.blocks.insert(BlockID::new(self.current_id), block);

                    // else_stmt
                    self.current_id = else_id;
                    self.current_stmts = Vec::new();

                    self.push_stmt(else_stmt);

                    let block = CFGBlock {
                        kind: BlockKind::Node,
                        id: BlockID::new(self.current_id),
                        stmts: self.current_stmts.clone(),
                        jump_to: CFGJump::Unconditional(BlockID::new(after_id)),
                    };

                    self.blocks.insert(BlockID::new(self.current_id), block);

                    // after
                    self.current_id = after_id;
                    self.current_stmts = Vec::new();
                } else {
                    let then_id = self.next_id;
                    let after_id = self.next_id + 1;
                    self.next_id += 2;

                    let block = CFGBlock {
                        kind: BlockKind::Node,
                        id: BlockID::new(self.current_id),
                        stmts: self.current_stmts.clone(),
                        jump_to: CFGJump::Conditional(
                            evaluated_cond,
                            BlockID::new(then_id),
                            BlockID::new(after_id),
                        ),
                    };

                    self.blocks.insert(BlockID::new(self.current_id), block);

                    // then_stmt
                    self.current_id = then_id;
                    self.current_stmts = Vec::new();

                    self.push_stmt(then_stmt);

                    let block = CFGBlock {
                        kind: BlockKind::Node,
                        id: BlockID::new(self.current_id),
                        stmts: self.current_stmts.clone(),
                        jump_to: CFGJump::Unconditional(BlockID::new(after_id)),
                    };

                    self.blocks.insert(BlockID::new(self.current_id), block);

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
                    .insert(switch_node.break_id, BlockID::new(after_id));
                self.case_map.insert(switch_node.switch_id, Vec::new());

                let previous_id = self.current_id;
                let previous_stmts = self.current_stmts.clone();

                // stmt
                self.current_id = stmt_id;
                self.current_stmts = Vec::new();

                self.push_stmt(&switch_node.stmt);

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::new(after_id)),
                };
                self.blocks.insert(BlockID::new(self.current_id), block);

                // switch jump

                let cases = self.case_map.remove(&switch_node.switch_id).unwrap();
                let default_block = self
                    .default_map
                    .remove(&switch_node.switch_id)
                    .unwrap_or(BlockID::new(after_id));

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(previous_id),
                    stmts: previous_stmts,
                    jump_to: CFGJump::Switch(evaluated_cond, cases, default_block),
                };

                self.blocks.insert(BlockID::new(previous_id), block);

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
                    .insert(while_node.break_id, BlockID::new(after_id));
                self.continue_map
                    .insert(while_node.continue_id, BlockID::new(cond_id));

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::new(cond_id)),
                };

                self.blocks.insert(BlockID::new(self.current_id), block);

                // cond
                self.current_id = cond_id;
                self.current_stmts = Vec::new();

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Conditional(
                        evaluated_cond,
                        BlockID::new(loop_id),
                        BlockID::new(after_id),
                    ),
                };

                self.blocks.insert(BlockID::new(self.current_id), block);

                // loop
                self.current_id = loop_id;
                self.current_stmts = Vec::new();

                self.push_stmt(&while_node.loop_stmt);

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::new(cond_id)),
                };
                self.blocks.insert(BlockID::new(self.current_id), block);

                // after
                self.current_id = after_id;
                self.current_stmts = Vec::new();
            }
            ASTStmtNode::DoWhile(ref dowhile_node) => {
                let loop_id = self.next_id;
                let cond_id = self.next_id + 1;
                let after_id = self.next_id + 2;
                self.next_id += 3;

                self.break_map
                    .insert(dowhile_node.break_id, BlockID::new(after_id));
                self.continue_map
                    .insert(dowhile_node.continue_id, BlockID::new(cond_id));

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::new(loop_id)),
                };

                self.blocks.insert(BlockID::new(self.current_id), block);

                // loop
                self.current_id = loop_id;
                self.current_stmts = Vec::new();

                self.push_stmt(&dowhile_node.loop_stmt);

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::new(cond_id)),
                };
                self.blocks.insert(BlockID::new(self.current_id), block);

                // cond
                self.current_id = cond_id;
                self.current_stmts = Vec::new();

                let evaluated_cond = self.push_expr(&dowhile_node.cond);

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Conditional(
                        evaluated_cond,
                        BlockID::new(loop_id),
                        BlockID::new(after_id),
                    ),
                };

                self.blocks.insert(BlockID::new(self.current_id), block);

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
                    .insert(for_node.break_id, BlockID::new(after_id));
                self.continue_map
                    .insert(for_node.continue_id, BlockID::new(step_id));

                if for_node.start.is_some() {
                    let start = for_node.start.as_ref().unwrap();
                    self.push_expr(start);
                }

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::new(cond_id)),
                };

                self.blocks.insert(BlockID::new(self.current_id), block);

                // cond
                self.current_id = cond_id;
                self.current_stmts = Vec::new();

                let jump_to = if for_node.cond.is_some() {
                    let evaluated_cond = self.push_expr(for_node.cond.as_ref().unwrap());

                    CFGJump::Conditional(
                        evaluated_cond,
                        BlockID::new(loop_id),
                        BlockID::new(after_id),
                    )
                } else {
                    CFGJump::Unconditional(BlockID::new(loop_id))
                };

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to,
                };

                self.blocks.insert(BlockID::new(self.current_id), block);

                // loop
                self.current_id = loop_id;
                self.current_stmts = Vec::new();

                self.push_stmt(&for_node.loop_stmt);

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::new(step_id)),
                };
                self.blocks.insert(BlockID::new(self.current_id), block);

                // step
                self.current_id = step_id;
                self.current_stmts = Vec::new();

                if for_node.step.is_some() {
                    let step = for_node.step.as_ref().unwrap();
                    self.push_expr(step);
                }

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::new(cond_id)),
                };
                self.blocks.insert(BlockID::new(self.current_id), block);

                // after
                self.current_id = after_id;
                self.current_stmts = Vec::new();
            }
            ASTStmtNode::Break(ref stmt_id) => {
                let break_block = self.break_map.get(stmt_id).unwrap().clone();
                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(break_block),
                };

                self.blocks.insert(BlockID::new(self.current_id), block);
                self.current_id = self.next_id;
                self.next_id += 1;
                self.current_stmts = Vec::new();
            }
            ASTStmtNode::Continue(ref stmt_id) => {
                let continue_block = self.continue_map.get(stmt_id).unwrap().clone();

                let block = CFGBlock {
                    kind: BlockKind::Node,
                    id: BlockID::new(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(continue_block),
                };

                self.blocks.insert(BlockID::new(self.current_id), block);
                self.current_id = self.next_id;
                self.next_id += 1;
                self.current_stmts = Vec::new();
            }
        }
    }
}
