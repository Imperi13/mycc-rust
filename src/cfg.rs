use crate::ast::ASTBlockStmt;
use crate::ast::ASTExpr;
use crate::ast::ASTExprNode;
use crate::ast::ASTGlobal;
use crate::ast::ASTStmt;
use crate::ast::ASTStmtNode;
use crate::ast::AssignKind;
use crate::ast::AssignNode;
use crate::obj::Obj;
use crate::obj::ObjArena;

use std::collections::HashMap;
use std::fmt;

#[derive(Clone)]
pub enum CFGStmt {
    Decl(Obj),
    Expr(ASTExpr),
}

#[derive(Clone, Debug)]
pub enum BlockID {
    Entry,
    Return,
    Block(usize),
}

#[derive(Clone, Debug)]
pub enum CFGJump {
    None,
    Return(Obj),
    Unconditional(BlockID),
    Conditional(ASTExpr, BlockID, BlockID),
}

#[derive(Clone)]
pub struct CFGBlock {
    id: BlockID,
    stmts: Vec<CFGStmt>,
    jump_to: CFGJump,
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

impl fmt::Debug for CFGBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "ID: {:?}", self.id)?;
        writeln!(f, "stmts:")?;
        for stmt in self.stmts.iter() {
            match stmt {
                CFGStmt::Decl(ref _obj) => writeln!(f, "Decl "),
                CFGStmt::Expr(ref expr) => expr.fmt_with_indent(f, "\t"),
            }?;
        }

        writeln!(f, "jump_to: {:?}", self.jump_to)
    }
}

#[derive(Clone)]
pub struct CFGFunction {
    entry_block: CFGBlock,
    return_block: CFGBlock,
    blocks: HashMap<usize, CFGBlock>,
}

#[derive(Clone)]
pub enum CFGGlobal {
    Function(CFGFunction),
    Variable(Obj),
}

impl fmt::Debug for CFGGlobal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CFGGlobal::Function(ref func) => {
                writeln!(f, "{:?}", func.entry_block)?;
                for (_, block) in func.blocks.iter() {
                    writeln!(f, "{:?}", block)?;
                }
                writeln!(f, "{:?}", func.return_block)
            }
            CFGGlobal::Variable(ref obj) => {
                writeln!(f, "Variable {}", obj.borrow().name)
            }
        }
    }
}

pub fn gen_cfg_all(obj_arena: &mut ObjArena, ast_all: &Vec<ASTGlobal>) -> Vec<CFGGlobal> {
    let mut cfg_globals = Vec::new();
    for ast in ast_all.iter() {
        let cfg_global = match ast {
            ASTGlobal::Function(func_obj, _, ref stmts) => {
                let retval = obj_arena.publish_obj(
                    "retval",
                    func_obj.borrow().obj_type.get_return_type().unwrap(),
                );
                let mut arena = CFGArena::new(retval);
                CFGGlobal::Function(arena.gen_cfg_function(stmts))
            }
            ASTGlobal::Variable(ref obj) => CFGGlobal::Variable(obj.clone()),
        };

        cfg_globals.push(cfg_global);
    }

    cfg_globals
}

struct CFGArena {
    retval: Obj,

    entry_block: CFGBlock,
    return_block: CFGBlock,
    blocks: HashMap<usize, CFGBlock>,

    current_id: usize,
    current_stmts: Vec<CFGStmt>,
}

impl CFGArena {
    pub fn new(retval: Obj) -> Self {
        CFGArena {
            retval,
            entry_block: CFGBlock::new(BlockID::Entry),
            return_block: CFGBlock::new(BlockID::Return),
            blocks: HashMap::new(),
            current_id: 0,
            current_stmts: Vec::new(),
        }
    }

    pub fn gen_cfg_function(&mut self, stmts: &Vec<ASTBlockStmt>) -> CFGFunction {
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
        self.current_id += 1;
        self.current_stmts = Vec::new();

        self.return_block.jump_to = CFGJump::Return(self.retval.clone());

        CFGFunction {
            entry_block: self.entry_block.clone(),
            return_block: self.return_block.clone(),
            blocks: self.blocks.clone(),
        }
    }

    pub fn push_stmt(&mut self, stmt: &ASTStmt) {
        match stmt.get_node() {
            ASTStmtNode::ExprStmt(ref expr) => {
                self.current_stmts.push(CFGStmt::Expr(expr.clone()));
            }
            ASTStmtNode::Return(ref expr) => {
                let retval_expr = ASTExpr::new(
                    ASTExprNode::Var(self.retval.clone()),
                    self.retval.borrow().obj_type.clone(),
                );
                let assign_expr = ASTExpr::new(
                    ASTExprNode::Assign(AssignNode {
                        lhs: retval_expr,
                        rhs: expr.clone(),
                        kind: AssignKind::Assign,
                    }),
                    self.retval.borrow().obj_type.clone(),
                );
                self.current_stmts.push(CFGStmt::Expr(assign_expr));
                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Return),
                };

                self.blocks.insert(self.current_id, block);
                self.current_id += 1;
                self.current_stmts = Vec::new();
            }
            _ => unimplemented!(),
        }
    }
}
