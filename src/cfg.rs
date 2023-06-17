use crate::ast::ASTExpr;
use crate::obj::Obj;

#[derive(Clone)]
pub enum CFGStmt {
    Decl(Obj),
    Expr(ASTExpr),
}

#[derive(Clone)]
pub enum BlockID {
    Entry,
    Return,
    Block(usize),
}

#[derive(Clone)]
pub enum CFGJump {
    Unconditional(BlockID),
    Conditional(ASTExpr, BlockID, BlockID),
}

#[derive(Clone)]
pub struct CFGBlock {
    stmts: Vec<CFGStmt>,
    jump_to: CFGJump,
}

#[derive(Clone)]
pub struct CFGFunction {
    entry_block: CFGBlock,
    return_block: CFGBlock,
    blocks: Vec<CFGBlock>,
}

#[derive(Clone)]
pub enum CFGGlobal {
    Function(CFGFunction),
    Variable(Obj),
}
