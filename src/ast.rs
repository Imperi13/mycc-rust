pub mod expr;
pub mod stmt;

use expr::ASTExpr;
use stmt::ASTStmt;

use crate::obj::GlobalObjArena;
use crate::obj::LocalObjArena;
use crate::obj::Obj;
use std::fmt;

#[derive(Clone)]
pub enum ASTBlockStmt {
    Declaration(Obj),
    Stmt(ASTStmt),
}

impl ASTBlockStmt {
    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match self {
            ASTBlockStmt::Declaration(ref _obj) => writeln!(f, "{}Declaration", indent),
            ASTBlockStmt::Stmt(ref stmt) => stmt.fmt_with_indent(f, indent),
        }
    }
}

impl fmt::Debug for ASTBlockStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, "")
    }
}

pub struct ASTFunction {
    pub func_obj: Obj,
    pub args_obj: Vec<Obj>,
    pub stmts: Vec<ASTBlockStmt>,

    pub obj_arena: LocalObjArena,
}

pub struct AST {
    pub extern_globals: Vec<Obj>,
    pub global_variables: Vec<Obj>,
    pub funcs: Vec<Obj>,

    pub obj_arena: GlobalObjArena,
}
