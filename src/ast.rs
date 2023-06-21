mod expr;
mod stmt;

use crate::obj::Obj;
use std::fmt;

pub use expr::{
    ASTExpr, ASTExprNode, AssignKind, AssignNode, BinaryOpKind, BinaryOpNode, UnaryOpKind,
    UnaryOpNode,
};
pub use stmt::{ASTStmt, ASTStmtNode, DoWhileStmt, ForStmt, SwitchStmt, WhileStmt};

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

#[derive(Clone)]
pub enum ASTGlobal {
    Function(Obj, Vec<Obj>, Vec<ASTBlockStmt>),
    Variable(Obj),
}

impl ASTGlobal {
    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match self {
            ASTGlobal::Function(ref func_obj, ref _args, ref stmts) => {
                writeln!(f, "{}Function {}", indent, func_obj.borrow().name)?;
                for (i, stmt) in stmts.iter().enumerate() {
                    writeln!(f, "{} {}th stmt:", indent, i)?;
                    stmt.fmt_with_indent(f, &format!("{}\t", indent))?;
                }
                Ok(())
            }
            ASTGlobal::Variable(ref obj) => {
                writeln!(f, "{}Variable {}", indent, obj.borrow().name)
            }
        }
    }
}

impl fmt::Debug for ASTGlobal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, "")
    }
}
