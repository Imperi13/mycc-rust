use crate::ast::ASTBlockStmt;
use crate::ast::ASTExpr;

use std::fmt;

#[derive(Clone)]
pub enum ASTStmtNode {
    ExprStmt(ASTExpr),
    Return(Option<ASTExpr>),
    Break(usize),
    Continue(usize),
    Block(Vec<ASTBlockStmt>),
    If(ASTExpr, ASTStmt, Option<ASTStmt>),
    While(ASTExpr, ASTStmt, usize),
    DoWhile(ASTExpr, ASTStmt, usize),
    For(
        Option<ASTExpr>,
        Option<ASTExpr>,
        Option<ASTExpr>,
        ASTStmt,
        usize,
    ),
}

#[derive(Clone)]
pub struct ASTStmt {
    head: Box<ASTStmtNode>,
}

impl ASTStmt {
    pub fn new(node: ASTStmtNode) -> ASTStmt {
        ASTStmt {
            head: Box::new(node),
        }
    }

    pub fn get_node(&self) -> ASTStmtNode {
        (*self.head).clone()
    }

    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match *self.head {
            ASTStmtNode::Return(ref expr) => {
                writeln!(f, "{}Return", indent)?;
                writeln!(f, "{}expr:", indent)?;
                if expr.is_some() {
                    let expr = expr.as_ref().unwrap();
                    expr.fmt_with_indent(f, &format!("{}\t", indent))?;
                }
                Ok(())
            }
            ASTStmtNode::Break(ref stmt_id) => {
                writeln!(f, "{}Break: id{}", indent, stmt_id)
            }
            ASTStmtNode::Continue(ref stmt_id) => {
                writeln!(f, "{}Continue: id{}", indent, stmt_id)
            }
            ASTStmtNode::ExprStmt(ref expr) => {
                writeln!(f, "{}ExprStmt", indent)?;
                writeln!(f, "{}expr:", indent)?;
                expr.fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTStmtNode::Block(ref stmts) => {
                writeln!(f, "{}Block", indent)?;
                for (i, stmt) in stmts.iter().enumerate() {
                    writeln!(f, "{} {}th expr:", indent, i)?;
                    stmt.fmt_with_indent(f, &format!("{}\t", indent))?;
                }
                Ok(())
            }
            ASTStmtNode::If(ref cond, ref if_stmt, ref else_stmt) => {
                writeln!(f, "{}If", indent)?;
                writeln!(f, "{}cond:", indent)?;
                cond.fmt_with_indent(f, &format!("{}\t", indent))?;
                writeln!(f, "{}if_stmt:", indent)?;
                if_stmt.fmt_with_indent(f, &format!("{}\t", indent))?;

                if else_stmt.is_some() {
                    let else_stmt = else_stmt.clone().unwrap();

                    writeln!(f, "{}else_stmt:", indent)?;
                    else_stmt.fmt_with_indent(f, &format!("{}\t", indent))
                } else {
                    Ok(())
                }
            }
            ASTStmtNode::While(ref cond, ref stmt, ref stmt_id) => {
                writeln!(f, "{}While: id{}", indent, stmt_id)?;
                writeln!(f, "{}cond:", indent)?;
                cond.fmt_with_indent(f, &format!("{}\t", indent))?;
                writeln!(f, "{}stmt:", indent)?;
                stmt.fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTStmtNode::DoWhile(ref cond, ref stmt, ref stmt_id) => {
                writeln!(f, "{}DoWhile: id{}", indent, stmt_id)?;
                writeln!(f, "{}cond:", indent)?;
                cond.fmt_with_indent(f, &format!("{}\t", indent))?;
                writeln!(f, "{}stmt:", indent)?;
                stmt.fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTStmtNode::For(ref start, ref cond, ref step, ref stmt, ref stmt_id) => {
                writeln!(f, "{}For: id{}", indent, stmt_id)?;
                if start.is_some() {
                    writeln!(f, "{}start:", indent)?;
                    let start = start.as_ref().unwrap();
                    start.fmt_with_indent(f, &format!("{}\t", indent))?;
                }
                if cond.is_some() {
                    writeln!(f, "{}cond:", indent)?;
                    let cond = cond.as_ref().unwrap();
                    cond.fmt_with_indent(f, &format!("{}\t", indent))?;
                }
                if step.is_some() {
                    writeln!(f, "{}step:", indent)?;
                    let step = step.as_ref().unwrap();
                    step.fmt_with_indent(f, &format!("{}\t", indent))?;
                }
                writeln!(f, "{}stmt:", indent)?;
                stmt.fmt_with_indent(f, &format!("{}\t", indent))
            }
        }
    }
}

impl fmt::Debug for ASTStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, "")
    }
}
