use crate::ast::ASTBlockStmt;
use crate::ast::ASTExpr;

use std::fmt;

#[derive(Clone)]
pub struct SwitchStmt {
    pub cond: ASTExpr,
    pub stmt: ASTStmt,
    pub break_id: usize,
}

#[derive(Clone)]
pub struct WhileStmt {
    pub cond: ASTExpr,
    pub loop_stmt: ASTStmt,
    pub continue_id: usize,
    pub break_id: usize,
}

#[derive(Clone)]
pub struct DoWhileStmt {
    pub cond: ASTExpr,
    pub loop_stmt: ASTStmt,
    pub continue_id: usize,
    pub break_id: usize,
}

#[derive(Clone)]
pub struct ForStmt {
    pub start: Option<ASTExpr>,
    pub cond: Option<ASTExpr>,
    pub step: Option<ASTExpr>,
    pub loop_stmt: ASTStmt,

    pub continue_id: usize,
    pub break_id: usize,
}

#[derive(Clone)]
pub enum ASTStmtNode {
    ExprStmt(ASTExpr),
    Return(Option<ASTExpr>),
    Break(usize),
    Continue(usize),
    Block(Vec<ASTBlockStmt>),
    If(ASTExpr, ASTStmt, Option<ASTStmt>),
    Switch(SwitchStmt),
    While(WhileStmt),
    DoWhile(DoWhileStmt),
    For(ForStmt),
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
            ASTStmtNode::Switch(ref switch_node) => {
                writeln!(f, "{}Switch: ", indent)?;
                writeln!(f, "{}cond:", indent)?;
                switch_node
                    .cond
                    .fmt_with_indent(f, &format!("{}\t", indent))?;
                writeln!(f, "{}stmt:", indent)?;
                switch_node
                    .stmt
                    .fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTStmtNode::While(ref while_node) => {
                writeln!(f, "{}While: ", indent)?;
                writeln!(f, "{}cond:", indent)?;
                while_node
                    .cond
                    .fmt_with_indent(f, &format!("{}\t", indent))?;
                writeln!(f, "{}stmt:", indent)?;
                while_node
                    .loop_stmt
                    .fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTStmtNode::DoWhile(ref dowhile_node) => {
                writeln!(f, "{}DoWhile: ", indent)?;
                writeln!(f, "{}cond:", indent)?;
                dowhile_node
                    .cond
                    .fmt_with_indent(f, &format!("{}\t", indent))?;
                writeln!(f, "{}stmt:", indent)?;
                dowhile_node
                    .loop_stmt
                    .fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTStmtNode::For(ref for_node) => {
                writeln!(f, "{}For: ", indent)?;
                if for_node.start.is_some() {
                    writeln!(f, "{}start:", indent)?;
                    let start = for_node.start.as_ref().unwrap();
                    start.fmt_with_indent(f, &format!("{}\t", indent))?;
                }
                if for_node.cond.is_some() {
                    writeln!(f, "{}cond:", indent)?;
                    let cond = for_node.cond.as_ref().unwrap();
                    cond.fmt_with_indent(f, &format!("{}\t", indent))?;
                }
                if for_node.step.is_some() {
                    writeln!(f, "{}step:", indent)?;
                    let step = for_node.step.as_ref().unwrap();
                    step.fmt_with_indent(f, &format!("{}\t", indent))?;
                }
                writeln!(f, "{}stmt:", indent)?;
                for_node
                    .loop_stmt
                    .fmt_with_indent(f, &format!("{}\t", indent))
            }
        }
    }
}

impl fmt::Debug for ASTStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, "")
    }
}
