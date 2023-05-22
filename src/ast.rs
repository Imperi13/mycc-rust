use crate::parse::Obj;
use crate::types::Type;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum BinaryOpKind {
    Assign,
    Comma,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitOr,
    BitAnd,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    LeftShift,
    RightShift,
}

#[derive(Clone)]
pub struct BinaryOpNode {
    pub lhs: ASTExpr,
    pub rhs: ASTExpr,
    pub kind: BinaryOpKind,
}

#[derive(Clone, Debug)]
pub enum UnaryOpKind {
    Sizeof,
    Alignof,
    Plus,
    Minus,
    Addr,
    Deref,
    LogicalNot,
    BitNot,
}

#[derive(Clone)]
pub struct UnaryOpNode {
    pub expr: ASTExpr,
    pub kind: UnaryOpKind,
}

#[derive(Clone)]
pub enum ASTExprNode {
    BinaryOp(BinaryOpNode),
    UnaryOp(UnaryOpNode),
    Cast(Type, ASTExpr),
    FuncCall(ASTExpr),
    Number(u64),
    StrLiteral(String),
    Var(Rc<RefCell<Obj>>),
}

#[derive(Clone)]
pub struct ASTExpr {
    head: Rc<RefCell<ASTExprNode>>,
    pub expr_type: Type,
}

impl ASTExpr {
    pub fn new(node: ASTExprNode, expr_type: Type) -> ASTExpr {
        ASTExpr {
            head: Rc::new(RefCell::new(node)),
            expr_type,
        }
    }

    pub fn get_node(&self) -> ASTExprNode {
        (*self.head).borrow().clone()
    }

    pub fn build_cast_node(cast_to: Type, expr: ASTExpr) -> ASTExpr {
        ASTExpr::new(ASTExprNode::Cast(cast_to.clone(), expr), cast_to)
    }

    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match *self.head.borrow() {
            ASTExprNode::BinaryOp(ref binary_node) => {
                writeln!(f, "{}BinaryOp {:?}", indent, binary_node.kind)?;
                writeln!(f, "{}lhs:", indent)?;
                binary_node
                    .lhs
                    .fmt_with_indent(f, &format!("{}\t", indent))?;
                writeln!(f, "{}rhs:", indent)?;
                binary_node.rhs.fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTExprNode::UnaryOp(ref unary_node) => {
                writeln!(f, "{}UnaryOp {:?}", indent, unary_node.kind)?;
                writeln!(f, "{}expr:", indent)?;
                unary_node.expr.fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTExprNode::Cast(ref _ty, ref expr) => {
                writeln!(f, "{}Cast", indent)?;
                writeln!(f, "{}expr:", indent)?;
                expr.fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTExprNode::FuncCall(ref func_expr) => {
                writeln!(f, "{}FuncCall", indent)?;
                writeln!(f, "{}func_expr:", indent)?;
                func_expr.fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTExprNode::Number(num) => {
                writeln!(f, "{}Number {}", indent, num)
            }
            ASTExprNode::StrLiteral(ref text) => writeln!(f, "{}Number {}", indent, text),
            ASTExprNode::Var(ref obj) => {
                writeln!(f, "{}Var {}", indent, &*obj.borrow().name)
            }
        }
    }
}

impl fmt::Debug for ASTExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, "")
    }
}

#[derive(Clone)]
pub enum ASTStmtNode {
    Return(ASTExpr),
    Declaration(Rc<RefCell<Obj>>),
    ExprStmt(ASTExpr),
    Block(Vec<ASTStmt>),
    If(ASTExpr, ASTStmt, Option<ASTStmt>),
    While(ASTExpr, ASTStmt),
    For(ASTExpr, ASTExpr, ASTExpr, ASTStmt),
}

#[derive(Clone)]
pub struct ASTStmt {
    head: Rc<RefCell<ASTStmtNode>>,
}

impl ASTStmt {
    pub fn new(node: ASTStmtNode) -> ASTStmt {
        ASTStmt {
            head: Rc::new(RefCell::new(node)),
        }
    }

    pub fn get_node(&self) -> ASTStmtNode {
        (*self.head).borrow().clone()
    }

    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match *self.head.borrow() {
            ASTStmtNode::Return(ref expr) => {
                writeln!(f, "{}Return", indent)?;
                writeln!(f, "{}expr:", indent)?;
                expr.fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTStmtNode::Declaration(ref obj) => {
                writeln!(f, "{}Declaration :{}", indent, &*obj.borrow().name)
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
            ASTStmtNode::While(ref cond, ref stmt) => {
                writeln!(f, "{}While", indent)?;
                writeln!(f, "{}cond:", indent)?;
                cond.fmt_with_indent(f, &format!("{}\t", indent))?;
                writeln!(f, "{}stmt:", indent)?;
                stmt.fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTStmtNode::For(ref start, ref cond, ref step, ref stmt) => {
                writeln!(f, "{}For", indent)?;
                writeln!(f, "{}start:", indent)?;
                start.fmt_with_indent(f, &format!("{}\t", indent))?;
                writeln!(f, "{}cond:", indent)?;
                cond.fmt_with_indent(f, &format!("{}\t", indent))?;
                writeln!(f, "{}step:", indent)?;
                step.fmt_with_indent(f, &format!("{}\t", indent))?;
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

#[derive(Clone)]
pub enum ASTGlobal {
    Function(Rc<RefCell<Obj>>, Vec<ASTStmt>),
    Variable(Rc<RefCell<Obj>>),
}

impl ASTGlobal {
    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match self {
            ASTGlobal::Function(ref obj, ref stmts) => {
                writeln!(f, "{}Function {}", indent, &*obj.borrow().name)?;
                for (i, stmt) in stmts.iter().enumerate() {
                    writeln!(f, "{} {}th stmt:", indent, i)?;
                    stmt.fmt_with_indent(f, &format!("{}\t", indent))?;
                }
                Ok(())
            }
            ASTGlobal::Variable(ref obj) => {
                writeln!(f, "{}Variable {}", indent, &*obj.borrow().name)
            }
        }
    }
}

impl fmt::Debug for ASTGlobal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, "")
    }
}
