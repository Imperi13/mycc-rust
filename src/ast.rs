use crate::obj::Obj;
use crate::types::Type;
use std::fmt;

#[derive(Clone, Debug)]
pub enum BinaryOpKind {
    Comma,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitOr,
    BitXor,
    BitAnd,
    LogicalOr,
    LogicalAnd,
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

#[derive(Clone, Debug)]
pub enum AssignKind {
    Assign,
    LeftShiftAssign,
    RightShiftAssign,
    OrAssign,
    XorAssign,
    AndAssign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
}

#[derive(Clone)]
pub struct AssignNode {
    pub lhs: ASTExpr,
    pub rhs: ASTExpr,
    pub kind: AssignKind,
}

#[derive(Clone)]
pub enum ASTExprNode {
    Conditional(ASTExpr, ASTExpr, ASTExpr),
    Assign(AssignNode),
    BinaryOp(BinaryOpNode),
    UnaryOp(UnaryOpNode),
    Cast(Type, ASTExpr),
    FuncCall(ASTExpr, Vec<ASTExpr>),
    PostIncrement(ASTExpr),
    PostDecrement(ASTExpr),
    Number(u64),
    StrLiteral(String),
    Var(Obj),
}

#[derive(Clone)]
pub struct ASTExpr {
    head: Box<ASTExprNode>,
    pub expr_type: Type,
}

impl ASTExpr {
    pub fn new(node: ASTExprNode, expr_type: Type) -> ASTExpr {
        ASTExpr {
            head: Box::new(node),
            expr_type,
        }
    }

    pub fn get_node(&self) -> ASTExprNode {
        (*self.head).clone()
    }

    pub fn cast_to(&self, cast_to: &Type) -> ASTExpr {
        ASTExpr::new(
            ASTExprNode::Cast(cast_to.clone(), self.clone()),
            cast_to.clone(),
        )
    }

    pub fn cast_array(&self) -> ASTExpr {
        if self.expr_type.is_array_type() {
            let array_to = self.expr_type.get_array_to().unwrap();
            let ptr_ty = Type::new_ptr_type(array_to);
            self.cast_to(&ptr_ty)
        } else {
            self.clone()
        }
    }

    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match *self.head {
            ASTExprNode::Conditional(ref cond, ref then_expr, ref else_expr) => {
                writeln!(f, "{}Conditional", indent)?;
                writeln!(f, "{}cond:", indent)?;
                cond.fmt_with_indent(f, &format!("{}\t", indent))?;
                writeln!(f, "{}then_expr:", indent)?;
                then_expr.fmt_with_indent(f, &format!("{}\t", indent))?;
                writeln!(f, "{}else_expr:", indent)?;
                else_expr.fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTExprNode::Assign(ref assign_node) => {
                writeln!(f, "{}Assign", indent)?;
                writeln!(f, "{}lhs:", indent)?;
                assign_node
                    .lhs
                    .fmt_with_indent(f, &format!("{}\t", indent))?;
                writeln!(f, "{}val:", indent)?;
                assign_node.rhs.fmt_with_indent(f, &format!("{}\t", indent))
            }
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
            ASTExprNode::FuncCall(ref func_expr, ref _args) => {
                writeln!(f, "{}FuncCall", indent)?;
                writeln!(f, "{}func_expr:", indent)?;
                func_expr.fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTExprNode::PostIncrement(ref expr) => {
                writeln!(f, "{}PostIncrement", indent)?;
                writeln!(f, "{}expr:", indent)?;
                expr.fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTExprNode::PostDecrement(ref expr) => {
                writeln!(f, "{}PostDecrement", indent)?;
                writeln!(f, "{}expr:", indent)?;
                expr.fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTExprNode::Number(num) => {
                writeln!(f, "{}Number {}", indent, num)
            }
            ASTExprNode::StrLiteral(ref text) => writeln!(f, "{}Number {}", indent, text),
            ASTExprNode::Var(ref obj) => {
                writeln!(f, "{}Var {}", indent, obj.get_node().name)
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
    Break(usize),
    Continue(usize),
    Declaration(Obj),
    ExprStmt(ASTExpr),
    Block(Vec<ASTStmt>),
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
                expr.fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTStmtNode::Break(ref stmt_id) => {
                writeln!(f, "{}Break: id{}", indent, stmt_id)
            }
            ASTStmtNode::Continue(ref stmt_id) => {
                writeln!(f, "{}Continue: id{}", indent, stmt_id)
            }
            ASTStmtNode::Declaration(ref obj) => {
                writeln!(f, "{}Declaration :{}", indent, obj.get_node().name)
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

#[derive(Clone)]
pub enum ASTGlobal {
    Function(Obj, Vec<Obj>, Vec<ASTStmt>),
    Variable(Obj),
}

impl ASTGlobal {
    pub fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match self {
            ASTGlobal::Function(ref func_obj, ref _args, ref stmts) => {
                writeln!(f, "{}Function {}", indent, func_obj.get_node().name)?;
                for (i, stmt) in stmts.iter().enumerate() {
                    writeln!(f, "{} {}th stmt:", indent, i)?;
                    stmt.fmt_with_indent(f, &format!("{}\t", indent))?;
                }
                Ok(())
            }
            ASTGlobal::Variable(ref obj) => {
                writeln!(f, "{}Variable {}", indent, obj.get_node().name)
            }
        }
    }
}

impl fmt::Debug for ASTGlobal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, "")
    }
}
