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
    Dot(ASTExpr, usize),
    Arrow(ASTExpr, usize),
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

    pub fn is_castable(&self, cast_to: &Type) -> bool {
        &self.expr_type == cast_to
            || (self.expr_type.is_int_type() && cast_to.is_int_type())
            || (self.expr_type.is_int_type() && cast_to.is_bool_type())
            || (self.expr_type.is_array_type() && cast_to.is_ptr_type())
    }

    pub fn cast_to(&self, cast_to: &Type) -> ASTExpr {
        assert!(self.is_castable(cast_to));
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
}

#[derive(Clone, PartialEq, Eq)]
pub enum ConstValue {
    Integer(i64),
}

impl ASTExpr {
    pub fn is_consteval(&self) -> bool {
        match *self.head {
            ASTExprNode::Number(_) => true,
            _ => false,
        }
    }

    pub fn eval_const(&self) -> ConstValue {
        assert!(self.is_consteval());
        match *self.head {
            ASTExprNode::Number(num) => ConstValue::Integer(num as i64),
            _ => panic!(),
        }
    }
}

impl ASTExpr {
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
            ASTExprNode::Dot(ref st_expr, index) => {
                writeln!(f, "{}Dot index:{}", indent, index)?;
                writeln!(f, "{}st_expr:", indent)?;
                st_expr.fmt_with_indent(f, &format!("{}\t", indent))
            }
            ASTExprNode::Arrow(ref st_expr, index) => {
                writeln!(f, "{}Arrow index:{}", indent, index)?;
                writeln!(f, "{}st_expr:", indent)?;
                st_expr.fmt_with_indent(f, &format!("{}\t", indent))
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
                writeln!(f, "{}Var {}", indent, obj.borrow().name)
            }
        }
    }
}

impl fmt::Debug for ASTExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, "")
    }
}
