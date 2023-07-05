use crate::const_value::ConstValue;
use crate::obj::Obj;
use crate::types::Type;

use std::fmt;

#[derive(Clone, Debug)]
pub enum CFGBinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitOr,
    BitXor,
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
pub struct CFGBinaryOpNode {
    pub lhs: CFGExpr,
    pub rhs: CFGExpr,
    pub kind: CFGBinaryOpKind,
}

impl fmt::Debug for CFGBinaryOpNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = match self.kind {
            CFGBinaryOpKind::Add => "+",
            CFGBinaryOpKind::Sub => "-",
            CFGBinaryOpKind::Mul => "*",
            CFGBinaryOpKind::Div => "/",
            CFGBinaryOpKind::Mod => "%",
            CFGBinaryOpKind::BitOr => "|",
            CFGBinaryOpKind::BitXor => "^",
            CFGBinaryOpKind::BitAnd => "&",
            CFGBinaryOpKind::Less => "<",
            CFGBinaryOpKind::LessEqual => "<=",
            CFGBinaryOpKind::Greater => ">",
            CFGBinaryOpKind::GreaterEqual => ">=",
            CFGBinaryOpKind::Equal => "==",
            CFGBinaryOpKind::NotEqual => "!=",
            CFGBinaryOpKind::LeftShift => "<<",
            CFGBinaryOpKind::RightShift => ">>",
        };

        write!(f, "{:?} {} {:?}", self.lhs, op, self.rhs)
    }
}

#[derive(Clone, Debug)]
pub enum CFGUnaryOpKind {
    Plus,
    Minus,
    Addr,
    LogicalNot,
    BitNot,
}

#[derive(Clone)]
pub struct CFGUnaryOpNode {
    pub expr: CFGExpr,
    pub kind: CFGUnaryOpKind,
}

impl fmt::Debug for CFGUnaryOpNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = match self.kind {
            CFGUnaryOpKind::Plus => "+",
            CFGUnaryOpKind::Minus => "-",
            CFGUnaryOpKind::Addr => "&",
            CFGUnaryOpKind::BitNot => "~",
            CFGUnaryOpKind::LogicalNot => "!",
        };

        write!(f, "{}{:?}", op, self.expr)
    }
}

#[derive(Clone)]
pub enum CFGExprNode {
    BinaryOp(CFGBinaryOpNode),
    UnaryOp(CFGUnaryOpNode),
    Cast(Type, CFGExpr),
    Dot(CFGExpr, usize),
    Arrow(CFGExpr, usize),
    // Deref is a unary operator but it can be a left-hand side value, so it is separated
    Deref(CFGExpr),
    Number(u64),
    StrLiteral(String),
    Var(Obj),
    Sizeof(Type),
    Alignof(Type),
}

#[derive(Clone)]
pub struct CFGExpr {
    head: Box<CFGExprNode>,
    pub expr_type: Type,
}

impl CFGExpr {
    pub fn new(node: CFGExprNode, expr_type: Type) -> Self {
        CFGExpr {
            head: Box::new(node),
            expr_type,
        }
    }

    pub fn get_node(&self) -> CFGExprNode {
        (*self.head).clone()
    }
}

impl fmt::Debug for CFGExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.get_node() {
            CFGExprNode::Var(ref obj) => write!(f, "{:?}", obj),
            CFGExprNode::Cast(ref ty, ref expr) => write!(f, "({:?})({:?})", ty, expr),
            CFGExprNode::Number(num) => write!(f, "{num}"),
            CFGExprNode::Dot(ref expr, index) => write!(f, "{:?}.{index}", expr),
            CFGExprNode::Arrow(ref expr, index) => write!(f, "{:?}->{index}", expr),
            CFGExprNode::Deref(ref expr) => write!(f, "*({:?})", expr),
            CFGExprNode::Sizeof(ref ty) => write!(f, "sizeof({:?})", ty),
            CFGExprNode::Alignof(ref ty) => write!(f, "alignof({:?})", ty),
            CFGExprNode::StrLiteral(ref text) => write!(f, "\"{text}\""),
            CFGExprNode::UnaryOp(ref node) => write!(f, "{:?}", node),
            CFGExprNode::BinaryOp(ref node) => write!(f, "{:?}", node),
        }
    }
}

impl CFGExpr {
    pub fn is_consteval(&self) -> bool {
        match *self.head {
            CFGExprNode::Number(_) => true,
            _ => false,
        }
    }

    pub fn eval_const(&self) -> ConstValue {
        assert!(self.is_consteval());
        match *self.head {
            CFGExprNode::Number(num) => ConstValue::Integer(self.expr_type.clone(), num as i64),
            _ => panic!(),
        }
    }

    pub fn is_const_zero(&self) -> bool {
        self.is_consteval() && self.eval_const().is_constzero()
    }
}
