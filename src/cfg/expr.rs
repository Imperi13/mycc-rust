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
    Deref,
    LogicalNot,
    BitNot,
}

#[derive(Clone)]
pub struct CFGUnaryOpNode {
    pub expr: CFGExpr,
    pub kind: CFGUnaryOpKind,
}

#[derive(Clone)]
pub enum CFGExprNode {
    BinaryOp(CFGBinaryOpNode),
    UnaryOp(CFGUnaryOpNode),
    Cast(Type, CFGExpr),
    Dot(CFGExpr, usize),
    Arrow(CFGExpr, usize),
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
            CFGExprNode::Var(ref obj) => write!(f, "{}_{}", obj.borrow().name, obj.borrow().id),
            CFGExprNode::Cast(ref ty, ref expr) => write!(f, "({:?})({:?})", ty, expr),
            CFGExprNode::Number(num) => write!(f, "{num}"),
            CFGExprNode::BinaryOp(ref node) => write!(f, "{:?}", node),
            _ => write!(f, ""),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum ConstValue {
    Integer(i64),
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
            CFGExprNode::Number(num) => ConstValue::Integer(num as i64),
            _ => panic!(),
        }
    }

    pub fn is_const_zero(&self) -> bool {
        self.is_consteval() && (self.eval_const() == ConstValue::Integer(0))
    }
}
