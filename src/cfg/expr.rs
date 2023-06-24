use crate::obj::Obj;
use crate::types::Type;

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

#[derive(Clone, Debug)]
pub enum CFGUnaryOpKind {
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
