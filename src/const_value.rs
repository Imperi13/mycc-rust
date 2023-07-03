use crate::ast::expr::ASTExpr;
use crate::ast::expr::ASTExprNode;
use crate::cfg::expr::CFGExpr;
use crate::cfg::expr::CFGExprNode;
use crate::types::Type;
use crate::types::TypeNode;

#[derive(Clone, PartialEq, Eq)]
pub enum ConstValue {
    Integer(i64),
}

impl ConstValue {
    pub fn to_ast(&self) -> ASTExpr {
        match *self {
            ConstValue::Integer(num) => {
                ASTExpr::new(ASTExprNode::Number(num as u64), Type::new(TypeNode::Int))
            }
        }
    }

    pub fn to_cfg(&self) -> CFGExpr {
        match *self {
            ConstValue::Integer(num) => {
                CFGExpr::new(CFGExprNode::Number(num as u64), Type::new(TypeNode::Int))
            }
        }
    }

    pub fn const_value_add(lhs: ConstValue, rhs: ConstValue) -> ConstValue {
        let ConstValue::Integer(lhs) = lhs;
        let ConstValue::Integer(rhs) = rhs;

        ConstValue::Integer(lhs + rhs)
    }

    pub fn const_value_sub(lhs: ConstValue, rhs: ConstValue) -> ConstValue {
        let ConstValue::Integer(lhs) = lhs;
        let ConstValue::Integer(rhs) = rhs;

        ConstValue::Integer(lhs - rhs)
    }

    pub fn const_value_mul(lhs: ConstValue, rhs: ConstValue) -> ConstValue {
        let ConstValue::Integer(lhs) = lhs;
        let ConstValue::Integer(rhs) = rhs;

        ConstValue::Integer(lhs * rhs)
    }

    pub fn const_value_div(lhs: ConstValue, rhs: ConstValue) -> ConstValue {
        let ConstValue::Integer(lhs) = lhs;
        let ConstValue::Integer(rhs) = rhs;

        ConstValue::Integer(lhs / rhs)
    }
}
