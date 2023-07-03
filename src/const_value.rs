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

    pub fn is_constzero(&self) -> bool {
        let ConstValue::Integer(val) = *self;
        val == 0
    }

    // const_value binary op

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

    // cons_value unary op

    pub fn const_value_plus(val: ConstValue) -> ConstValue {
        val
    }

    pub fn const_value_minus(val: ConstValue) -> ConstValue {
        let ConstValue::Integer(val) = val;
        ConstValue::Integer(-val)
    }

    pub fn const_value_logical_not(val: ConstValue) -> ConstValue {
        if val.is_constzero() {
            ConstValue::Integer(0)
        } else {
            ConstValue::Integer(1)
        }
    }
}
