use crate::ast::expr::ASTExpr;
use crate::ast::expr::ASTExprNode;
use crate::cfg::expr::CFGExpr;
use crate::cfg::expr::CFGExprNode;
use crate::types::Type;
use crate::types::TypeNode;

#[derive(Clone, PartialEq, Eq)]
pub enum ConstValue {
    Integer(Type, i64),
}

impl ConstValue {
    pub fn to_ast(&self) -> ASTExpr {
        match *self {
            ConstValue::Integer(ref ty, num) => {
                ASTExpr::new(ASTExprNode::Number(num as u64), ty.clone())
            }
        }
    }

    pub fn to_cfg(&self) -> CFGExpr {
        match *self {
            ConstValue::Integer(ref ty, num) => {
                CFGExpr::new(CFGExprNode::Number(num as u64), ty.clone())
            }
        }
    }

    pub fn is_constzero(&self) -> bool {
        let ConstValue::Integer(_, val) = *self;
        val == 0
    }

    // const_value binary op

    pub fn const_value_add(lhs: ConstValue, rhs: ConstValue) -> ConstValue {
        let ConstValue::Integer(ty_lhs, lhs) = lhs;
        let ConstValue::Integer(ty_rhs, rhs) = rhs;
        assert!(ty_lhs == ty_rhs);

        ConstValue::Integer(ty_lhs, lhs + rhs)
    }

    pub fn const_value_sub(lhs: ConstValue, rhs: ConstValue) -> ConstValue {
        let ConstValue::Integer(ty_lhs, lhs) = lhs;
        let ConstValue::Integer(ty_rhs, rhs) = rhs;
        assert!(ty_lhs == ty_rhs);

        ConstValue::Integer(ty_lhs, lhs - rhs)
    }

    pub fn const_value_mul(lhs: ConstValue, rhs: ConstValue) -> ConstValue {
        let ConstValue::Integer(ty_lhs, lhs) = lhs;
        let ConstValue::Integer(ty_rhs, rhs) = rhs;
        assert!(ty_lhs == ty_rhs);

        ConstValue::Integer(ty_lhs, lhs * rhs)
    }

    pub fn const_value_div(lhs: ConstValue, rhs: ConstValue) -> ConstValue {
        let ConstValue::Integer(ty_lhs, lhs) = lhs;
        let ConstValue::Integer(ty_rhs, rhs) = rhs;
        assert!(ty_lhs == ty_rhs);

        ConstValue::Integer(ty_lhs, lhs / rhs)
    }

    // cons_value unary op

    pub fn const_value_plus(val: ConstValue) -> ConstValue {
        val
    }

    pub fn const_value_minus(val: ConstValue) -> ConstValue {
        let ConstValue::Integer(ty, val) = val;
        ConstValue::Integer(ty, -val)
    }

    pub fn const_value_logical_not(val: ConstValue) -> ConstValue {
        if val.is_constzero() {
            ConstValue::Integer(Type::new(TypeNode::Int), 0)
        } else {
            ConstValue::Integer(Type::new(TypeNode::Int), 1)
        }
    }

    // const_value cast

    pub fn const_value_cast(val: ConstValue, cast_to: Type) -> ConstValue {
        if cast_to.is_bool_type() {
            if val.is_constzero() {
                ConstValue::Integer(Type::new(TypeNode::Bool), 0)
            } else {
                ConstValue::Integer(Type::new(TypeNode::Bool), 0)
            }
        } else {
            let ConstValue::Integer(_, val) = val;
            ConstValue::Integer(cast_to, val)
        }
    }
}
