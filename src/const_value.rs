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
}
