use std::rc::Rc;

#[derive(Clone)]
pub struct FunctionTypeNode {
    pub return_type: Type,
}

#[derive(Clone)]
pub enum TypeNode {
    Int,
    Func(FunctionTypeNode),
    Ptr(Type),
}

#[derive(Clone)]
pub struct Type {
    head: Rc<TypeNode>,
}

impl Type {
    pub fn new(kind: TypeNode) -> Type {
        Type {
            head: Rc::new(kind),
        }
    }

    pub fn get_fn_type_node(&self) -> Result<FunctionTypeNode, ()> {
        match &*self.head {
            TypeNode::Func(node) => Ok(node.clone()),
            _ => Err(()),
        }
    }

    pub fn is_function_type(&self) -> bool {
        matches!(*self.head, TypeNode::Func(_))
    }

    pub fn is_int_type(&self) -> bool {
        matches!(*self.head, TypeNode::Int)
    }

    pub fn is_ptr_type(&self) -> bool {
        matches!(*self.head, TypeNode::Ptr(_))
    }
}
