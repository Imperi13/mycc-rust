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

    pub fn new_ptr_type(ptr_to: Type) -> Type {
        Type {
            head: Rc::new(TypeNode::Ptr(ptr_to)),
        }
    }

    pub fn get_node(&self) -> TypeNode {
        (*self.head).clone()
    }

    pub fn get_ptr_to(&self) -> Result<Type, ()> {
        match &*self.head {
            TypeNode::Ptr(ptr_to) => Ok(ptr_to.clone()),
            _ => Err(()),
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
