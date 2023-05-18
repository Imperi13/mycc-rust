use std::rc::Rc;

#[derive(Clone)]
pub enum TypeNode {
    Int,
    Func,
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

    pub fn get_node(&self) -> TypeNode {
        (*self.head).clone()
    }

    pub fn is_function_type(&self) -> bool {
        matches!(*self.head, TypeNode::Func)
    }

    pub fn is_int_type(&self) -> bool {
        matches!(*self.head, TypeNode::Int)
    }

    pub fn is_ptr_type(&self) -> bool {
        matches!(*self.head, TypeNode::Ptr(_))
    }
}
