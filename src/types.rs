use std::fmt;
use std::rc::Rc;

#[derive(Clone)]
pub enum TypeNode {
    Int,
    Char,
    Func(Type, Vec<Type>),
    Ptr(Type),
    Array(Type, u32),
}

#[derive(Clone)]
pub struct Type {
    head: Rc<TypeNode>,
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self.head {
            TypeNode::Int => write!(f, "Int"),
            TypeNode::Char => write!(f, "Char"),
            TypeNode::Func(ref ret, ref args) => {
                write!(f, "Func({:?},(", ret)?;
                for arg in args.iter() {
                    write!(f, "{:?},", arg)?;
                }
                write!(f, ")")
            }
            TypeNode::Ptr(ref ptr_to) => write!(f, "Ptr({:?})", ptr_to),
            TypeNode::Array(ref array_to, len) => write!(f, "Array({:?},{})", array_to, len),
        }
    }
}

impl Type {
    pub fn new(kind: TypeNode) -> Type {
        Type {
            head: Rc::new(kind),
        }
    }

    pub fn new_fn_type(return_type: Type, args: Vec<Type>) -> Type {
        Type::new(TypeNode::Func(return_type, args))
    }

    pub fn new_ptr_type(ptr_to: Type) -> Type {
        Type {
            head: Rc::new(TypeNode::Ptr(ptr_to)),
        }
    }

    pub fn new_array_type(array_to: Type, len: u32) -> Type {
        Type {
            head: Rc::new(TypeNode::Array(array_to, len)),
        }
    }

    pub fn get_math_binaryop_type(lhs: Type, rhs: Type) -> Type {
        if lhs.is_int_type() && rhs.is_int_type() {
            if matches!(*lhs.head, TypeNode::Char) && matches!(*rhs.head, TypeNode::Char) {
                Type::new(TypeNode::Char)
            } else {
                Type::new(TypeNode::Int)
            }
        } else {
            unreachable!()
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

    pub fn get_array_to(&self) -> Result<Type, ()> {
        match &*self.head {
            TypeNode::Array(array_to, _) => Ok(array_to.clone()),
            _ => Err(()),
        }
    }

    pub fn is_function_type(&self) -> bool {
        matches!(*self.head, TypeNode::Func(_, _))
    }

    pub fn is_int_type(&self) -> bool {
        matches!(*self.head, TypeNode::Int) || matches!(*self.head, TypeNode::Char)
    }

    pub fn is_ptr_type(&self) -> bool {
        matches!(*self.head, TypeNode::Ptr(_))
    }

    pub fn is_array_type(&self) -> bool {
        matches!(*self.head, TypeNode::Array(_, _))
    }
}
