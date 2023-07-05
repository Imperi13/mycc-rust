use std::cell::Ref;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Clone)]
pub struct StructDecl {
    pub id: usize,
    pub tag: String,
    pub members: Option<Vec<(Type, String)>>,
}

#[derive(Clone)]
pub enum TypeNode {
    Void,
    Bool,
    Int,
    Char,
    Func(Type, Option<Vec<Type>>),
    Ptr(Type),
    Array(Type, u32),
    Struct(StructDecl),
}

#[derive(Clone)]
pub struct Type {
    head: Rc<RefCell<TypeNode>>,
}

impl PartialEq<Type> for Type {
    #[inline]
    fn eq(&self, rhs: &Type) -> bool {
        match (&*self.head.borrow(), &*rhs.head.borrow()) {
            (TypeNode::Void, TypeNode::Void) => true,
            (TypeNode::Int, TypeNode::Int) => true,
            (TypeNode::Char, TypeNode::Char) => true,
            (TypeNode::Bool, TypeNode::Bool) => true,
            (TypeNode::Ptr(ref lhs_to), TypeNode::Ptr(ref rhs_to)) => lhs_to == rhs_to,
            (TypeNode::Struct(ref lhs_st), TypeNode::Struct(ref rhs_st)) => lhs_st.id == rhs_st.id,
            (TypeNode::Array(ref lhs_to, lhs_len), TypeNode::Array(ref rhs_to, rhs_len)) => {
                lhs_to == rhs_to && lhs_len == rhs_len
            }
            (
                TypeNode::Func(ref _lhs_ret, ref _lhs_args),
                TypeNode::Func(ref _rhs_ret, ref _rhs_args),
            ) => unimplemented!(),
            _ => false,
        }
    }
}

impl Eq for Type {}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self.head.borrow() {
            TypeNode::Void => write!(f, "Void"),
            TypeNode::Bool => write!(f, "Bool"),
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
            TypeNode::Struct(_) => unimplemented!(),
        }
    }
}

impl Type {
    pub fn new(kind: TypeNode) -> Type {
        Type {
            head: Rc::new(RefCell::new(kind)),
        }
    }

    pub fn new_fn_type(return_type: Type, args: Option<Vec<Type>>) -> Type {
        Type::new(TypeNode::Func(return_type, args))
    }

    pub fn new_ptr_type(ptr_to: Type) -> Type {
        Type::new(TypeNode::Ptr(ptr_to))
    }

    pub fn new_array_type(array_to: Type, len: u32) -> Type {
        Type::new(TypeNode::Array(array_to, len))
    }

    pub fn get_math_binaryop_type(lhs: Type, rhs: Type) -> Type {
        if lhs.is_int_type() && rhs.is_int_type() {
            if matches!(*lhs.head.borrow(), TypeNode::Char)
                && matches!(*rhs.head.borrow(), TypeNode::Char)
            {
                Type::new(TypeNode::Char)
            } else {
                Type::new(TypeNode::Int)
            }
        } else {
            unreachable!()
        }
    }

    pub fn borrow(&self) -> Ref<TypeNode> {
        (*self.head).borrow()
    }

    pub fn get_ptr_to(&self) -> Result<Type, ()> {
        match *self.head.borrow() {
            TypeNode::Ptr(ref ptr_to) => Ok(ptr_to.clone()),
            _ => Err(()),
        }
    }

    pub fn get_array_to(&self) -> Result<Type, ()> {
        match *self.head.borrow() {
            TypeNode::Array(ref array_to, _) => Ok(array_to.clone()),
            _ => Err(()),
        }
    }

    pub fn get_return_type(&self) -> Result<Type, ()> {
        match *self.head.borrow() {
            TypeNode::Func(ref ty, _) => Ok(ty.clone()),
            _ => Err(()),
        }
    }

    pub fn get_arg_types(&self) -> Result<Option<Vec<Type>>, ()> {
        match *self.head.borrow() {
            TypeNode::Func(_, ref args) => Ok(args.clone()),
            _ => Err(()),
        }
    }

    pub fn get_struct_member(&self, name: &str) -> (usize, Type) {
        match *self.head.borrow() {
            TypeNode::Struct(ref st_decl) => {
                if st_decl.members.is_none() {
                    panic!()
                }
                let members = st_decl.members.as_ref().unwrap();

                for (index, (mem_ty, mem_name)) in members.iter().enumerate() {
                    if mem_name == name {
                        return (index, mem_ty.clone());
                    }
                }
                panic!()
            }
            _ => panic!(),
        }
    }

    pub fn is_function_type(&self) -> bool {
        matches!(*self.head.borrow(), TypeNode::Func(_, _))
    }

    pub fn is_int_type(&self) -> bool {
        matches!(*self.head.borrow(), TypeNode::Int)
            || matches!(*self.head.borrow(), TypeNode::Char)
    }

    pub fn is_void_type(&self) -> bool {
        matches!(*self.head.borrow(), TypeNode::Void)
    }

    pub fn is_void_ptr_type(&self) -> bool {
        self.is_ptr_type() && self.get_ptr_to().unwrap().is_void_type()
    }

    pub fn is_bool_type(&self) -> bool {
        matches!(*self.head.borrow(), TypeNode::Bool)
    }

    pub fn is_ptr_type(&self) -> bool {
        matches!(*self.head.borrow(), TypeNode::Ptr(_))
    }

    pub fn is_array_type(&self) -> bool {
        matches!(*self.head.borrow(), TypeNode::Array(_, _))
    }

    pub fn is_struct_type(&self) -> bool {
        matches!(*self.head.borrow(), TypeNode::Struct(_))
    }

    pub fn is_complete_type(&self) -> bool {
        match *self.head.borrow() {
            TypeNode::Int | TypeNode::Char | TypeNode::Bool | TypeNode::Ptr(_) => true,
            TypeNode::Array(ref array_to, _) => array_to.is_complete_type(),
            TypeNode::Struct(ref st_decl) => st_decl.members.is_some(),
            TypeNode::Func(_, _) => false,
            TypeNode::Void => false,
        }
    }
}
