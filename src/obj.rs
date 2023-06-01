use crate::types::Type;

use std::cell::Ref;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone)]
pub struct ObjNode {
    pub id: usize,
    pub name: String,
    pub obj_type: Type,
}

#[derive(Clone)]
pub struct Obj {
    head: Rc<RefCell<ObjNode>>,
}

impl Obj {
    pub fn new(node: ObjNode) -> Obj {
        Obj {
            head: Rc::new(RefCell::new(node)),
        }
    }

    pub fn borrow(&self) -> Ref<ObjNode> {
        (*self.head).borrow()
    }
}
