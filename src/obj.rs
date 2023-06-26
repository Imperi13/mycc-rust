use crate::types::Type;

use std::fmt;

use std::cell::Ref;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum ObjID {
    Global(usize),
    Local(usize),
}

#[derive(Clone)]
pub struct ObjNode {
    pub id: ObjID,
    pub name: String,

    pub obj_type: Type,
}

#[derive(Clone)]
pub struct Obj {
    head: Rc<RefCell<ObjNode>>,
}

impl fmt::Debug for Obj {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.borrow().id {
            ObjID::Global(id) => write!(f, "{}_global_{}", self.borrow().name, id),
            ObjID::Local(id) => write!(f, "{}_local_{}", self.borrow().name, id),
        }
    }
}

impl Obj {
    fn new(node: ObjNode) -> Obj {
        Obj {
            head: Rc::new(RefCell::new(node)),
        }
    }

    pub fn borrow(&self) -> Ref<ObjNode> {
        (*self.head).borrow()
    }

    pub fn is_global(&self) -> bool {
        matches!(self.borrow().id, ObjID::Global(_))
    }
}

#[derive(Clone)]
pub struct GlobalObjArena {
    current_id: usize,
}

impl GlobalObjArena {
    pub fn new() -> Self {
        GlobalObjArena { current_id: 0 }
    }

    pub fn publish_obj(&mut self, obj_name: &str, obj_type: Type) -> Obj {
        let node = ObjNode {
            id: ObjID::Global(self.current_id),
            name: String::from(obj_name),
            obj_type,
        };

        self.current_id += 1;
        Obj::new(node)
    }
}

#[derive(Clone)]
pub struct LocalObjArena {
    current_id: usize,
}

impl LocalObjArena {
    pub fn new() -> Self {
        LocalObjArena { current_id: 0 }
    }

    pub fn publish_obj(&mut self, obj_name: &str, obj_type: Type) -> Obj {
        let node = ObjNode {
            id: ObjID::Local(self.current_id),
            name: String::from(obj_name),
            obj_type,
        };

        self.current_id += 1;
        Obj::new(node)
    }
}
