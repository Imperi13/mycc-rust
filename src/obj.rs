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
    fn new(node: ObjNode) -> Obj {
        Obj {
            head: Rc::new(RefCell::new(node)),
        }
    }

    pub fn borrow(&self) -> Ref<ObjNode> {
        (*self.head).borrow()
    }
}

pub struct ObjArena {
    current_id: usize,
}

impl ObjArena {
    pub fn new() -> Self {
        ObjArena { current_id: 0 }
    }

    pub fn publish_obj(&mut self, obj_name: &str, obj_type: Type) -> Obj {
        let node = ObjNode {
            id: self.current_id,
            name: String::from(obj_name),
            obj_type,
        };

        self.current_id += 1;
        Obj::new(node)
    }
}
