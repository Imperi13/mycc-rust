use super::BlockID;
use super::BlockKind;
use super::CFGBlock;
use super::CFGFunction;
use super::CFGJump;
use super::CFGStmt;

use std::collections::VecDeque;

impl CFGFunction {
    fn rename_jump(block: &mut CFGBlock, old_id: usize, new_id: usize) {
        match block.jump_to {
            CFGJump::Unconditional(ref mut id) => {
                if id == &BlockID::new(old_id) {
                    *id = BlockID::new(new_id);
                }
            }
            CFGJump::Conditional(_, ref mut then_id, ref mut else_id) => {
                if then_id == &BlockID::new(old_id) {
                    *then_id = BlockID::new(new_id);
                }
                if else_id == &BlockID::new(old_id) {
                    *else_id = BlockID::new(new_id);
                }
            }
            CFGJump::Switch(_, ref mut cases, ref mut else_id) => {
                for (_, case) in cases.iter_mut() {
                    if case == &BlockID::new(old_id) {
                        *case = BlockID::new(new_id);
                    }
                }
                if else_id == &BlockID::new(old_id) {
                    *else_id = BlockID::new(new_id);
                }
            }
            _ => (),
        }
    }

    fn rename_block_id(&mut self, old_id: usize, new_id: usize) {
        assert!(self.blocks.contains_key(&BlockID::new(old_id)));
        assert!(!self.blocks.contains_key(&BlockID::new(new_id)));

        for (_, block) in self.blocks.iter_mut() {
            CFGFunction::rename_jump(block, old_id, new_id);
        }

        let mut new_block = self.blocks.remove(&BlockID::new(old_id)).unwrap();
        new_block.id = BlockID::new(new_id);

        self.blocks.insert(BlockID::new(new_id), new_block);
    }

    pub fn cleanup_unreachable_block(&mut self) {
        let size = self.blocks.len();
        let mut visit = vec![false; size];
        let mut queue = VecDeque::new();
        visit[self.entry_id.to_usize()] = true;
        queue.push_back(self.entry_id.clone());

        while !queue.is_empty() {
            let now = queue.pop_front().unwrap();
            let now_block = self.blocks.get(&now).unwrap();
            match now_block.jump_to {
                CFGJump::Unconditional(ref id) => {
                    if !visit[id.to_usize()] {
                        visit[id.to_usize()] = true;
                        queue.push_back(id.clone());
                    }
                }
                CFGJump::Conditional(_, ref then_id, ref else_id) => {
                    if !visit[then_id.to_usize()] {
                        visit[then_id.to_usize()] = true;
                        queue.push_back(then_id.clone());
                    }
                    if !visit[else_id.to_usize()] {
                        visit[else_id.to_usize()] = true;
                        queue.push_back(else_id.clone());
                    }
                }
                CFGJump::Switch(_, ref cases, ref default_id) => {
                    for (_, case_id) in cases.iter() {
                        if !visit[case_id.to_usize()] {
                            visit[case_id.to_usize()] = true;
                            queue.push_back(case_id.clone());
                        }
                    }
                    if !visit[default_id.to_usize()] {
                        visit[default_id.to_usize()] = true;
                        queue.push_back(default_id.clone());
                    }
                }
                CFGJump::Return(_) => (),
            }
        }
        // remove unreachable block

        let mut next_id = 0;

        for index in 0..size {
            if !visit[index] {
                self.blocks.remove(&BlockID::new(index));
            } else {
                if index != next_id {
                    self.rename_block_id(index, next_id);
                }
                next_id += 1;
            }
        }
    }

    pub fn move_declaration_to_entry(&mut self) {
        let mut decl = Vec::new();
        for (_, block) in self.blocks.iter_mut() {
            if block.kind != BlockKind::Entry {
                let decl_stmts: Vec<CFGStmt> = block
                    .stmts
                    .iter()
                    .filter(|s| matches!(s, CFGStmt::Decl(_)))
                    .map(|s| s.clone())
                    .collect();
                let others: Vec<CFGStmt> = block
                    .stmts
                    .iter()
                    .filter(|s| !matches!(s, CFGStmt::Decl(_)))
                    .map(|s| s.clone())
                    .collect();

                block.stmts = others;
                decl.extend(decl_stmts);
            }
        }

        let entry_block = self.blocks.get_mut(&self.entry_id).unwrap();
        entry_block.stmts.extend(decl);
    }
}
