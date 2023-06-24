use super::BlockID;
use super::CFGBlock;
use super::CFGFunction;
use super::CFGJump;

use std::collections::VecDeque;

impl CFGFunction {
    fn rename_jump(block: &mut CFGBlock, old_id: usize, new_id: usize) {
        match block.jump_to {
            CFGJump::Unconditional(ref mut id) => {
                if id == &BlockID::Block(old_id) {
                    *id = BlockID::Block(new_id);
                }
            }
            CFGJump::Conditional(_, ref mut then_id, ref mut else_id) => {
                if then_id == &BlockID::Block(old_id) {
                    *then_id = BlockID::Block(new_id);
                }
                if else_id == &BlockID::Block(old_id) {
                    *else_id = BlockID::Block(new_id);
                }
            }
            CFGJump::Switch(_, ref mut cases, ref mut else_id) => {
                for (_, case) in cases.iter_mut() {
                    if case == &BlockID::Block(old_id) {
                        *case = BlockID::Block(new_id);
                    }
                }
                if else_id == &BlockID::Block(old_id) {
                    *else_id = BlockID::Block(new_id);
                }
            }
            _ => (),
        }
    }

    fn rename_block_id(&mut self, old_id: usize, new_id: usize) {
        assert!(self.blocks.contains_key(&old_id));
        assert!(!self.blocks.contains_key(&new_id));

        for (_, block) in self.blocks.iter_mut() {
            CFGFunction::rename_jump(block, old_id, new_id);
        }

        let mut new_block = self.blocks.remove(&old_id).unwrap();
        new_block.id = BlockID::Block(new_id);

        self.blocks.insert(new_id, new_block);
    }

    pub fn cleanup_unreachable_block(&mut self) {
        let size = self.blocks.len();

        let mut visit = vec![false; size];
        let mut queue = VecDeque::new();

        match self.entry_block.jump_to {
            CFGJump::Unconditional(ref id) => match id.clone() {
                BlockID::Block(num) => {
                    visit[num] = true;
                    queue.push_back(num);
                }
                _ => (),
            },
            CFGJump::Conditional(_, ref then_id, ref else_id) => {
                match then_id.clone() {
                    BlockID::Block(num) => {
                        visit[num] = true;
                        queue.push_back(num);
                    }
                    _ => (),
                }
                match else_id.clone() {
                    BlockID::Block(num) => {
                        visit[num] = true;
                        queue.push_back(num);
                    }
                    _ => (),
                }
            }
            _ => panic!(),
        }

        // bfs

        while !queue.is_empty() {
            let now = queue.pop_front().unwrap();

            let now_block = self.blocks.get(&now).unwrap();

            match now_block.jump_to {
                CFGJump::Unconditional(ref id) => match id.clone() {
                    BlockID::Block(num) => {
                        if !visit[num] {
                            visit[num] = true;
                            queue.push_back(num);
                        }
                    }
                    _ => (),
                },
                CFGJump::Conditional(_, ref then_id, ref else_id) => {
                    match then_id.clone() {
                        BlockID::Block(num) => {
                            if !visit[num] {
                                visit[num] = true;
                                queue.push_back(num);
                            }
                        }
                        _ => (),
                    }
                    match else_id.clone() {
                        BlockID::Block(num) => {
                            if !visit[num] {
                                visit[num] = true;
                                queue.push_back(num);
                            }
                        }
                        _ => (),
                    }
                }
                CFGJump::Switch(_, ref cases, ref default) => {
                    for (_, case_to) in cases.iter() {
                        match case_to.clone() {
                            BlockID::Block(num) => {
                                if !visit[num] {
                                    visit[num] = true;
                                    queue.push_back(num);
                                }
                            }
                            _ => (),
                        }
                    }
                    match default.clone() {
                        BlockID::Block(num) => {
                            if !visit[num] {
                                visit[num] = true;
                                queue.push_back(num);
                            }
                        }
                        _ => (),
                    }
                }
                _ => panic!(),
            }
        }

        // remove unreachable block

        let mut next_id = 0;

        for index in 0..size {
            if !visit[index] {
                self.blocks.remove(&index);
            } else {
                if index != next_id {
                    self.rename_block_id(index, next_id);
                }
                next_id += 1;
            }
        }
    }
}
