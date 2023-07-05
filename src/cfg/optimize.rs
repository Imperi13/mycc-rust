mod constant_propagation;

use super::BlockID;
use super::BlockKind;
use super::CFGBlock;
use super::CFGFunction;
use super::CFGJump;
use super::CFGStmt;

use std::collections::HashMap;
use std::collections::VecDeque;

impl CFGFunction {
    // entry..1,2,3,..,return
    pub fn rename_dfs_order(&mut self) {
        assert!(self.is_valid_blocks());
        let size = self.blocks.len();
        let mut visit = vec![false; size];
        let mut rename_to = vec![0; size];
        let mut current_id = 0;
        let mut stack = VecDeque::new();

        visit[self.entry_id.to_usize()] = true;
        stack.push_back(self.entry_id.clone());

        while !stack.is_empty() {
            let now = stack.pop_back().unwrap();
            let now_block = self.blocks.get(&now).unwrap();

            rename_to[now.to_usize()] = current_id;
            current_id += 1;

            match now_block.jump_to {
                CFGJump::Unconditional(ref id) => {
                    if !visit[id.to_usize()] {
                        visit[id.to_usize()] = true;
                        stack.push_back(id.clone());
                    }
                }
                CFGJump::Conditional(_, ref then_id, ref else_id) => {
                    if !visit[else_id.to_usize()] {
                        visit[else_id.to_usize()] = true;
                        stack.push_back(else_id.clone());
                    }
                    if !visit[then_id.to_usize()] {
                        visit[then_id.to_usize()] = true;
                        stack.push_back(then_id.clone());
                    }
                }
                CFGJump::Switch(_, ref cases, ref default_id) => {
                    if !visit[default_id.to_usize()] {
                        visit[default_id.to_usize()] = true;
                        stack.push_back(default_id.clone());
                    }
                    for (_, case_id) in cases.iter().rev() {
                        if !visit[case_id.to_usize()] {
                            visit[case_id.to_usize()] = true;
                            stack.push_back(case_id.clone());
                        }
                    }
                }
                CFGJump::Return(_) => (),
            }
        }

        let mut block_map = HashMap::new();

        for (block_id, mut block) in self.blocks.clone().into_iter() {
            block.id = BlockID::new(rename_to[block.id.to_usize()]);
            match block.jump_to {
                CFGJump::Unconditional(ref mut id) => {
                    *id = BlockID::new(rename_to[id.to_usize()]);
                }
                CFGJump::Conditional(_, ref mut then_id, ref mut else_id) => {
                    *then_id = BlockID::new(rename_to[then_id.to_usize()]);
                    *else_id = BlockID::new(rename_to[else_id.to_usize()]);
                }
                CFGJump::Switch(_, ref mut cases, ref mut else_id) => {
                    for (_, case_id) in cases.iter_mut() {
                        *case_id = BlockID::new(rename_to[case_id.to_usize()]);
                    }
                    *else_id = BlockID::new(rename_to[else_id.to_usize()]);
                }
                _ => (),
            }

            block_map.insert(BlockID::new(rename_to[block_id.to_usize()]), block);
        }

        self.blocks = block_map;
        self.entry_id = BlockID::new(rename_to[self.entry_id.to_usize()]);
        self.return_id = BlockID::new(rename_to[self.return_id.to_usize()]);

        assert!(self.is_valid_blocks());
    }

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
        assert!(self.is_valid_blocks());

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

        assert!(self.is_valid_blocks());
    }

    pub fn move_declaration_to_entry(&mut self) {
        assert!(self.is_valid_blocks());

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

        assert!(self.is_valid_blocks());
    }

    pub fn extended_basic_block(&self) -> Vec<Vec<BlockID>> {
        let size = self.blocks.len();
        let mut arena = DfsArena::new(size);
        let mut ret = Vec::new();

        for index in 0..size {
            if !arena.visit[index] {
                arena.visit[index] = true;
                ret.extend(arena.dfs(self, &BlockID::new(index)));
            }
        }

        ret
    }
}

struct DfsArena {
    visit: Vec<bool>,
}

impl DfsArena {
    pub fn new(size: usize) -> Self {
        DfsArena {
            visit: vec![false; size],
        }
    }

    pub fn dfs(&mut self, func: &CFGFunction, block_id: &BlockID) -> Vec<Vec<BlockID>> {
        let block = func.blocks.get(block_id).unwrap();
        let mut ret = Vec::new();

        match block.jump_to {
            CFGJump::Unconditional(ref id) => {
                let block = func.blocks.get(id).unwrap();
                if block.pred_blocks.len() == 1 {
                    self.visit[id.to_usize()] = true;
                    ret.extend(
                        self.dfs(func, id)
                            .into_iter()
                            .map(|e| {
                                let mut tmp = vec![block_id.clone(); 1];
                                tmp.extend(e);
                                tmp
                            })
                            .collect::<Vec<Vec<BlockID>>>(),
                    );
                }
            }
            CFGJump::Conditional(_, ref then_id, ref else_id) => {
                let block = func.blocks.get(then_id).unwrap();
                if block.pred_blocks.len() == 1 {
                    self.visit[then_id.to_usize()] = true;
                    ret.extend(
                        self.dfs(func, then_id)
                            .into_iter()
                            .map(|e| {
                                let mut tmp = vec![block_id.clone(); 1];
                                tmp.extend(e);
                                tmp
                            })
                            .collect::<Vec<Vec<BlockID>>>(),
                    );
                }

                let block = func.blocks.get(else_id).unwrap();
                if block.pred_blocks.len() == 1 {
                    self.visit[else_id.to_usize()] = true;
                    ret.extend(
                        self.dfs(func, else_id)
                            .into_iter()
                            .map(|e| {
                                let mut tmp = vec![block_id.clone(); 1];
                                tmp.extend(e);
                                tmp
                            })
                            .collect::<Vec<Vec<BlockID>>>(),
                    );
                }
            }
            CFGJump::Switch(_, ref cases, ref default_id) => {
                let block = func.blocks.get(default_id).unwrap();
                if block.pred_blocks.len() == 1 {
                    self.visit[default_id.to_usize()] = true;
                    ret.extend(
                        self.dfs(func, default_id)
                            .into_iter()
                            .map(|e| {
                                let mut tmp = vec![block_id.clone(); 1];
                                tmp.extend(e);
                                tmp
                            })
                            .collect::<Vec<Vec<BlockID>>>(),
                    );
                }
                for (_, case_id) in cases.iter().rev() {
                    let block = func.blocks.get(case_id).unwrap();
                    if block.pred_blocks.len() == 1 {
                        self.visit[case_id.to_usize()] = true;
                        ret.extend(
                            self.dfs(func, case_id)
                                .into_iter()
                                .map(|e| {
                                    let mut tmp = vec![block_id.clone(); 1];
                                    tmp.extend(e);
                                    tmp
                                })
                                .collect::<Vec<Vec<BlockID>>>(),
                        );
                    }
                }
            }
            CFGJump::Return(_) => (),
        }

        if ret.len() == 0 {
            vec![vec![block_id.clone(); 1]; 1]
        } else {
            ret
        }
    }
}
