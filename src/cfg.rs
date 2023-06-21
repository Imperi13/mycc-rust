use crate::ast::ASTBlockStmt;
use crate::ast::ASTExpr;
use crate::ast::ASTExprNode;
use crate::ast::ASTGlobal;
use crate::ast::ASTStmt;
use crate::ast::ASTStmtNode;
use crate::ast::AssignKind;
use crate::ast::AssignNode;
use crate::obj::Obj;
use crate::obj::ObjArena;

use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt;

#[derive(Clone)]
pub enum CFGStmt {
    Decl(Obj),
    Expr(ASTExpr),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BlockID {
    Entry,
    Return,
    Block(usize),
}

#[derive(Clone, Debug)]
pub enum CFGJump {
    None,
    Return,
    Unconditional(BlockID),
    Conditional(ASTExpr, BlockID, BlockID),
    Switch(ASTExpr, Vec<(ASTExpr, BlockID)>, BlockID),
}

#[derive(Clone)]
pub struct CFGBlock {
    pub id: BlockID,
    pub stmts: Vec<CFGStmt>,
    pub jump_to: CFGJump,
}

impl CFGBlock {
    pub fn new(id: BlockID) -> Self {
        CFGBlock {
            id,
            stmts: Vec::new(),
            jump_to: CFGJump::None,
        }
    }
}

impl fmt::Debug for CFGBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "ID: {:?}", self.id)?;
        writeln!(f, "stmts:")?;
        for stmt in self.stmts.iter() {
            match stmt {
                CFGStmt::Decl(ref _obj) => writeln!(f, "Decl "),
                CFGStmt::Expr(ref expr) => expr.fmt_with_indent(f, "\t"),
            }?;
        }

        writeln!(f, "jump_to: {:?}", self.jump_to)
    }
}

#[derive(Clone)]
pub struct CFGFunction {
    pub func_obj: Obj,
    pub args: Vec<Obj>,
    pub retval: Option<Obj>,

    pub entry_block: CFGBlock,
    pub return_block: CFGBlock,
    pub blocks: HashMap<usize, CFGBlock>,
}

impl CFGFunction {
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

        for index in 0..size {
            if !visit[index] {
                self.blocks.remove(&index);
            }
        }
    }
}

#[derive(Clone)]
pub enum CFGGlobal {
    Function(CFGFunction),
    Variable(Obj),
}

impl fmt::Debug for CFGGlobal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CFGGlobal::Function(ref func) => {
                writeln!(f, "{:?}", func.entry_block)?;
                for (_, block) in func.blocks.iter() {
                    writeln!(f, "{:?}", block)?;
                }
                writeln!(f, "{:?}", func.return_block)
            }
            CFGGlobal::Variable(ref obj) => {
                writeln!(f, "Variable {}", obj.borrow().name)
            }
        }
    }
}

pub fn gen_cfg_all(obj_arena: &mut ObjArena, ast_all: &Vec<ASTGlobal>) -> Vec<CFGGlobal> {
    let mut cfg_globals = Vec::new();
    for ast in ast_all.iter() {
        let cfg_global = match ast {
            ASTGlobal::Function(ref func_obj, ref args, ref stmts) => {
                let mut arena = CFGArena::new(obj_arena);
                CFGGlobal::Function(arena.gen_cfg_function(func_obj, args, stmts))
            }
            ASTGlobal::Variable(ref obj) => CFGGlobal::Variable(obj.clone()),
        };

        cfg_globals.push(cfg_global);
    }

    cfg_globals
}

struct CFGArena<'a> {
    obj_arena: &'a mut ObjArena,
    retval: Option<Obj>,

    entry_block: CFGBlock,
    return_block: CFGBlock,
    blocks: HashMap<usize, CFGBlock>,

    break_map: HashMap<usize, usize>,
    continue_map: HashMap<usize, usize>,
    default_map: HashMap<usize, usize>,
    case_map: HashMap<usize, Vec<(ASTExpr, BlockID)>>,

    current_id: usize,
    next_id: usize,
    current_stmts: Vec<CFGStmt>,
}

impl<'a> CFGArena<'a> {
    pub fn new(obj_arena: &'a mut ObjArena) -> Self {
        CFGArena {
            obj_arena,
            retval: None,
            entry_block: CFGBlock::new(BlockID::Entry),
            return_block: CFGBlock::new(BlockID::Return),
            blocks: HashMap::new(),
            break_map: HashMap::new(),
            continue_map: HashMap::new(),
            default_map: HashMap::new(),
            case_map: HashMap::new(),
            current_id: 0,
            next_id: 1,
            current_stmts: Vec::new(),
        }
    }

    pub fn gen_cfg_function(
        &mut self,
        func_obj: &Obj,
        args: &Vec<Obj>,
        stmts: &Vec<ASTBlockStmt>,
    ) -> CFGFunction {
        if !func_obj
            .borrow()
            .obj_type
            .get_return_type()
            .unwrap()
            .is_void_type()
        {
            let retval = self.obj_arena.publish_obj(
                "retval",
                func_obj.borrow().obj_type.get_return_type().unwrap(),
            );

            self.retval = Some(retval);
        }

        self.entry_block.jump_to = CFGJump::Unconditional(BlockID::Block(self.current_id));

        for block_stmt in stmts.iter() {
            match block_stmt {
                ASTBlockStmt::Stmt(ref stmt) => self.push_stmt(stmt),
                ASTBlockStmt::Declaration(ref obj) => {
                    self.entry_block.stmts.push(CFGStmt::Decl(obj.clone()));
                }
            }
        }

        let last_block = CFGBlock {
            id: BlockID::Block(self.current_id),
            stmts: self.current_stmts.clone(),
            jump_to: CFGJump::Unconditional(BlockID::Return),
        };

        self.blocks.insert(self.current_id, last_block);
        self.current_id = self.next_id;
        self.next_id += 1;
        self.current_stmts = Vec::new();

        self.return_block.jump_to = CFGJump::Return;

        let mut cfg_func = CFGFunction {
            func_obj: func_obj.clone(),
            args: args.clone(),
            retval: self.retval.clone(),
            entry_block: self.entry_block.clone(),
            return_block: self.return_block.clone(),
            blocks: self.blocks.clone(),
        };

        cfg_func.cleanup_unreachable_block();

        cfg_func
    }

    pub fn push_stmt(&mut self, stmt: &ASTStmt) {
        match stmt.get_node() {
            ASTStmtNode::ExprStmt(ref expr) => {
                self.current_stmts.push(CFGStmt::Expr(expr.clone()));
            }
            ASTStmtNode::Default(ref stmt, switch_id) => {
                let block_id = self.next_id;
                self.next_id += 1;

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Block(block_id)),
                };

                self.blocks.insert(self.current_id, block);
                self.default_map.insert(switch_id, block_id);

                // stmt
                self.current_id = block_id;
                self.current_stmts = Vec::new();

                self.push_stmt(stmt);
            }
            ASTStmtNode::Case(ref expr, ref stmt, switch_id) => {
                let block_id = self.next_id;
                self.next_id += 1;

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Block(block_id)),
                };

                self.blocks.insert(self.current_id, block);
                self.case_map
                    .get_mut(&switch_id)
                    .unwrap()
                    .push((expr.clone(), BlockID::Block(block_id)));

                // stmt
                self.current_id = block_id;
                self.current_stmts = Vec::new();

                self.push_stmt(stmt);
            }
            ASTStmtNode::Block(ref stmts) => {
                for block_stmt in stmts.iter() {
                    match block_stmt {
                        ASTBlockStmt::Stmt(ref stmt) => self.push_stmt(stmt),
                        ASTBlockStmt::Declaration(ref obj) => {
                            self.entry_block.stmts.push(CFGStmt::Decl(obj.clone()));
                        }
                    }
                }
            }
            ASTStmtNode::Return(ref expr) => {
                if expr.is_some() {
                    let expr = expr.as_ref().unwrap();
                    let retval = self.retval.clone().unwrap();

                    let retval_expr = ASTExpr::new(
                        ASTExprNode::Var(retval.clone()),
                        retval.borrow().obj_type.clone(),
                    );
                    let assign_expr = ASTExpr::new(
                        ASTExprNode::Assign(AssignNode {
                            lhs: retval_expr,
                            rhs: expr.clone(),
                            kind: AssignKind::Assign,
                        }),
                        retval.borrow().obj_type.clone(),
                    );
                    self.current_stmts.push(CFGStmt::Expr(assign_expr));
                }

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Return),
                };

                self.blocks.insert(self.current_id, block);
                self.current_id = self.next_id;
                self.next_id += 1;
                self.current_stmts = Vec::new();
            }
            ASTStmtNode::If(ref cond, ref then_stmt, ref else_stmt) => {
                if else_stmt.is_some() {
                    let else_stmt = else_stmt.as_ref().unwrap();

                    let then_id = self.next_id;
                    let else_id = self.next_id + 1;
                    let after_id = self.next_id + 2;
                    self.next_id += 3;

                    let block = CFGBlock {
                        id: BlockID::Block(self.current_id),
                        stmts: self.current_stmts.clone(),
                        jump_to: CFGJump::Conditional(
                            cond.clone(),
                            BlockID::Block(then_id),
                            BlockID::Block(else_id),
                        ),
                    };

                    self.blocks.insert(self.current_id, block);

                    // then_stmt
                    self.current_id = then_id;
                    self.current_stmts = Vec::new();

                    self.push_stmt(then_stmt);

                    let block = CFGBlock {
                        id: BlockID::Block(self.current_id),
                        stmts: self.current_stmts.clone(),
                        jump_to: CFGJump::Unconditional(BlockID::Block(after_id)),
                    };

                    self.blocks.insert(self.current_id, block);

                    // else_stmt
                    self.current_id = else_id;
                    self.current_stmts = Vec::new();

                    self.push_stmt(else_stmt);

                    let block = CFGBlock {
                        id: BlockID::Block(self.current_id),
                        stmts: self.current_stmts.clone(),
                        jump_to: CFGJump::Unconditional(BlockID::Block(after_id)),
                    };

                    self.blocks.insert(self.current_id, block);

                    // after
                    self.current_id = after_id;
                    self.current_stmts = Vec::new();
                } else {
                    let then_id = self.next_id;
                    let after_id = self.next_id + 1;
                    self.next_id += 2;

                    let block = CFGBlock {
                        id: BlockID::Block(self.current_id),
                        stmts: self.current_stmts.clone(),
                        jump_to: CFGJump::Conditional(
                            cond.clone(),
                            BlockID::Block(then_id),
                            BlockID::Block(after_id),
                        ),
                    };

                    self.blocks.insert(self.current_id, block);

                    // then_stmt
                    self.current_id = then_id;
                    self.current_stmts = Vec::new();

                    self.push_stmt(then_stmt);

                    let block = CFGBlock {
                        id: BlockID::Block(self.current_id),
                        stmts: self.current_stmts.clone(),
                        jump_to: CFGJump::Unconditional(BlockID::Block(after_id)),
                    };

                    self.blocks.insert(self.current_id, block);

                    // after
                    self.current_id = after_id;
                    self.current_stmts = Vec::new();
                }
            }
            ASTStmtNode::Switch(ref switch_node) => {
                let stmt_id = self.next_id;
                let after_id = self.next_id + 1;
                self.next_id += 2;

                self.break_map.insert(switch_node.break_id, after_id);
                self.case_map.insert(switch_node.switch_id, Vec::new());

                let previous_id = self.current_id;
                let previous_stmts = self.current_stmts.clone();

                // stmt
                self.current_id = stmt_id;
                self.current_stmts = Vec::new();

                self.push_stmt(&switch_node.stmt);

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Block(after_id)),
                };
                self.blocks.insert(self.current_id, block);

                // switch jump

                let cases = self.case_map.remove(&switch_node.switch_id).unwrap();

                let default_block = if self.default_map.contains_key(&switch_node.switch_id) {
                    BlockID::Block(
                        self.default_map
                            .get(&switch_node.switch_id)
                            .unwrap()
                            .clone(),
                    )
                } else {
                    BlockID::Block(after_id)
                };

                let block = CFGBlock {
                    id: BlockID::Block(previous_id),
                    stmts: previous_stmts,
                    jump_to: CFGJump::Switch(switch_node.cond.clone(), cases, default_block),
                };

                self.blocks.insert(previous_id, block);

                // after
                self.current_id = after_id;
                self.current_stmts = Vec::new();
            }
            ASTStmtNode::While(ref while_node) => {
                let cond_id = self.next_id;
                let loop_id = self.next_id + 1;
                let after_id = self.next_id + 2;
                self.next_id += 3;

                self.break_map.insert(while_node.break_id, after_id);
                self.continue_map.insert(while_node.continue_id, cond_id);

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Block(cond_id)),
                };

                self.blocks.insert(self.current_id, block);

                // cond
                self.current_id = cond_id;
                self.current_stmts = Vec::new();

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Conditional(
                        while_node.cond.clone(),
                        BlockID::Block(loop_id),
                        BlockID::Block(after_id),
                    ),
                };

                self.blocks.insert(self.current_id, block);

                // loop
                self.current_id = loop_id;
                self.current_stmts = Vec::new();

                self.push_stmt(&while_node.loop_stmt);

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Block(cond_id)),
                };
                self.blocks.insert(self.current_id, block);

                // after
                self.current_id = after_id;
                self.current_stmts = Vec::new();
            }
            ASTStmtNode::DoWhile(ref dowhile_node) => {
                let loop_id = self.next_id;
                let cond_id = self.next_id + 1;
                let after_id = self.next_id + 2;
                self.next_id += 3;

                self.break_map.insert(dowhile_node.break_id, after_id);
                self.continue_map.insert(dowhile_node.continue_id, cond_id);

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Block(loop_id)),
                };

                self.blocks.insert(self.current_id, block);

                // loop
                self.current_id = loop_id;
                self.current_stmts = Vec::new();

                self.push_stmt(&dowhile_node.loop_stmt);

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Block(cond_id)),
                };
                self.blocks.insert(self.current_id, block);

                // cond
                self.current_id = cond_id;
                self.current_stmts = Vec::new();

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Conditional(
                        dowhile_node.cond.clone(),
                        BlockID::Block(loop_id),
                        BlockID::Block(after_id),
                    ),
                };

                self.blocks.insert(self.current_id, block);

                // after
                self.current_id = after_id;
                self.current_stmts = Vec::new();
            }
            ASTStmtNode::For(ref for_node) => {
                let cond_id = self.next_id;
                let step_id = self.next_id + 1;
                let loop_id = self.next_id + 2;
                let after_id = self.next_id + 3;
                self.next_id += 4;

                self.break_map.insert(for_node.break_id, after_id);
                self.continue_map.insert(for_node.continue_id, step_id);

                if for_node.start.is_some() {
                    let start = for_node.start.as_ref().unwrap();
                    self.current_stmts.push(CFGStmt::Expr(start.clone()));
                }

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Block(cond_id)),
                };

                self.blocks.insert(self.current_id, block);

                // cond
                self.current_id = cond_id;
                self.current_stmts = Vec::new();

                let jump_to = if for_node.cond.is_some() {
                    CFGJump::Conditional(
                        for_node.cond.as_ref().unwrap().clone(),
                        BlockID::Block(loop_id),
                        BlockID::Block(after_id),
                    )
                } else {
                    CFGJump::Unconditional(BlockID::Block(loop_id))
                };

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to,
                };

                self.blocks.insert(self.current_id, block);

                // loop
                self.current_id = loop_id;
                self.current_stmts = Vec::new();

                self.push_stmt(&for_node.loop_stmt);

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Block(step_id)),
                };
                self.blocks.insert(self.current_id, block);

                // step
                self.current_id = step_id;
                self.current_stmts = Vec::new();

                if for_node.step.is_some() {
                    let step = for_node.step.as_ref().unwrap();
                    self.current_stmts.push(CFGStmt::Expr(step.clone()));
                }

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Block(cond_id)),
                };
                self.blocks.insert(self.current_id, block);

                // after
                self.current_id = after_id;
                self.current_stmts = Vec::new();
            }
            ASTStmtNode::Break(ref stmt_id) => {
                let break_id = self.break_map.get(stmt_id).unwrap().clone();
                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Block(break_id)),
                };

                self.blocks.insert(self.current_id, block);
                self.current_id = self.next_id;
                self.next_id += 1;
                self.current_stmts = Vec::new();
            }
            ASTStmtNode::Continue(ref stmt_id) => {
                let continue_id = self.continue_map.get(stmt_id).unwrap().clone();

                let block = CFGBlock {
                    id: BlockID::Block(self.current_id),
                    stmts: self.current_stmts.clone(),
                    jump_to: CFGJump::Unconditional(BlockID::Block(continue_id)),
                };

                self.blocks.insert(self.current_id, block);
                self.current_id = self.next_id;
                self.next_id += 1;
                self.current_stmts = Vec::new();
            }
        }
    }
}
