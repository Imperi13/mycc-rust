use crate::cfg::expr::CFGBinaryOpKind;
use crate::cfg::expr::CFGBinaryOpNode;
use crate::cfg::expr::CFGExpr;
use crate::cfg::expr::CFGExprNode;
use crate::cfg::expr::CFGUnaryOpKind;
use crate::cfg::expr::CFGUnaryOpNode;
use crate::cfg::BlockID;
use crate::cfg::CFGBlock;
use crate::cfg::CFGFunction;
use crate::cfg::CFGGlobal;
use crate::cfg::CFGJump;
use crate::cfg::CFGStmt;
use crate::obj::Obj;
use crate::types::Type;
use crate::types::TypeNode;

use inkwell::attributes::AttributeLoc;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::AnyTypeEnum;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use inkwell::values::PointerValue;
use inkwell::AddressSpace;
use std::collections::HashMap;
use std::path::Path;

pub struct CodegenArena<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,

    objs_ptr: HashMap<usize, PointerValue<'ctx>>,

    // for function
    current_func: Option<FunctionValue<'ctx>>,
    entry_block: Option<BasicBlock<'ctx>>,
    return_block: Option<BasicBlock<'ctx>>,
    blocks: HashMap<usize, BasicBlock<'ctx>>,
}

impl<'ctx> CodegenArena<'ctx> {
    pub fn new(context: &Context) -> CodegenArena {
        let module = context.create_module("main");
        let builder = context.create_builder();
        CodegenArena {
            context,
            module,
            builder,
            current_func: None,
            entry_block: None,
            return_block: None,
            blocks: HashMap::new(),
            objs_ptr: HashMap::new(),
        }
    }

    pub fn codegen_all<P: AsRef<Path>>(&mut self, globals: &Vec<CFGGlobal>, output_path: P) {
        for global in globals.iter() {
            match global {
                CFGGlobal::Function(ref func) => self.codegen_func(func),
                CFGGlobal::Variable(ref obj) => self.codegen_global_variable(obj),
            };
        }

        self.module.print_to_file(output_path).unwrap();
    }

    fn convert_llvm_anytype<'a>(&'a self, c_type: &Type) -> AnyTypeEnum<'ctx> {
        match *c_type.borrow() {
            TypeNode::Void => self.context.void_type().into(),
            TypeNode::Bool => self.context.i8_type().into(),
            TypeNode::Int => self.context.i32_type().into(),
            TypeNode::Char => self.context.i8_type().into(),
            TypeNode::Ptr(ref c_ptr_to) => {
                let ptr_to = self.convert_llvm_anytype(c_ptr_to);
                match ptr_to.clone() {
                    // use i8* for void *
                    AnyTypeEnum::VoidType(_) => self
                        .context
                        .i8_type()
                        .ptr_type(AddressSpace::default())
                        .into(),
                    AnyTypeEnum::FunctionType(fn_type) => {
                        fn_type.ptr_type(AddressSpace::default()).into()
                    }
                    _ => BasicTypeEnum::try_from(ptr_to)
                        .unwrap()
                        .ptr_type(AddressSpace::default())
                        .into(),
                }
            }
            TypeNode::Func(ref return_type, ref args) => {
                let return_type = self.convert_llvm_anytype(return_type);
                let arg_type = if args.is_some() {
                    let args = args.clone().unwrap();
                    args.iter()
                        .map(|ty| self.convert_llvm_basictype(ty).into())
                        .collect::<Vec<BasicMetadataTypeEnum>>()
                } else {
                    Vec::new()
                };

                match return_type.clone() {
                    AnyTypeEnum::FunctionType(_) => panic!(),
                    AnyTypeEnum::VoidType(void_type) => void_type.fn_type(&arg_type, false).into(),
                    _ => BasicTypeEnum::try_from(return_type)
                        .unwrap()
                        .fn_type(&arg_type, false)
                        .into(),
                }
            }
            TypeNode::Array(ref c_array_to, len) => {
                let array_to = self.convert_llvm_basictype(c_array_to);
                array_to.array_type(len).into()
            }
            TypeNode::Struct(ref st_decl) => {
                if st_decl.members.is_some() {
                    let members = st_decl.members.as_ref().unwrap();
                    let mut mem_ty = Vec::new();
                    for (ty, _) in members.iter() {
                        mem_ty.push(self.convert_llvm_basictype(ty));
                    }

                    self.context.struct_type(&mem_ty, false).into()
                } else {
                    self.context.opaque_struct_type(&st_decl.tag).into()
                }
            }
        }
    }

    fn convert_llvm_basictype<'a>(&'a self, c_type: &Type) -> BasicTypeEnum<'ctx> {
        self.convert_llvm_anytype(c_type).try_into().unwrap()
    }

    fn alloc_local_obj<'a>(&'a mut self, obj: &Obj) -> PointerValue<'ctx> {
        if self.objs_ptr.contains_key(&obj.borrow().id) {
            panic!("already exists obj");
        }

        let builder = self.context.create_builder();

        let func = self.current_func.unwrap();

        let entry = func.get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        let ptr = match *obj.borrow().obj_type.borrow() {
            TypeNode::Array(ref array_to, len) => {
                let asm_type = self.convert_llvm_basictype(array_to);
                builder.build_array_alloca(
                    asm_type,
                    self.context.i32_type().const_int(len as u64, false),
                    &obj.borrow().name,
                )
            }
            _ => {
                let asm_type = self.convert_llvm_basictype(&obj.borrow().obj_type);
                builder.build_alloca(asm_type, &obj.borrow().name)
            }
        };

        self.objs_ptr.insert(obj.borrow().id, ptr);
        ptr
    }

    fn get_local_obj(&self, obj: &Obj) -> PointerValue {
        if !self.objs_ptr.contains_key(&obj.borrow().id) {
            panic!("not found obj")
        }

        self.objs_ptr.get(&obj.borrow().id).unwrap().clone()
    }

    fn codegen_func(&mut self, func: &CFGFunction) {
        let main_fn_type = self
            .convert_llvm_anytype(&func.func_obj.borrow().obj_type)
            .try_into()
            .unwrap();
        let main_fn = self
            .module
            .add_function(&func.func_obj.borrow().name, main_fn_type, None);
        let frame_pointer_attribute = self.context.create_string_attribute("frame-pointer", "all");
        main_fn.add_attribute(AttributeLoc::Function, frame_pointer_attribute);

        self.current_func = Some(main_fn);
        self.objs_ptr.insert(
            func.func_obj.borrow().id,
            main_fn.as_global_value().as_pointer_value(),
        );

        let entry_block = self.context.append_basic_block(main_fn, "entry_block");
        let return_block = self.context.append_basic_block(main_fn, "return_block");

        for (index, _) in func.blocks.iter() {
            let block = self
                .context
                .append_basic_block(main_fn, &format!("block_{}", index.to_string()));
            self.blocks.insert(index.clone(), block);
        }

        self.entry_block = Some(entry_block);
        self.return_block = Some(return_block);

        // codegen entry_block
        self.builder.position_at_end(entry_block);
        if func.retval.is_some() {
            let retval = func.retval.as_ref().unwrap();
            self.alloc_local_obj(retval);
        }
        for (i, arg) in main_fn.get_param_iter().enumerate() {
            let ptr = self.alloc_local_obj(&func.args[i]);
            self.builder.build_store(ptr, arg);
        }

        for stmt in func.entry_block.stmts.iter() {
            self.codegen_stmt(stmt);
        }

        self.codegen_jump(&func.entry_block.jump_to);

        // codegen return_block
        self.builder.position_at_end(return_block);

        for stmt in func.return_block.stmts.iter() {
            self.codegen_stmt(stmt);
        }

        if func.retval.is_some() {
            let retval = func.retval.as_ref().unwrap();

            let ptr = self.get_local_obj(retval);
            let ret_type = &retval.borrow().obj_type;
            let llvm_type = self.convert_llvm_basictype(&ret_type);
            let retval = self.builder.build_load(llvm_type, ptr, "retval");
            self.builder.build_return(Some(&retval));
        } else {
            self.builder.build_return(None);
        }

        // codegen other block

        for (_, cfg_block) in func.blocks.iter() {
            self.codegen_block(cfg_block);
        }

        self.current_func = None;
        self.entry_block = None;
        self.return_block = None;
        self.blocks = HashMap::new();
    }

    fn codegen_block(&mut self, cfg_block: &CFGBlock) {
        let block = match cfg_block.id {
            BlockID::Block(ref id) => self.blocks.get(id).unwrap().clone(),
            BlockID::Entry => panic!(),
            BlockID::Return => panic!(),
        };

        self.builder.position_at_end(block);

        for stmt in cfg_block.stmts.iter() {
            self.codegen_stmt(stmt);
        }

        self.codegen_jump(&cfg_block.jump_to);
    }

    fn codegen_stmt(&mut self, stmt: &CFGStmt) {
        match stmt {
            CFGStmt::Decl(ref obj) => {
                self.alloc_local_obj(obj);
            }
            CFGStmt::Assign(ref var_expr, ref val) => {
                let lhs_ptr = self.codegen_addr(var_expr);
                let rhs = self.codegen_expr(val);

                self.builder.build_store(lhs_ptr, rhs);
            }
            CFGStmt::FuncCall(_, _, _) => todo!(),
        }
    }

    fn codegen_jump(&mut self, jump: &CFGJump) {
        match jump {
            CFGJump::Unconditional(ref block_id) => {
                let block = match block_id {
                    BlockID::Entry => self.entry_block.unwrap(),
                    BlockID::Return => self.return_block.unwrap(),
                    BlockID::Block(ref id) => self.blocks.get(id).unwrap().clone(),
                };

                self.builder.build_unconditional_branch(block);
            }
            CFGJump::Conditional(ref cond, ref then_id, ref else_id) => {
                let then_block = match then_id {
                    BlockID::Entry => self.entry_block.unwrap(),
                    BlockID::Return => self.return_block.unwrap(),
                    BlockID::Block(ref id) => self.blocks.get(id).unwrap().clone(),
                };

                let else_block = match else_id {
                    BlockID::Entry => self.entry_block.unwrap(),
                    BlockID::Return => self.return_block.unwrap(),
                    BlockID::Block(ref id) => self.blocks.get(id).unwrap().clone(),
                };

                let zero = self
                    .convert_llvm_basictype(&cond.expr_type)
                    .into_int_type()
                    .const_int(0, false);

                let cond = self.codegen_expr(cond);
                let cond = self.builder.build_int_compare(
                    inkwell::IntPredicate::NE,
                    cond.into_int_value(),
                    zero,
                    "if_cond",
                );

                self.builder
                    .build_conditional_branch(cond, then_block, else_block);
            }
            CFGJump::Switch(ref cond, ref cases, ref default_id) => {
                let mut llvm_cases = Vec::new();

                for (case_expr, case_id) in cases.iter() {
                    let case_value = self.codegen_expr(case_expr).into_int_value();
                    let block = match case_id {
                        BlockID::Entry => self.entry_block.unwrap(),
                        BlockID::Return => self.return_block.unwrap(),
                        BlockID::Block(ref id) => self.blocks.get(id).unwrap().clone(),
                    };
                    llvm_cases.push((case_value, block));
                }

                let default_block = match default_id {
                    BlockID::Entry => self.entry_block.unwrap(),
                    BlockID::Return => self.return_block.unwrap(),
                    BlockID::Block(ref id) => self.blocks.get(id).unwrap().clone(),
                };

                let cond_value = self.codegen_expr(cond).into_int_value();

                self.builder
                    .build_switch(cond_value, default_block, &llvm_cases);
            }
            CFGJump::Return => panic!(),
            CFGJump::None => panic!(),
        }
    }

    fn codegen_global_variable(&mut self, obj: &Obj) {
        if obj.borrow().obj_type.is_function_type() {
            let main_fn_type = self
                .convert_llvm_anytype(&obj.borrow().obj_type)
                .try_into()
                .unwrap();
            let main_fn = self
                .module
                .add_function(&obj.borrow().name, main_fn_type, None);

            self.objs_ptr.insert(
                obj.borrow().id,
                main_fn.as_global_value().as_pointer_value(),
            );
        } else {
            let llvm_type = self.convert_llvm_basictype(&obj.borrow().obj_type);
            let global_obj = self.module.add_global(
                llvm_type,
                Some(AddressSpace::default()),
                &obj.borrow().name,
            );

            global_obj.set_initializer(&llvm_type.const_zero());

            self.objs_ptr
                .insert(obj.borrow().id, global_obj.as_pointer_value());
        }
    }

    fn codegen_addr(&self, ast: &CFGExpr) -> PointerValue {
        match ast.get_node() {
            CFGExprNode::Var(obj) => self.get_local_obj(&obj),
            CFGExprNode::UnaryOp(unary_node) => match unary_node.kind {
                CFGUnaryOpKind::Deref => self.codegen_expr(&unary_node.expr).into_pointer_value(),
                _ => panic!(),
            },
            CFGExprNode::Dot(ref st_expr, index) => {
                let st_ptr = self.codegen_addr(st_expr);
                let st_ty = self
                    .convert_llvm_basictype(&st_expr.expr_type)
                    .into_struct_type();

                self.builder
                    .build_struct_gep(st_ty, st_ptr, index as u32, "addr struct dot")
                    .unwrap()
            }
            CFGExprNode::Arrow(ref st_expr, index) => {
                let st_ptr = self.codegen_expr(st_expr).into_pointer_value();
                let st_ty = self
                    .convert_llvm_basictype(&st_expr.expr_type.get_ptr_to().unwrap())
                    .into_struct_type();

                self.builder
                    .build_struct_gep(st_ty, st_ptr, index as u32, "addr struct arrow")
                    .unwrap()
            }
            _ => panic!(),
        }
    }

    fn codegen_expr(&self, ast: &CFGExpr) -> BasicValueEnum {
        match ast.get_node() {
            CFGExprNode::Number(num) => {
                let llvm_type = self.convert_llvm_basictype(&ast.expr_type).into_int_type();
                llvm_type.const_int(num, false).into()
            }
            CFGExprNode::Cast(ref cast_to, ref expr) => {
                let val = self.codegen_expr(expr);

                if expr.expr_type.is_int_type() && cast_to.is_int_type() {
                    let llvm_type = self.convert_llvm_basictype(cast_to).into_int_type();
                    self.builder
                        .build_int_cast(val.into_int_value(), llvm_type, "int cast")
                        .into()
                } else if (expr.expr_type.is_int_type() || expr.expr_type.is_ptr_type())
                    && cast_to.is_bool_type()
                {
                    let val = if expr.expr_type.is_int_type() {
                        val
                    } else {
                        self.builder
                            .build_ptr_to_int(
                                val.into_pointer_value(),
                                self.context.i64_type(),
                                "ptr to int",
                            )
                            .into()
                    };

                    let int_type = if expr.expr_type.is_int_type() {
                        self.convert_llvm_basictype(&expr.expr_type).into_int_type()
                    } else {
                        self.context.i64_type()
                    };

                    let zero = int_type.const_int(0, false);
                    let cond = self.builder.build_int_compare(
                        inkwell::IntPredicate::NE,
                        val.into_int_value(),
                        zero,
                        "cast to bool",
                    );

                    self.builder
                        .build_int_cast_sign_flag(
                            cond,
                            self.context.i8_type(),
                            false,
                            "cast to bool",
                        )
                        .into()
                } else if expr.is_const_zero() && cast_to.is_ptr_type() {
                    let llvm_type = self.convert_llvm_basictype(cast_to).into_pointer_type();
                    llvm_type.const_null().into()
                } else {
                    val
                }
            }
            CFGExprNode::UnaryOp(ref node) => self.codegen_unary_op(node, &ast.expr_type),
            CFGExprNode::BinaryOp(ref node) => self.codegen_binary_op(node, &ast.expr_type),
            _ => todo!(),
        }
    }

    fn codegen_unary_op(&self, unary_node: &CFGUnaryOpNode, expr_type: &Type) -> BasicValueEnum {
        match unary_node.kind {
            CFGUnaryOpKind::Plus => self.codegen_expr(&unary_node.expr),
            CFGUnaryOpKind::Minus => {
                let expr = self.codegen_expr(&unary_node.expr);
                BasicValueEnum::IntValue(self.builder.build_int_neg(expr.into_int_value(), "neg"))
            }
            CFGUnaryOpKind::Addr => {
                BasicValueEnum::PointerValue(self.codegen_addr(&unary_node.expr))
            }
            CFGUnaryOpKind::Deref => {
                assert!(&unary_node.expr.expr_type.is_ptr_type());
                let llvm_type = self.convert_llvm_basictype(expr_type);
                let ptr = self.codegen_expr(&unary_node.expr).into_pointer_value();
                if expr_type.is_array_type() {
                    ptr.into()
                } else {
                    self.builder.build_load(llvm_type, ptr, "var")
                }
            }
            CFGUnaryOpKind::LogicalNot => {
                let expr = self.codegen_expr(&unary_node.expr);
                let zero = self
                    .convert_llvm_basictype(&unary_node.expr.expr_type)
                    .into_int_type()
                    .const_int(0, false);

                let cond = self.builder.build_int_compare(
                    inkwell::IntPredicate::EQ,
                    expr.into_int_value(),
                    zero,
                    "logical not",
                );
                self.builder
                    .build_int_cast_sign_flag(cond, self.context.i32_type(), false, "cast to i32")
                    .into()
            }
            CFGUnaryOpKind::BitNot => {
                let expr = self.codegen_expr(&unary_node.expr);
                self.builder
                    .build_not(expr.into_int_value(), "bit not")
                    .into()
            }
        }
    }

    fn codegen_binary_op(&self, binary_node: &CFGBinaryOpNode, expr_type: &Type) -> BasicValueEnum {
        match binary_node.kind {
            CFGBinaryOpKind::Add => {
                let lhs_type = &binary_node.lhs.expr_type;
                let rhs_type = &binary_node.rhs.expr_type;
                let lhs = self.codegen_expr(&binary_node.lhs);
                let rhs = self.codegen_expr(&binary_node.rhs);

                if lhs_type.is_int_type() && rhs_type.is_int_type() {
                    BasicValueEnum::IntValue(self.builder.build_int_add(
                        lhs.into_int_value(),
                        rhs.into_int_value(),
                        "add node",
                    ))
                } else if lhs_type.is_ptr_type() && rhs_type.is_int_type() {
                    let ptr_to = lhs_type.get_ptr_to().unwrap();
                    unsafe {
                        self.builder
                            .build_gep(
                                self.convert_llvm_basictype(&ptr_to),
                                lhs.into_pointer_value(),
                                &[rhs.into_int_value()],
                                "ptr_add",
                            )
                            .into()
                    }
                } else if lhs_type.is_int_type() && rhs_type.is_ptr_type() {
                    let ptr_to = rhs_type.get_ptr_to().unwrap();
                    unsafe {
                        self.builder
                            .build_gep(
                                self.convert_llvm_basictype(&ptr_to),
                                rhs.into_pointer_value(),
                                &[lhs.into_int_value()],
                                "ptr_add",
                            )
                            .into()
                    }
                } else {
                    unreachable!()
                }
            }
            CFGBinaryOpKind::Sub => {
                let lhs_type = &binary_node.lhs.expr_type;
                let rhs_type = &binary_node.rhs.expr_type;
                let lhs = self.codegen_expr(&binary_node.lhs);
                let rhs = self.codegen_expr(&binary_node.rhs);
                if lhs_type.is_int_type() && rhs_type.is_int_type() {
                    BasicValueEnum::IntValue(self.builder.build_int_sub(
                        lhs.into_int_value(),
                        rhs.into_int_value(),
                        "sub node",
                    ))
                } else if lhs_type.is_ptr_type() && rhs_type.is_int_type() {
                    let ptr_to = lhs_type.get_ptr_to().unwrap();
                    let neg_rhs = self.builder.build_int_neg(rhs.into_int_value(), "neg");
                    unsafe {
                        self.builder
                            .build_gep(
                                self.convert_llvm_basictype(&ptr_to),
                                lhs.into_pointer_value(),
                                &[neg_rhs],
                                "ptr_add",
                            )
                            .into()
                    }
                } else if lhs_type.is_ptr_type() && rhs_type.is_ptr_type() {
                    let ptr_to = lhs_type.get_ptr_to().unwrap();
                    let llvm_type = self.convert_llvm_basictype(&ptr_to);
                    self.builder
                        .build_ptr_diff(
                            llvm_type,
                            lhs.into_pointer_value(),
                            rhs.into_pointer_value(),
                            "ptr diff",
                        )
                        .into()
                } else {
                    unreachable!()
                }
            }
            CFGBinaryOpKind::Mul => {
                let lhs = self.codegen_expr(&binary_node.lhs);
                let rhs = self.codegen_expr(&binary_node.rhs);
                BasicValueEnum::IntValue(self.builder.build_int_mul(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "mul node",
                ))
            }
            CFGBinaryOpKind::Div => {
                let lhs = self.codegen_expr(&binary_node.lhs);
                let rhs = self.codegen_expr(&binary_node.rhs);
                BasicValueEnum::IntValue(self.builder.build_int_signed_div(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "div node",
                ))
            }
            CFGBinaryOpKind::Mod => {
                let lhs = self.codegen_expr(&binary_node.lhs);
                let rhs = self.codegen_expr(&binary_node.rhs);
                self.builder
                    .build_int_signed_rem(lhs.into_int_value(), rhs.into_int_value(), "mod node")
                    .into()
            }
            CFGBinaryOpKind::BitOr => {
                let lhs = self.codegen_expr(&binary_node.lhs);
                let rhs = self.codegen_expr(&binary_node.rhs);
                self.builder
                    .build_or(lhs.into_int_value(), rhs.into_int_value(), "or node")
                    .into()
            }
            CFGBinaryOpKind::BitXor => {
                let lhs = self.codegen_expr(&binary_node.lhs);
                let rhs = self.codegen_expr(&binary_node.rhs);
                self.builder
                    .build_xor(lhs.into_int_value(), rhs.into_int_value(), "or node")
                    .into()
            }
            CFGBinaryOpKind::BitAnd => {
                let lhs = self.codegen_expr(&binary_node.lhs);
                let rhs = self.codegen_expr(&binary_node.rhs);
                self.builder
                    .build_and(lhs.into_int_value(), rhs.into_int_value(), "and node")
                    .into()
            }
            CFGBinaryOpKind::Equal => {
                let lhs = self.codegen_expr(&binary_node.lhs);
                let rhs = self.codegen_expr(&binary_node.rhs);
                let cmp = self.builder.build_int_compare(
                    inkwell::IntPredicate::EQ,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "equal node",
                );
                BasicValueEnum::IntValue(self.builder.build_int_cast_sign_flag(
                    cmp,
                    self.convert_llvm_basictype(expr_type).into_int_type(),
                    false,
                    "cast to i64",
                ))
            }
            CFGBinaryOpKind::NotEqual => {
                let lhs = self.codegen_expr(&binary_node.lhs);
                let rhs = self.codegen_expr(&binary_node.rhs);
                let cmp = self.builder.build_int_compare(
                    inkwell::IntPredicate::NE,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "equal node",
                );

                BasicValueEnum::IntValue(self.builder.build_int_cast_sign_flag(
                    cmp,
                    self.convert_llvm_basictype(expr_type).into_int_type(),
                    false,
                    "cast to i64",
                ))
            }
            CFGBinaryOpKind::Less => {
                let lhs = self.codegen_expr(&binary_node.lhs);
                let rhs = self.codegen_expr(&binary_node.rhs);
                let cmp = self.builder.build_int_compare(
                    inkwell::IntPredicate::SLT,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "equal node",
                );

                BasicValueEnum::IntValue(self.builder.build_int_cast_sign_flag(
                    cmp,
                    self.convert_llvm_basictype(expr_type).into_int_type(),
                    false,
                    "cast to i64",
                ))
            }
            CFGBinaryOpKind::LessEqual => {
                let lhs = self.codegen_expr(&binary_node.lhs);
                let rhs = self.codegen_expr(&binary_node.rhs);
                let cmp = self.builder.build_int_compare(
                    inkwell::IntPredicate::SLE,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "equal node",
                );

                BasicValueEnum::IntValue(self.builder.build_int_cast_sign_flag(
                    cmp,
                    self.convert_llvm_basictype(expr_type).into_int_type(),
                    false,
                    "cast to i64",
                ))
            }
            CFGBinaryOpKind::Greater => {
                let lhs = self.codegen_expr(&binary_node.lhs);
                let rhs = self.codegen_expr(&binary_node.rhs);
                let cmp = self.builder.build_int_compare(
                    inkwell::IntPredicate::SGT,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "equal node",
                );

                BasicValueEnum::IntValue(self.builder.build_int_cast_sign_flag(
                    cmp,
                    self.convert_llvm_basictype(expr_type).into_int_type(),
                    false,
                    "cast to i64",
                ))
            }
            CFGBinaryOpKind::GreaterEqual => {
                let lhs = self.codegen_expr(&binary_node.lhs);
                let rhs = self.codegen_expr(&binary_node.rhs);
                let cmp = self.builder.build_int_compare(
                    inkwell::IntPredicate::SGE,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "equal node",
                );

                BasicValueEnum::IntValue(self.builder.build_int_cast_sign_flag(
                    cmp,
                    self.convert_llvm_basictype(expr_type).into_int_type(),
                    false,
                    "cast to i64",
                ))
            }
            CFGBinaryOpKind::LeftShift => {
                let lhs = self.codegen_expr(&binary_node.lhs);
                let rhs = self.codegen_expr(&binary_node.rhs);
                self.builder
                    .build_left_shift(
                        lhs.into_int_value(),
                        rhs.into_int_value(),
                        "left shift node",
                    )
                    .into()
            }
            CFGBinaryOpKind::RightShift => {
                let lhs = self.codegen_expr(&binary_node.lhs);
                let rhs = self.codegen_expr(&binary_node.rhs);
                self.builder
                    .build_right_shift(
                        lhs.into_int_value(),
                        rhs.into_int_value(),
                        false,
                        "left shift node",
                    )
                    .into()
            }
        }
    }
}
