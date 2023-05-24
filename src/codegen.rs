use crate::ast::ASTExpr;
use crate::ast::ASTExprNode;
use crate::ast::ASTGlobal;
use crate::ast::ASTStmt;
use crate::ast::ASTStmtNode;
use crate::ast::AssignKind;
use crate::ast::AssignNode;
use crate::ast::BinaryOpKind;
use crate::ast::BinaryOpNode;
use crate::ast::UnaryOpKind;
use crate::ast::UnaryOpNode;
use crate::parse::Obj;
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
use inkwell::values::BasicMetadataValueEnum;
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

    current_func: Option<FunctionValue<'ctx>>,
    objs_ptr: HashMap<usize, PointerValue<'ctx>>,
    break_block: HashMap<usize, BasicBlock<'ctx>>,
    continue_block: HashMap<usize, BasicBlock<'ctx>>,
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
            objs_ptr: HashMap::new(),
            break_block: HashMap::new(),
            continue_block: HashMap::new(),
        }
    }

    pub fn codegen_all(&mut self, globals: &Vec<ASTGlobal>, output_path: &str) {
        for obj in globals.iter() {
            match obj {
                ASTGlobal::Function(_, _, _) => self.codegen_func(obj),
                ASTGlobal::Variable(_) => self.codegen_global_variable(obj),
            };
        }

        self.print_to_file(output_path);
    }

    fn print_to_file(&self, filepath: &str) {
        let path = Path::new(filepath);
        self.module.print_to_file(path).unwrap();
    }

    fn convert_llvm_anytype<'a>(&'a self, c_type: &Type) -> AnyTypeEnum<'ctx> {
        match c_type.get_node() {
            TypeNode::Int => self.context.i32_type().into(),
            TypeNode::Char => self.context.i8_type().into(),
            TypeNode::Ptr(ref c_ptr_to) => {
                let ptr_to = self.convert_llvm_anytype(c_ptr_to);
                match ptr_to.clone() {
                    AnyTypeEnum::VoidType(_) => panic!(),
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
                let arg_type = args
                    .iter()
                    .map(|ty| self.convert_llvm_basictype(ty).into())
                    .collect::<Vec<BasicMetadataTypeEnum>>();
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
        }
    }

    fn convert_llvm_basictype<'a>(&'a self, c_type: &Type) -> BasicTypeEnum<'ctx> {
        self.convert_llvm_anytype(c_type).try_into().unwrap()
    }

    fn alloc_local_obj<'a>(&'a mut self, obj: &Obj) -> PointerValue<'ctx> {
        if self.objs_ptr.contains_key(&obj.id) {
            panic!("already exists obj");
        }

        let builder = self.context.create_builder();

        let func = self.current_func.unwrap();

        let entry = func.get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        let ptr = match obj.obj_type.get_node() {
            TypeNode::Array(ref array_to, len) => {
                let asm_type = self.convert_llvm_basictype(array_to);
                builder.build_array_alloca(
                    asm_type,
                    self.context.i32_type().const_int(len as u64, false),
                    &obj.name,
                )
            }
            _ => {
                let asm_type = self.convert_llvm_basictype(&obj.obj_type);
                builder.build_alloca(asm_type, &obj.name)
            }
        };

        self.objs_ptr.insert(obj.id, ptr);
        ptr
    }

    fn get_local_obj(&self, obj: &Obj) -> PointerValue {
        if !self.objs_ptr.contains_key(&obj.id) {
            panic!("not found obj")
        }

        self.objs_ptr.get(&obj.id).unwrap().clone()
    }

    fn get_break_block(&self, stmt_id: usize) -> BasicBlock {
        if !self.break_block.contains_key(&stmt_id) {
            panic!("not found obj");
        }

        self.break_block.get(&stmt_id).unwrap().clone()
    }

    fn get_continue_block(&self, stmt_id: usize) -> BasicBlock {
        if !self.continue_block.contains_key(&stmt_id) {
            panic!("not found obj");
        }

        self.continue_block.get(&stmt_id).unwrap().clone()
    }

    fn codegen_func(&mut self, func: &ASTGlobal) {
        let ASTGlobal::Function(ref obj,ref args, ref stmts) = func else{panic!()};

        let main_fn_type = self
            .convert_llvm_anytype(&(*obj.borrow()).obj_type)
            .try_into()
            .unwrap();
        let main_fn = self
            .module
            .add_function(&*obj.borrow().name, main_fn_type, None);
        let frame_pointer_attribute = self.context.create_string_attribute("frame-pointer", "all");
        main_fn.add_attribute(AttributeLoc::Function, frame_pointer_attribute);

        let basic_block = self.context.append_basic_block(main_fn, "entry");

        self.current_func = Some(main_fn);
        self.objs_ptr.insert(
            (*obj).borrow().id,
            main_fn.as_global_value().as_pointer_value(),
        );
        self.builder.position_at_end(basic_block);
        for (i, arg) in main_fn.get_param_iter().enumerate() {
            let ptr = self.alloc_local_obj(&*args[i].borrow());
            self.builder.build_store(ptr, arg);
        }

        for stmt in stmts.iter() {
            self.codegen_stmt(stmt);
        }
        self.current_func = None;
    }

    fn codegen_global_variable(&mut self, var_decl: &ASTGlobal) {
        let ASTGlobal::Variable(ref obj) = var_decl else {panic!()};
        let llvm_type = self.convert_llvm_basictype(&(*obj.borrow()).obj_type);
        let global_obj = self.module.add_global(
            llvm_type,
            Some(AddressSpace::default()),
            &(*obj.borrow()).name,
        );

        global_obj.set_initializer(&llvm_type.const_zero());

        self.objs_ptr
            .insert((*obj).borrow().id, global_obj.as_pointer_value());
    }

    fn codegen_addr(&self, ast: &ASTExpr) -> PointerValue {
        match ast.get_node() {
            ASTExprNode::Var(obj) => self.get_local_obj(&*obj.borrow()),
            ASTExprNode::UnaryOp(unary_node) => match unary_node.kind {
                UnaryOpKind::Deref => self.codegen_expr(&unary_node.expr).into_pointer_value(),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }

    fn codegen_stmt(&mut self, ast: &ASTStmt) {
        match ast.get_node() {
            ASTStmtNode::Return(ref expr) => {
                let val = self.codegen_expr(expr);
                self.builder.build_return(Some(&val));
            }
            ASTStmtNode::Break(stmt_id) => {
                let break_block = self.get_break_block(stmt_id);
                self.builder.build_unconditional_branch(break_block);
            }
            ASTStmtNode::Continue(stmt_id) => {
                let continue_block = self.get_continue_block(stmt_id);
                self.builder.build_unconditional_branch(continue_block);
            }
            ASTStmtNode::Declaration(ref obj) => {
                self.alloc_local_obj(&*obj.borrow());
            }
            ASTStmtNode::ExprStmt(ref expr) => {
                self.codegen_expr(expr);
            }
            ASTStmtNode::Block(ref stmts) => {
                for stmt in stmts.iter() {
                    self.codegen_stmt(stmt);
                }
            }
            ASTStmtNode::If(ref cond, ref then_stmt, ref else_stmt) => {
                let func = self.current_func.unwrap();
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

                let then_bb = self.context.append_basic_block(func, "if_then");
                let else_bb = self.context.append_basic_block(func, "if_else");
                let after_bb = self.context.append_basic_block(func, "if_after");

                self.builder
                    .build_conditional_branch(cond, then_bb, else_bb);

                self.builder.position_at_end(then_bb);
                self.codegen_stmt(then_stmt);
                self.builder.build_unconditional_branch(after_bb);

                self.builder.position_at_end(else_bb);
                if else_stmt.is_some() {
                    let else_stmt = else_stmt.as_ref().unwrap();
                    self.codegen_stmt(else_stmt);
                }
                self.builder.build_unconditional_branch(after_bb);

                self.builder.position_at_end(after_bb);
            }
            ASTStmtNode::While(ref cond, ref stmt, stmt_id) => {
                let func = self.current_func.unwrap();
                let zero = self
                    .convert_llvm_basictype(&cond.expr_type)
                    .into_int_type()
                    .const_int(0, false);

                let cond_bb = self.context.append_basic_block(func, "cond");
                let loop_bb = self.context.append_basic_block(func, "loop");
                let after_bb = self.context.append_basic_block(func, "after");

                // push break_block
                self.break_block.insert(stmt_id, after_bb);
                self.continue_block.insert(stmt_id, cond_bb);

                self.builder.build_unconditional_branch(cond_bb);
                self.builder.position_at_end(cond_bb);
                let cond = self.codegen_expr(cond);
                let cond = self.builder.build_int_compare(
                    inkwell::IntPredicate::NE,
                    cond.into_int_value(),
                    zero,
                    "if_cond",
                );

                self.builder
                    .build_conditional_branch(cond, loop_bb, after_bb);

                self.builder.position_at_end(loop_bb);
                self.codegen_stmt(stmt);
                self.builder.build_unconditional_branch(cond_bb);

                self.builder.position_at_end(after_bb);
            }
            ASTStmtNode::DoWhile(ref cond, ref stmt, stmt_id) => {
                let func = self.current_func.unwrap();
                let zero = self
                    .convert_llvm_basictype(&cond.expr_type)
                    .into_int_type()
                    .const_int(0, false);

                let loop_bb = self.context.append_basic_block(func, "loop");
                let cond_bb = self.context.append_basic_block(func, "cond");
                let after_bb = self.context.append_basic_block(func, "after");

                self.break_block.insert(stmt_id, after_bb);
                self.continue_block.insert(stmt_id, cond_bb);

                self.builder.build_unconditional_branch(loop_bb);
                self.builder.position_at_end(loop_bb);
                self.codegen_stmt(stmt);

                self.builder.build_unconditional_branch(cond_bb);
                self.builder.position_at_end(cond_bb);
                let cond = self.codegen_expr(cond);
                let cond = self.builder.build_int_compare(
                    inkwell::IntPredicate::NE,
                    cond.into_int_value(),
                    zero,
                    "if_cond",
                );

                self.builder
                    .build_conditional_branch(cond, loop_bb, after_bb);

                self.builder.position_at_end(after_bb);
            }
            ASTStmtNode::For(ref start, ref cond, ref step, ref stmt, stmt_id) => {
                let func = self.current_func.unwrap();

                let cond_bb = self.context.append_basic_block(func, "cond");
                let step_bb = self.context.append_basic_block(func, "step");
                let loop_bb = self.context.append_basic_block(func, "loop");
                let after_bb = self.context.append_basic_block(func, "after");

                // push break_block
                self.break_block.insert(stmt_id, after_bb);
                self.continue_block.insert(stmt_id, step_bb);

                if start.is_some() {
                    let start = start.as_ref().unwrap();
                    self.codegen_expr(start);
                }
                self.builder.build_unconditional_branch(cond_bb);

                self.builder.position_at_end(cond_bb);
                if cond.is_some() {
                    let cond = cond.as_ref().unwrap();
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
                        .build_conditional_branch(cond, loop_bb, after_bb);
                } else {
                    self.builder.build_unconditional_branch(loop_bb);
                }

                self.builder.position_at_end(loop_bb);
                self.codegen_stmt(stmt);
                self.builder.build_unconditional_branch(step_bb);

                self.builder.position_at_end(step_bb);
                if step.is_some() {
                    self.codegen_expr(step.as_ref().unwrap());
                }
                self.builder.build_unconditional_branch(cond_bb);

                self.builder.position_at_end(after_bb);
            }
        }
    }

    fn codegen_expr(&self, ast: &ASTExpr) -> BasicValueEnum {
        match ast.get_node() {
            ASTExprNode::Conditional(ref cond, ref then_expr, ref else_expr) => {
                let func = self.current_func.unwrap();
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

                let then_bb = self.context.append_basic_block(func, "if_then");
                let else_bb = self.context.append_basic_block(func, "if_else");
                let after_bb = self.context.append_basic_block(func, "if_after");

                self.builder
                    .build_conditional_branch(cond, then_bb, else_bb);

                self.builder.position_at_end(then_bb);
                let then_val = self.codegen_expr(then_expr);
                self.builder.build_unconditional_branch(after_bb);

                self.builder.position_at_end(else_bb);
                let else_val = self.codegen_expr(else_expr);
                self.builder.build_unconditional_branch(after_bb);

                self.builder.position_at_end(after_bb);

                let llvm_type = self.convert_llvm_basictype(&ast.expr_type);

                let phi = self.builder.build_phi(llvm_type, "iftmp");

                phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

                phi.as_basic_value()
            }
            ASTExprNode::Assign(ref assign_node) => {
                self.codegen_assign(assign_node, &ast.expr_type)
            }
            ASTExprNode::BinaryOp(ref binary_node) => {
                self.codegen_binary_op(binary_node, &ast.expr_type)
            }
            ASTExprNode::UnaryOp(ref unary_node) => {
                self.codegen_unary_op(unary_node, &ast.expr_type)
            }
            ASTExprNode::Cast(ref cast_to, ref expr) => {
                let int_value = self.codegen_expr(expr);

                if expr.expr_type.is_int_type() && cast_to.is_int_type() {
                    let llvm_type = self.convert_llvm_basictype(cast_to).into_int_type();
                    self.builder
                        .build_int_cast(int_value.into_int_value(), llvm_type, "int cast")
                        .into()
                } else {
                    int_value
                }
            }
            ASTExprNode::FuncCall(ref func_expr, ref args) => {
                let func_ptr = self.codegen_expr(func_expr).into_pointer_value();
                let arg_val = args
                    .iter()
                    .map(|val| self.codegen_expr(val).into())
                    .collect::<Vec<BasicMetadataValueEnum>>();
                let fn_type = self
                    .convert_llvm_anytype(&func_expr.expr_type)
                    .into_function_type();
                self.builder
                    .build_indirect_call(fn_type, func_ptr, &arg_val, "func_call")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
            }
            ASTExprNode::PostIncrement(ref expr) => {
                let llvm_type = self.convert_llvm_basictype(&ast.expr_type);
                let ptr = self.codegen_addr(expr);
                let val = self.builder.build_load(llvm_type, ptr, "val");

                let add = self.builder.build_int_add(
                    val.into_int_value(),
                    llvm_type.into_int_type().const_int(1, false),
                    "add",
                );
                self.builder.build_store(ptr, add);

                val
            }
            ASTExprNode::PostDecrement(ref expr) => {
                let llvm_type = self.convert_llvm_basictype(&ast.expr_type);
                let ptr = self.codegen_addr(expr);
                let val = self.builder.build_load(llvm_type, ptr, "val");

                let sub = self.builder.build_int_sub(
                    val.into_int_value(),
                    llvm_type.into_int_type().const_int(1, false),
                    "sub",
                );
                self.builder.build_store(ptr, sub);

                val
            }
            ASTExprNode::Number(num) => BasicValueEnum::IntValue(
                self.convert_llvm_basictype(&ast.expr_type)
                    .into_int_type()
                    .const_int(num as u64, false),
            ),
            ASTExprNode::StrLiteral(ref text) => unsafe {
                self.builder
                    .build_global_string(text, "str literal")
                    .as_pointer_value()
                    .into()
            },
            ASTExprNode::Var(ref obj) => {
                let ptr = self.codegen_addr(ast);
                let obj_type = &(*obj).borrow().obj_type;
                if obj_type.is_function_type() || obj_type.is_array_type() {
                    BasicValueEnum::PointerValue(ptr)
                } else {
                    let llvm_type = self.convert_llvm_basictype(&obj_type);
                    self.builder.build_load(llvm_type, ptr, "var")
                }
            }
        }
    }

    fn codegen_assign(&self, assign_node: &AssignNode, _expr_type: &Type) -> BasicValueEnum {
        match assign_node.kind {
            AssignKind::Assign => {
                let lhs_ptr = self.codegen_addr(&assign_node.lhs);
                let rhs = self.codegen_expr(&assign_node.rhs);

                self.builder.build_store(lhs_ptr, rhs);
                rhs
            }
            AssignKind::LeftShiftAssign => {
                let lhs_ptr = self.codegen_addr(&assign_node.lhs);

                let lhs_llvm_type = self.convert_llvm_basictype(&assign_node.lhs.expr_type);
                let lhs = self.builder.build_load(lhs_llvm_type, lhs_ptr, "lhs val");
                let rhs = self.codegen_expr(&assign_node.rhs);

                let lshift = self.builder.build_left_shift(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "add",
                );

                self.builder.build_store(lhs_ptr, lshift);
                lshift.into()
            }
            AssignKind::RightShiftAssign => {
                let lhs_ptr = self.codegen_addr(&assign_node.lhs);

                let lhs_llvm_type = self.convert_llvm_basictype(&assign_node.lhs.expr_type);
                let lhs = self.builder.build_load(lhs_llvm_type, lhs_ptr, "lhs val");
                let rhs = self.codegen_expr(&assign_node.rhs);

                let rshift = self.builder.build_right_shift(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    false,
                    "add",
                );

                self.builder.build_store(lhs_ptr, rshift);
                rshift.into()
            }
            AssignKind::OrAssign => {
                let lhs_ptr = self.codegen_addr(&assign_node.lhs);

                let lhs_llvm_type = self.convert_llvm_basictype(&assign_node.lhs.expr_type);
                let lhs = self.builder.build_load(lhs_llvm_type, lhs_ptr, "lhs val");
                let rhs = self.codegen_expr(&assign_node.rhs);

                let or = self
                    .builder
                    .build_or(lhs.into_int_value(), rhs.into_int_value(), "add");

                self.builder.build_store(lhs_ptr, or);
                or.into()
            }
            AssignKind::XorAssign => {
                let lhs_ptr = self.codegen_addr(&assign_node.lhs);

                let lhs_llvm_type = self.convert_llvm_basictype(&assign_node.lhs.expr_type);
                let lhs = self.builder.build_load(lhs_llvm_type, lhs_ptr, "lhs val");
                let rhs = self.codegen_expr(&assign_node.rhs);

                let xor = self
                    .builder
                    .build_xor(lhs.into_int_value(), rhs.into_int_value(), "add");

                self.builder.build_store(lhs_ptr, xor);
                xor.into()
            }
            AssignKind::AndAssign => {
                let lhs_ptr = self.codegen_addr(&assign_node.lhs);

                let lhs_llvm_type = self.convert_llvm_basictype(&assign_node.lhs.expr_type);
                let lhs = self.builder.build_load(lhs_llvm_type, lhs_ptr, "lhs val");
                let rhs = self.codegen_expr(&assign_node.rhs);

                let and = self
                    .builder
                    .build_and(lhs.into_int_value(), rhs.into_int_value(), "add");

                self.builder.build_store(lhs_ptr, and);
                and.into()
            }
            AssignKind::AddAssign => {
                let lhs_ptr = self.codegen_addr(&assign_node.lhs);

                let lhs_llvm_type = self.convert_llvm_basictype(&assign_node.lhs.expr_type);
                let lhs = self.builder.build_load(lhs_llvm_type, lhs_ptr, "lhs val");
                let rhs = self.codegen_expr(&assign_node.rhs);

                let add =
                    self.builder
                        .build_int_add(lhs.into_int_value(), rhs.into_int_value(), "add");

                self.builder.build_store(lhs_ptr, add);
                add.into()
            }
            AssignKind::SubAssign => {
                let lhs_ptr = self.codegen_addr(&assign_node.lhs);

                let lhs_llvm_type = self.convert_llvm_basictype(&assign_node.lhs.expr_type);
                let lhs = self.builder.build_load(lhs_llvm_type, lhs_ptr, "lhs val");
                let rhs = self.codegen_expr(&assign_node.rhs);

                let sub =
                    self.builder
                        .build_int_sub(lhs.into_int_value(), rhs.into_int_value(), "add");

                self.builder.build_store(lhs_ptr, sub);
                sub.into()
            }
            AssignKind::MulAssign => {
                let lhs_ptr = self.codegen_addr(&assign_node.lhs);

                let lhs_llvm_type = self.convert_llvm_basictype(&assign_node.lhs.expr_type);
                let lhs = self.builder.build_load(lhs_llvm_type, lhs_ptr, "lhs val");
                let rhs = self.codegen_expr(&assign_node.rhs);

                let mul =
                    self.builder
                        .build_int_mul(lhs.into_int_value(), rhs.into_int_value(), "add");

                self.builder.build_store(lhs_ptr, mul);
                mul.into()
            }
            AssignKind::DivAssign => {
                let lhs_ptr = self.codegen_addr(&assign_node.lhs);

                let lhs_llvm_type = self.convert_llvm_basictype(&assign_node.lhs.expr_type);
                let lhs = self.builder.build_load(lhs_llvm_type, lhs_ptr, "lhs val");
                let rhs = self.codegen_expr(&assign_node.rhs);

                let div = self.builder.build_int_signed_div(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "add",
                );

                self.builder.build_store(lhs_ptr, div);
                div.into()
            }
            AssignKind::ModAssign => {
                let lhs_ptr = self.codegen_addr(&assign_node.lhs);

                let lhs_llvm_type = self.convert_llvm_basictype(&assign_node.lhs.expr_type);
                let lhs = self.builder.build_load(lhs_llvm_type, lhs_ptr, "lhs val");
                let rhs = self.codegen_expr(&assign_node.rhs);

                let mod_val = self.builder.build_int_signed_rem(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "add",
                );

                self.builder.build_store(lhs_ptr, mod_val);
                mod_val.into()
            }
        }
    }

    fn codegen_binary_op(&self, binary_node: &BinaryOpNode, expr_type: &Type) -> BasicValueEnum {
        match binary_node.kind {
            BinaryOpKind::Comma => {
                self.codegen_expr(&binary_node.lhs);
                self.codegen_expr(&binary_node.rhs)
            }
            BinaryOpKind::Add => {
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
                    unsafe {
                        self.builder
                            .build_gep(
                                self.convert_llvm_basictype(lhs_type),
                                lhs.into_pointer_value(),
                                &[rhs.into_int_value()],
                                "ptr_add",
                            )
                            .into()
                    }
                } else if lhs_type.is_int_type() && rhs_type.is_int_type() {
                    unsafe {
                        self.builder
                            .build_gep(
                                self.convert_llvm_basictype(rhs_type),
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
            BinaryOpKind::Sub => {
                let lhs = self.codegen_expr(&binary_node.lhs);
                let rhs = self.codegen_expr(&binary_node.rhs);
                BasicValueEnum::IntValue(self.builder.build_int_sub(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "sub node",
                ))
            }
            BinaryOpKind::Mul => {
                let lhs = self.codegen_expr(&binary_node.lhs);
                let rhs = self.codegen_expr(&binary_node.rhs);
                BasicValueEnum::IntValue(self.builder.build_int_mul(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "mul node",
                ))
            }
            BinaryOpKind::Div => {
                let lhs = self.codegen_expr(&binary_node.lhs);
                let rhs = self.codegen_expr(&binary_node.rhs);
                BasicValueEnum::IntValue(self.builder.build_int_signed_div(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "div node",
                ))
            }
            BinaryOpKind::Mod => {
                let lhs = self.codegen_expr(&binary_node.lhs);
                let rhs = self.codegen_expr(&binary_node.rhs);
                self.builder
                    .build_int_signed_rem(lhs.into_int_value(), rhs.into_int_value(), "mod node")
                    .into()
            }
            BinaryOpKind::BitOr => {
                let lhs = self.codegen_expr(&binary_node.lhs);
                let rhs = self.codegen_expr(&binary_node.rhs);
                self.builder
                    .build_or(lhs.into_int_value(), rhs.into_int_value(), "or node")
                    .into()
            }
            BinaryOpKind::BitXor => {
                let lhs = self.codegen_expr(&binary_node.lhs);
                let rhs = self.codegen_expr(&binary_node.rhs);
                self.builder
                    .build_xor(lhs.into_int_value(), rhs.into_int_value(), "or node")
                    .into()
            }
            BinaryOpKind::BitAnd => {
                let lhs = self.codegen_expr(&binary_node.lhs);
                let rhs = self.codegen_expr(&binary_node.rhs);
                self.builder
                    .build_and(lhs.into_int_value(), rhs.into_int_value(), "and node")
                    .into()
            }
            BinaryOpKind::LogicalOr => {
                let func = self.current_func.unwrap();

                let lhs_val = self.codegen_expr(&binary_node.lhs);
                let lhs_zero = self
                    .convert_llvm_basictype(&binary_node.lhs.expr_type)
                    .into_int_type()
                    .const_int(0, false);
                let lhs_cond = self.builder.build_int_compare(
                    inkwell::IntPredicate::NE,
                    lhs_val.into_int_value(),
                    lhs_zero,
                    "if_cond",
                );

                let then_bb = self.context.append_basic_block(func, "logical or then");
                let rhs_bb = self.context.append_basic_block(func, "logical or rhs");

                self.builder
                    .build_conditional_branch(lhs_cond, then_bb, rhs_bb);

                let else_bb = self.context.append_basic_block(func, "logical or else");

                self.builder.position_at_end(rhs_bb);
                let rhs_val = self.codegen_expr(&binary_node.rhs);
                let rhs_zero = self
                    .convert_llvm_basictype(&binary_node.rhs.expr_type)
                    .into_int_type()
                    .const_int(0, false);
                let rhs_cond = self.builder.build_int_compare(
                    inkwell::IntPredicate::NE,
                    rhs_val.into_int_value(),
                    rhs_zero,
                    "if_cond",
                );

                self.builder
                    .build_conditional_branch(rhs_cond, then_bb, else_bb);

                let after_bb = self.context.append_basic_block(func, "logical or after");

                self.builder.position_at_end(then_bb);
                self.builder.build_unconditional_branch(after_bb);

                self.builder.position_at_end(else_bb);
                self.builder.build_unconditional_branch(after_bb);

                self.builder.position_at_end(after_bb);

                let llvm_type = self.convert_llvm_basictype(expr_type);

                let phi = self.builder.build_phi(llvm_type, "iftmp");

                phi.add_incoming(&[
                    (&llvm_type.into_int_type().const_int(1, false), then_bb),
                    (&llvm_type.into_int_type().const_int(0, false), else_bb),
                ]);

                phi.as_basic_value()
            }
            BinaryOpKind::LogicalAnd => {
                let func = self.current_func.unwrap();

                let lhs_val = self.codegen_expr(&binary_node.lhs);
                let lhs_zero = self
                    .convert_llvm_basictype(&binary_node.lhs.expr_type)
                    .into_int_type()
                    .const_int(0, false);
                let lhs_cond = self.builder.build_int_compare(
                    inkwell::IntPredicate::NE,
                    lhs_val.into_int_value(),
                    lhs_zero,
                    "if_cond",
                );

                let rhs_bb = self.context.append_basic_block(func, "logical and rhs");
                let else_bb = self.context.append_basic_block(func, "logical and else");

                self.builder
                    .build_conditional_branch(lhs_cond, rhs_bb, else_bb);

                let then_bb = self.context.append_basic_block(func, "logical and then");

                self.builder.position_at_end(rhs_bb);
                let rhs_val = self.codegen_expr(&binary_node.rhs);
                let rhs_zero = self
                    .convert_llvm_basictype(&binary_node.rhs.expr_type)
                    .into_int_type()
                    .const_int(0, false);
                let rhs_cond = self.builder.build_int_compare(
                    inkwell::IntPredicate::NE,
                    rhs_val.into_int_value(),
                    rhs_zero,
                    "if_cond",
                );

                self.builder
                    .build_conditional_branch(rhs_cond, then_bb, else_bb);

                let after_bb = self.context.append_basic_block(func, "logical and after");

                self.builder.position_at_end(then_bb);
                self.builder.build_unconditional_branch(after_bb);

                self.builder.position_at_end(else_bb);
                self.builder.build_unconditional_branch(after_bb);

                self.builder.position_at_end(after_bb);

                let llvm_type = self.convert_llvm_basictype(expr_type);

                let phi = self.builder.build_phi(llvm_type, "iftmp");

                phi.add_incoming(&[
                    (&llvm_type.into_int_type().const_int(1, false), then_bb),
                    (&llvm_type.into_int_type().const_int(0, false), else_bb),
                ]);

                phi.as_basic_value()
            }
            BinaryOpKind::Equal => {
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
            BinaryOpKind::NotEqual => {
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
            BinaryOpKind::Less => {
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
            BinaryOpKind::LessEqual => {
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
            BinaryOpKind::Greater => {
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
            BinaryOpKind::GreaterEqual => {
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
            BinaryOpKind::LeftShift => {
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
            BinaryOpKind::RightShift => {
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

    fn codegen_unary_op(&self, unary_node: &UnaryOpNode, expr_type: &Type) -> BasicValueEnum {
        match unary_node.kind {
            UnaryOpKind::Sizeof => {
                let size_val = self
                    .convert_llvm_basictype(expr_type)
                    .size_of()
                    .unwrap()
                    .into();
                BasicValueEnum::IntValue(self.builder.build_int_cast_sign_flag(
                    size_val,
                    self.context.i32_type(),
                    false,
                    "cast to i32",
                ))
            }
            UnaryOpKind::Alignof => {
                let llvm_type = self.convert_llvm_basictype(expr_type);
                let align_val = match llvm_type {
                    BasicTypeEnum::IntType(ty) => ty.get_alignment().into(),
                    BasicTypeEnum::PointerType(ty) => ty.get_alignment().into(),
                    _ => unimplemented!(),
                };

                BasicValueEnum::IntValue(self.builder.build_int_cast_sign_flag(
                    align_val,
                    self.context.i32_type(),
                    false,
                    "cast to i32",
                ))
            }
            UnaryOpKind::Plus => self.codegen_expr(&unary_node.expr),
            UnaryOpKind::Minus => {
                let expr = self.codegen_expr(&unary_node.expr);
                BasicValueEnum::IntValue(self.builder.build_int_neg(expr.into_int_value(), "neg"))
            }
            UnaryOpKind::Addr => BasicValueEnum::PointerValue(self.codegen_addr(&unary_node.expr)),
            UnaryOpKind::Deref => {
                let llvm_type = self.convert_llvm_basictype(expr_type);
                let ptr = self.codegen_expr(&unary_node.expr).into_pointer_value();
                self.builder.build_load(llvm_type, ptr, "var")
            }
            UnaryOpKind::LogicalNot => {
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
            UnaryOpKind::BitNot => {
                let expr = self.codegen_expr(&unary_node.expr);
                self.builder
                    .build_not(expr.into_int_value(), "bit not")
                    .into()
            }
        }
    }
}
