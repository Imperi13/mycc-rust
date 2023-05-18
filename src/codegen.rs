use crate::ast::ASTExpr;
use crate::ast::ASTExprNode;
use crate::ast::ASTGlobal;
use crate::ast::ASTGlobalNode;
use crate::ast::ASTStmt;
use crate::ast::ASTStmtNode;
use crate::ast::BinaryOpKind;
use crate::ast::BinaryOpNode;
use crate::ast::UnaryOpKind;
use crate::ast::UnaryOpNode;
use crate::parse::Obj;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::IntType;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use inkwell::values::PointerValue;
use std::collections::HashMap;
use std::path::Path;

struct BuiltinType<'a> {
    pub int_type: IntType<'a>,
}

struct CodegenArena<'a> {
    context: &'a Context,
    module: Module<'a>,
    builder: Builder<'a>,
    types: BuiltinType<'a>,

    current_func: Option<FunctionValue<'a>>,
    objs_ptr: HashMap<usize, PointerValue<'a>>,
}

impl CodegenArena<'_> {
    pub fn print_to_file(&self, filepath: &str) {
        let path = Path::new(filepath);
        self.module.print_to_file(path).unwrap();
    }

    pub fn alloc_local_obj<'a>(&'a mut self, obj: &Obj) -> PointerValue<'a> {
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

        let ptr = builder.build_alloca(self.types.int_type, &obj.name);

        self.objs_ptr.insert(obj.id, ptr);
        ptr
    }

    pub fn get_local_obj(&self, obj: &Obj) -> PointerValue {
        if !self.objs_ptr.contains_key(&obj.id) {
            panic!("not found obj")
        }

        self.objs_ptr.get(&obj.id).unwrap().clone()
    }

    pub fn codegen_func(&mut self, func: ASTGlobal) {
        let ASTGlobalNode::Function(ref obj, ref stmts) = func.get_node();

        let main_fn_type = self.types.int_type.fn_type(&[], false);
        let main_fn = self
            .module
            .add_function(&*obj.borrow().name, main_fn_type, None);
        let basic_block = self.context.append_basic_block(main_fn, "entry");

        self.current_func = Some(main_fn);
        self.objs_ptr.insert(
            (*obj).borrow().id,
            main_fn.as_global_value().as_pointer_value(),
        );
        self.builder.position_at_end(basic_block);
        for stmt in stmts.iter() {
            self.codegen_stmt(stmt.clone());
        }
        self.current_func = None;
    }

    pub fn codegen_addr(&self, ast: ASTExpr) -> PointerValue {
        match ast.get_node() {
            ASTExprNode::Var(obj) => self.get_local_obj(&*obj.borrow()),
            _ => panic!(),
        }
    }

    pub fn codegen_stmt(&mut self, ast: ASTStmt) {
        match ast.get_node() {
            ASTStmtNode::Return(ref expr) => {
                let val = self.codegen_expr(expr.clone());
                self.builder.build_return(Some(&val.into_int_value()));
            }
            ASTStmtNode::Declaration(ref obj) => {
                self.alloc_local_obj(&*obj.borrow());
            }
            ASTStmtNode::ExprStmt(ref expr) => {
                self.codegen_expr(expr.clone());
            }
            ASTStmtNode::Block(ref stmts) => {
                for stmt in stmts.iter() {
                    self.codegen_stmt(stmt.clone());
                }
            }
            ASTStmtNode::If(ref cond, ref if_stmt, ref else_stmt) => {
                let func = self.current_func.unwrap();
                let zero = self.types.int_type.const_int(0, false);

                let cond = self.codegen_expr(cond.clone());
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
                self.codegen_stmt(if_stmt.clone());
                self.builder.build_unconditional_branch(after_bb);

                self.builder.position_at_end(else_bb);
                if else_stmt.is_some() {
                    let else_stmt = else_stmt.clone().unwrap();
                    self.codegen_stmt(else_stmt.clone());
                }
                self.builder.build_unconditional_branch(after_bb);

                self.builder.position_at_end(after_bb);
            }
            ASTStmtNode::While(ref cond, ref stmt) => {
                let func = self.current_func.unwrap();
                let zero = self.types.int_type.const_int(0, false);

                let cond_bb = self.context.append_basic_block(func, "cond");
                let loop_bb = self.context.append_basic_block(func, "loop");
                let after_bb = self.context.append_basic_block(func, "after");

                self.builder.build_unconditional_branch(cond_bb);
                self.builder.position_at_end(cond_bb);
                let cond = self.codegen_expr(cond.clone());
                let cond = self.builder.build_int_compare(
                    inkwell::IntPredicate::NE,
                    cond.into_int_value(),
                    zero,
                    "if_cond",
                );

                self.builder
                    .build_conditional_branch(cond, loop_bb, after_bb);

                self.builder.position_at_end(loop_bb);
                self.codegen_stmt(stmt.clone());
                self.builder.build_unconditional_branch(cond_bb);

                self.builder.position_at_end(after_bb);
            }
            ASTStmtNode::For(ref start, ref cond, ref step, ref stmt) => {
                let func = self.current_func.unwrap();
                let zero = self.types.int_type.const_int(0, false);

                let cond_bb = self.context.append_basic_block(func, "cond");
                let loop_bb = self.context.append_basic_block(func, "loop");
                let after_bb = self.context.append_basic_block(func, "after");

                self.codegen_expr(start.clone());
                self.builder.build_unconditional_branch(cond_bb);

                self.builder.position_at_end(cond_bb);
                let cond = self.codegen_expr(cond.clone());
                let cond = self.builder.build_int_compare(
                    inkwell::IntPredicate::NE,
                    cond.into_int_value(),
                    zero,
                    "if_cond",
                );

                self.builder
                    .build_conditional_branch(cond, loop_bb, after_bb);

                self.builder.position_at_end(loop_bb);
                self.codegen_stmt(stmt.clone());
                self.codegen_expr(step.clone());
                self.builder.build_unconditional_branch(cond_bb);

                self.builder.position_at_end(after_bb);
            }
        }
    }

    pub fn codegen_expr(&self, ast: ASTExpr) -> BasicValueEnum {
        match ast.get_node() {
            ASTExprNode::BinaryOp(ref binary_node) => self.codegen_binary_op(binary_node),
            ASTExprNode::UnaryOp(ref unary_node) => self.codegen_unary_op(unary_node),
            ASTExprNode::FuncCall(ref func_expr) => {
                let func_ptr = self.codegen_expr(func_expr.clone()).into_pointer_value();
                let fn_type = self.types.int_type.fn_type(&[], false);
                self.builder
                    .build_indirect_call(fn_type, func_ptr, &[], "func_call")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
            }
            ASTExprNode::Number(num) => {
                BasicValueEnum::IntValue(self.types.int_type.const_int(num as u64, false))
            }
            ASTExprNode::Var(ref obj) => {
                let ptr = self.codegen_addr(ast.clone());
                if (*obj).borrow().obj_type.is_function_type() {
                    BasicValueEnum::PointerValue(ptr)
                } else {
                    self.builder.build_load(self.types.int_type, ptr, "var")
                }
            }
        }
    }

    pub fn codegen_binary_op(&self, binary_node: &BinaryOpNode) -> BasicValueEnum {
        match binary_node.kind {
            BinaryOpKind::Assign => {
                let lhs_ptr = self.codegen_addr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());

                self.builder.build_store(lhs_ptr, rhs);
                rhs
            }
            BinaryOpKind::Add => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                BasicValueEnum::IntValue(self.builder.build_int_add(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "add node",
                ))
            }
            BinaryOpKind::Sub => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                BasicValueEnum::IntValue(self.builder.build_int_sub(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "sub node",
                ))
            }
            BinaryOpKind::Mul => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                BasicValueEnum::IntValue(self.builder.build_int_mul(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "mul node",
                ))
            }
            BinaryOpKind::Div => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                BasicValueEnum::IntValue(self.builder.build_int_signed_div(
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "div node",
                ))
            }
            BinaryOpKind::Equal => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                let cmp = self.builder.build_int_compare(
                    inkwell::IntPredicate::EQ,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "equal node",
                );
                BasicValueEnum::IntValue(self.builder.build_int_cast_sign_flag(
                    cmp,
                    self.types.int_type,
                    false,
                    "cast to i64",
                ))
            }
            BinaryOpKind::NotEqual => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                let cmp = self.builder.build_int_compare(
                    inkwell::IntPredicate::NE,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "equal node",
                );

                BasicValueEnum::IntValue(self.builder.build_int_cast_sign_flag(
                    cmp,
                    self.types.int_type,
                    false,
                    "cast to i64",
                ))
            }
            BinaryOpKind::Less => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                let cmp = self.builder.build_int_compare(
                    inkwell::IntPredicate::SLT,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "equal node",
                );

                BasicValueEnum::IntValue(self.builder.build_int_cast_sign_flag(
                    cmp,
                    self.types.int_type,
                    false,
                    "cast to i64",
                ))
            }
            BinaryOpKind::LessEqual => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                let cmp = self.builder.build_int_compare(
                    inkwell::IntPredicate::SLE,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "equal node",
                );

                BasicValueEnum::IntValue(self.builder.build_int_cast_sign_flag(
                    cmp,
                    self.types.int_type,
                    false,
                    "cast to i64",
                ))
            }
            BinaryOpKind::Greater => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                let cmp = self.builder.build_int_compare(
                    inkwell::IntPredicate::SGT,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "equal node",
                );

                BasicValueEnum::IntValue(self.builder.build_int_cast_sign_flag(
                    cmp,
                    self.types.int_type,
                    false,
                    "cast to i64",
                ))
            }
            BinaryOpKind::GreaterEqual => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                let cmp = self.builder.build_int_compare(
                    inkwell::IntPredicate::SGE,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "equal node",
                );

                BasicValueEnum::IntValue(self.builder.build_int_cast_sign_flag(
                    cmp,
                    self.types.int_type,
                    false,
                    "cast to i64",
                ))
            }
        }
    }

    pub fn codegen_unary_op(&self, unary_node: &UnaryOpNode) -> BasicValueEnum {
        match unary_node.kind {
            UnaryOpKind::Plus => self.codegen_expr(unary_node.expr.clone()),
            UnaryOpKind::Minus => {
                let expr = self.codegen_expr(unary_node.expr.clone());
                BasicValueEnum::IntValue(self.builder.build_int_neg(expr.into_int_value(), "neg"))
            }
            UnaryOpKind::Addr => {
                BasicValueEnum::PointerValue(self.codegen_addr(unary_node.expr.clone()))
            }
            UnaryOpKind::Deref => {
                let ptr = self
                    .codegen_expr(unary_node.expr.clone())
                    .into_pointer_value();
                self.builder.build_load(self.types.int_type, ptr, "var")
            }
        }
    }
}

pub fn codegen_all(funcs: Vec<ASTGlobal>, output_path: &str) {
    let context = Context::create();
    let module = context.create_module("main");
    let builder = context.create_builder();
    let types = BuiltinType {
        int_type: context.i32_type(),
    };

    let mut arena = CodegenArena {
        context: &context,
        module,
        builder,
        types,
        current_func: None,
        objs_ptr: HashMap::new(),
    };

    for func in funcs.iter() {
        arena.codegen_func(func.clone());
    }

    arena.print_to_file(output_path);
}
