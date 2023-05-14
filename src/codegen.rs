use super::parse::ASTExpr;
use super::parse::ASTExprNode;
use super::parse::ASTStmt;
use super::parse::ASTStmtNode;
use super::parse::BinaryOpKind;
use super::parse::BinaryOpNode;
use super::parse::UnaryOpKind;
use super::parse::UnaryOpNode;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::IntValue;
use std::path::Path;

struct CodegenArena<'a> {
    context: &'a Context,
    module: Module<'a>,
    builder: Builder<'a>,
}

impl CodegenArena<'_> {
    pub fn print_to_file(&self, filepath: &str) {
        let path = Path::new(filepath);
        self.module.print_to_file(path).unwrap();
    }

    pub fn codegen_func(&self, ast: ASTStmt) {
        let i64_type = self.context.i64_type();
        let main_fn_type = i64_type.fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_fn_type, None);
        let basic_block = self.context.append_basic_block(main_fn, "entry");

        self.builder.position_at_end(basic_block);
        self.codegen_stmt(ast);
    }

    pub fn codegen_stmt(&self, ast: ASTStmt) {
        match *ast.head.borrow() {
            ASTStmtNode::Return(ref expr) => {
                let val = self.codegen_expr(expr.clone());
                self.builder.build_return(Some(&val));
            }
        }
    }

    pub fn codegen_expr(&self, ast: ASTExpr) -> IntValue {
        match *ast.head.borrow() {
            ASTExprNode::BinaryOp(ref binary_node) => self.codegen_binary_op(binary_node),
            ASTExprNode::UnaryOp(ref unary_node) => self.codegen_unary_op(unary_node),
            ASTExprNode::Number(num) => {
                let i64_type = self.context.i64_type();
                i64_type.const_int(num as u64, false)
            }
        }
    }

    pub fn codegen_binary_op(&self, binary_node: &BinaryOpNode) -> IntValue {
        match binary_node.kind {
            BinaryOpKind::Add => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                self.builder.build_int_add(lhs, rhs, "add node")
            }
            BinaryOpKind::Sub => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                self.builder.build_int_sub(lhs, rhs, "sub node")
            }
            BinaryOpKind::Mul => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                self.builder.build_int_mul(lhs, rhs, "mul node")
            }
            BinaryOpKind::Div => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                self.builder.build_int_signed_div(lhs, rhs, "div node")
            }
            BinaryOpKind::Equal => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                let cmp = self.builder.build_int_compare(
                    inkwell::IntPredicate::EQ,
                    lhs,
                    rhs,
                    "equal node",
                );
                self.builder.build_int_cast_sign_flag(
                    cmp,
                    self.context.i64_type(),
                    false,
                    "cast to i64",
                )
            }
            BinaryOpKind::NotEqual => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                let cmp = self.builder.build_int_compare(
                    inkwell::IntPredicate::NE,
                    lhs,
                    rhs,
                    "equal node",
                );

                self.builder.build_int_cast_sign_flag(
                    cmp,
                    self.context.i64_type(),
                    false,
                    "cast to i64",
                )
            }
            BinaryOpKind::Less => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                let cmp = self.builder.build_int_compare(
                    inkwell::IntPredicate::SLT,
                    lhs,
                    rhs,
                    "equal node",
                );

                self.builder.build_int_cast_sign_flag(
                    cmp,
                    self.context.i64_type(),
                    false,
                    "cast to i64",
                )
            }
            BinaryOpKind::LessEqual => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                let cmp = self.builder.build_int_compare(
                    inkwell::IntPredicate::SLE,
                    lhs,
                    rhs,
                    "equal node",
                );

                self.builder.build_int_cast_sign_flag(
                    cmp,
                    self.context.i64_type(),
                    false,
                    "cast to i64",
                )
            }
            BinaryOpKind::Greater => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                let cmp = self.builder.build_int_compare(
                    inkwell::IntPredicate::SGT,
                    lhs,
                    rhs,
                    "equal node",
                );

                self.builder.build_int_cast_sign_flag(
                    cmp,
                    self.context.i64_type(),
                    false,
                    "cast to i64",
                )
            }
            BinaryOpKind::GreaterEqual => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                let cmp = self.builder.build_int_compare(
                    inkwell::IntPredicate::SGE,
                    lhs,
                    rhs,
                    "equal node",
                );

                self.builder.build_int_cast_sign_flag(
                    cmp,
                    self.context.i64_type(),
                    false,
                    "cast to i64",
                )
            }
        }
    }

    pub fn codegen_unary_op(&self, unary_node: &UnaryOpNode) -> IntValue {
        match unary_node.kind {
            UnaryOpKind::Plus => self.codegen_expr(unary_node.expr.clone()),
            UnaryOpKind::Minus => {
                let expr = self.codegen_expr(unary_node.expr.clone());
                self.builder.build_int_neg(expr, "neg")
            }
        }
    }
}

pub fn codegen_all(ast: ASTStmt, output_path: &str) {
    let context = Context::create();
    let module = context.create_module("main");
    let builder = context.create_builder();

    let arena = CodegenArena {
        context: &context,
        module,
        builder,
    };

    arena.codegen_func(ast);

    arena.print_to_file(output_path);
}
