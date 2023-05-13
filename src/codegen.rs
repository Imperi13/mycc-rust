use super::parse::ASTNode;
use super::parse::BinaryOpKind;
use super::parse::BinaryOpNode;
use super::parse::UnaryOpKind;
use super::parse::UnaryOpNode;
use super::parse::AST;
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

    pub fn codegen_ret(&self, ast: AST) {
        let i64_type = self.context.i64_type();
        let main_fn_type = i64_type.fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_fn_type, None);
        let basic_block = self.context.append_basic_block(main_fn, "entry");

        let val = self.codegen_expr(ast);

        self.builder.position_at_end(basic_block);
        self.builder.build_return(Some(&val));
    }

    pub fn codegen_expr(&self, ast: AST) -> IntValue {
        match *ast.head.borrow() {
            ASTNode::ASTBinaryOp(ref binary_node) => self.codegen_binary_op(binary_node),
            ASTNode::ASTUnaryOp(ref unary_node) => self.codegen_unary_op(unary_node),
            ASTNode::ASTNumber(num) => {
                let i64_type = self.context.i64_type();
                i64_type.const_int(num as u64, false)
            }
        }
    }

    pub fn codegen_binary_op(&self, binary_node: &BinaryOpNode) -> IntValue {
        match binary_node.kind {
            BinaryOpKind::BinaryOpAdd => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                self.builder.build_int_add(lhs, rhs, "add node")
            }
            BinaryOpKind::BinaryOpSub => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                self.builder.build_int_sub(lhs, rhs, "sub node")
            }
            BinaryOpKind::BinaryOpMul => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                self.builder.build_int_mul(lhs, rhs, "mul node")
            }
            BinaryOpKind::BinaryOpDiv => {
                let lhs = self.codegen_expr(binary_node.lhs.clone());
                let rhs = self.codegen_expr(binary_node.rhs.clone());
                self.builder.build_int_signed_div(lhs, rhs, "div node")
            }
            BinaryOpKind::BinaryOpEqual => {
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
            BinaryOpKind::BinaryOpNotEqual => {
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
            BinaryOpKind::BinaryOpSmaller => {
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
            BinaryOpKind::BinaryOpSmallerEqual => {
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
            BinaryOpKind::BinaryOpGreater => {
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
            BinaryOpKind::BinaryOpGreaterEqual => {
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
            UnaryOpKind::UnaryOpPlus => self.codegen_expr(unary_node.expr.clone()),
            UnaryOpKind::UnaryOpMinus => {
                let expr = self.codegen_expr(unary_node.expr.clone());
                self.builder.build_int_neg(expr, "neg")
            }
        }
    }
}

pub fn codegen_all(ast: AST, output_path: &str) {
    let context = Context::create();
    let module = context.create_module("main");
    let builder = context.create_builder();

    let arena = CodegenArena {
        context: &context,
        module,
        builder,
    };

    arena.codegen_ret(ast);

    arena.print_to_file(output_path);
}
