use super::parse::ASTNode;
use super::parse::BinaryOpKind;
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

        let val = self.codegen_binary_op(ast);

        self.builder.position_at_end(basic_block);
        self.builder.build_return(Some(&val));
    }

    pub fn codegen_binary_op(&self, ast: AST) -> IntValue {
        match *ast.head.borrow() {
            ASTNode::ASTBinaryOp(ref binary_node) => match binary_node.kind {
                BinaryOpKind::BinaryOpPlus => {
                    let lhs = self.codegen_binary_op(binary_node.lhs.clone());
                    let rhs = self.codegen_binary_op(binary_node.rhs.clone());
                    self.builder.build_int_add(lhs, rhs, "add node")
                }
                BinaryOpKind::BinaryOpMinus => {
                    let lhs = self.codegen_binary_op(binary_node.lhs.clone());
                    let rhs = self.codegen_binary_op(binary_node.rhs.clone());
                    self.builder.build_int_sub(lhs, rhs, "add node")
                }
                BinaryOpKind::BinaryOpMul => {
                    let lhs = self.codegen_binary_op(binary_node.lhs.clone());
                    let rhs = self.codegen_binary_op(binary_node.rhs.clone());
                    self.builder.build_int_mul(lhs, rhs, "add node")
                }
                BinaryOpKind::BinaryOpDiv => {
                    let lhs = self.codegen_binary_op(binary_node.lhs.clone());
                    let rhs = self.codegen_binary_op(binary_node.rhs.clone());
                    self.builder.build_int_signed_div(lhs, rhs, "add node")
                }
            },
            _ => self.codegen_primary(ast.clone()),
        }
    }

    pub fn codegen_primary(&self, ast: AST) -> IntValue {
        let i64_type = self.context.i64_type();

        if let ASTNode::ASTNumber(num) = *ast.head.borrow() {
            i64_type.const_int(num as u64, false)
        } else {
            panic!("not primary node")
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
