mod common;

macro_rules! test_function {
    ($func_name:ident,$code:literal,$status:expr) => {
        #[test]
        fn $func_name() {
            println!("{}", $code);
            let filepath = format!("test_{}.ll", stringify!($func_name));
            common::compile_code($code, &filepath, 1);
            assert_eq!(common::exec_code(&filepath), $status);
            common::cleanup(&filepath).unwrap();
        }
    };
}

test_function!(constexpr_1, "int main(){return 10+20;}", 30);
test_function!(constexpr_2, "int main(){return 10*20;}", 200);
test_function!(constexpr_3, "int main(){return 45%7;}", 3);

test_function!(
    constant_propagation_1,
    "int main(){int a;a = 10+20; return a;}",
    30
);
