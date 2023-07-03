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

test_function!(
    constant_propagation_2,
    "int main(){int a;int b;a = 10+20;b = a; return b;}",
    30
);

test_function!(
    constant_propagation_3,
    "int main(){int a;int b;int c;b = 0;c=0;a = 10+20;if (1) b = a; else c = a;   return b+c;}",
    30
);

test_function!(constant_propagation_4,
"int main() {int a;int b;a = 10;switch (a) {case 10:b = 23;break;case 5:b = 10;break;default:b = 5;break;}return b;}"
,23);
