mod common;

macro_rules! test_function {
    ($func_name:ident,$code:literal,$status:expr) => {
        #[test]
        fn $func_name() {
            let filepath = format!("test_{}.ll", stringify!($func_name));
            common::compile($code, &filepath);
            assert_eq!(common::exec_code(&filepath), $status);
            common::cleanup(&filepath).unwrap();
        }
    };
}

test_function!(number_1, "0", 0);
test_function!(number_2, "10", 10);
test_function!(number_3, "31", 31);

test_function!(add_expr_1, "1+1", 2);
test_function!(add_expr_2, "1+2+3+4", 10);
test_function!(add_expr_3, "10+9+8", 27);

test_function!(sub_expr_1, "5-4", 1);
test_function!(sub_expr_2, "10-4-3-2-1", 0);
test_function!(sub_expr_3, "9-3", 6);

test_function!(add_sub_expr_1, "7-4+3", 6);
test_function!(add_sub_expr_2, "10-9+8-7", 2);
test_function!(add_sub_expr_3, "23-10-8+5", 10);
