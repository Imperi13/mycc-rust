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

test_function!(mul_expr_1, "2*5", 10);
test_function!(mul_expr_2, "1*2*3", 6);

test_function!(div_expr_1, "4/2", 2);
test_function!(div_expr_2, "24/2/3", 4);
test_function!(div_expr_3, "5/2", 2);

test_function!(arith_expr_1, "5/2+3*4-42/3", 0);
test_function!(arith_expr_2, "1+2-5/9+10*0", 3);

test_function!(parenthesis_1, "(1+2)*3", 9);
test_function!(parenthesis_2, "(1+2*4)/(9-6)", 3);

test_function!(plus_1, "+1*3", 3);
test_function!(plus_2, "+2*+5+-2*3", 4);

test_function!(equal_1, "1==1", 1);
test_function!(equal_2, "1==0", 0);

test_function!(not_equal_1, "1!=1", 0);
test_function!(not_equal_2, "1!=0", 1);

test_function!(smaller_1, "32<43", 1);
test_function!(smaller_2, "32<10", 0);
test_function!(smaller_3, "32<=43", 1);
test_function!(smaller_4, "32<=10", 0);
test_function!(smaller_5, "32<=32", 1);

test_function!(greater_1, "32>43", 0);
test_function!(greater_2, "32>10", 1);
test_function!(greater_3, "32>=43", 0);
test_function!(greater_4, "32>=10", 1);
test_function!(greater_5, "32>=32", 1);

test_function!(whitespace_1,"1            + 2   ",3);
