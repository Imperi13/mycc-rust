mod common;

common::test_function!(number_1, "0", 0);
common::test_function!(number_2, "10", 10);
common::test_function!(number_3, "31", 31);

common::test_function!(add_expr_1, "1+1", 2);
common::test_function!(add_expr_2, "1+2+3+4", 10);
common::test_function!(add_expr_3, "10+9+8", 27);

common::test_function!(sub_expr_1, "5-4", 1);
common::test_function!(sub_expr_2, "10-4-3-2-1", 0);
common::test_function!(sub_expr_3, "9-3", 6);

common::test_function!(add_sub_expr_1, "7-4+3", 6);
common::test_function!(add_sub_expr_2, "10-9+8-7", 2);
common::test_function!(add_sub_expr_3, "23-10-8+5", 10);
