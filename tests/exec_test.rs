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

test_function!(number_1, "int main(){return 0;}", 0);
test_function!(number_2, "int main(){return 10;}", 10);
test_function!(number_3, "int main(){return 31;}", 31);

test_function!(add_expr_1, "int main(){return 1+1;}", 2);
test_function!(add_expr_2, "int main(){return 1+2+3+4;}", 10);
test_function!(add_expr_3, "int main(){return 10+9+8;}", 27);

test_function!(sub_expr_1, "int main(){return 5-4;}", 1);
test_function!(sub_expr_2, "int main(){return 10-4-3-2-1;}", 0);
test_function!(sub_expr_3, "int main(){return 9-3;}", 6);

test_function!(add_sub_expr_1, "int main(){return 7-4+3;}", 6);
test_function!(add_sub_expr_2, "int main(){return 10-9+8-7;}", 2);
test_function!(add_sub_expr_3, "int main(){return 23-10-8+5;}", 10);

test_function!(mul_expr_1, "int main(){return 2*5;}", 10);
test_function!(mul_expr_2, "int main(){return 1*2*3;}", 6);

test_function!(div_expr_1, "int main(){return 4/2;}", 2);
test_function!(div_expr_2, "int main(){return 24/2/3;}", 4);
test_function!(div_expr_3, "int main(){return 5/2;}", 2);

test_function!(arith_expr_1, "int main(){return 5/2+3*4-42/3;}", 0);
test_function!(arith_expr_2, "int main(){return 1+2-5/9+10*0;}", 3);

test_function!(parenthesis_1, "int main(){return (1+2)*3;}", 9);
test_function!(parenthesis_2, "int main(){return (1+2*4)/(9-6);}", 3);

test_function!(plus_1, "int main(){return +1*3;}", 3);
test_function!(plus_2, "int main(){return +2*+5+-2*3;}", 4);

test_function!(equal_1, "int main(){return 1==1;}", 1);
test_function!(equal_2, "int main(){return 1==0;}", 0);

test_function!(not_equal_1, "int main(){return 1!=1;}", 0);
test_function!(not_equal_2, "int main(){return 1!=0;}", 1);

test_function!(smaller_1, "int main(){return 32<43;}", 1);
test_function!(smaller_2, "int main(){return 32<10;}", 0);
test_function!(smaller_3, "int main(){return 32<=43;}", 1);
test_function!(smaller_4, "int main(){return 32<=10;}", 0);
test_function!(smaller_5, "int main(){return 32<=32;}", 1);

test_function!(greater_1, "int main(){return 32>43;}", 0);
test_function!(greater_2, "int main(){return 32>10;}", 1);
test_function!(greater_3, "int main(){return 32>=43;}", 0);
test_function!(greater_4, "int main(){return 32>=10;}", 1);
test_function!(greater_5, "int main(){return 32>=32;}", 1);

test_function!(whitespace_1, "int main(){return 1            + 2   ;}", 3);

test_function!(declaration_1, "int main(){int a;return 1+1;}", 2);
test_function!(declaration_2, "int main(){int a;int b;return 1+1;}", 2);

test_function!(assign_1, "int main(){int a;return a=10;}", 10);
test_function!(
    assign_2,
    "int main(){int a;int b;return (a=10) + (b=20);}",
    30
);

test_function!(var_1, "int main(){int a;a=10;return a;}", 10);
test_function!(var_2, "int main(){int a;a=10;return a+20;}", 30);
test_function!(var_3, "int main(){int a;int b;a=10;b=20;return a+b;}", 30);
test_function!(var_4, "int main(){int a;int b;a=b=10;return a+b;}", 20);

test_function!(if_1, "int main(){if(10)1+1;return 10;}", 10);
test_function!(if_2, "int main(){int a;a = 10;if(1)a = 20;return a;}", 20);
test_function!(if_3, "int main(){int a;a = 10;if(0)a = 20;return a;}", 10);
test_function!(ifelse_1, "int main(){if(1)1+1;else 1+2;return 10;}", 10);
test_function!(
    ifelse_2,
    "int main(){int a;if(1)a=10;else a=20;return a;}",
    10
);
test_function!(
    ifelse_3,
    "int main(){int a;if(0)a=10;else a=20;return a;}",
    20
);

test_function!(while_1, "int main(){while(0)1;return 10;}", 10);
test_function!(
    while_2,
    "int main(){int a;a = 0;while(a<10)a = a+1;return a;}",
    10
);
test_function!(
    while_3,
    "int main(){int a;a = 0;while(a<10)a = a+1;while(a<20)a = a+1;return a;}",
    20
);

test_function!(
    for_1,
    "int main(){int a;for(a=0;a<10;a=a+1)1;return a;}",
    10
);
test_function!(
    for_2,
    "int main(){int a;int b;b=0;for(a=0;a<10;a=a+1)b = b+2;return b;}",
    20
);

test_function!(block_1, "int main(){{int a;a = 10;} return 10;}", 10);
test_function!(
    block_2,
    "int main(){int a;int b; if(1){a=10;b = 5;} return a+b;}",
    15
);

test_function!(func_1, "int empty(){return 1;} int main(){return 2;}", 2);
test_function!(
    func_2,
    "int ten(){return 10;} int main(){return ten();}",
    10
);
test_function!(func_3,"int one(){return 1;} int two(){return 2;} int three(){return 3;} int main(){return one() + two() * three();}",7);

test_function!(ptr_1, "int main(){int *a;return 10;}", 10);
test_function!(ptr_2, "int main(){int *a;int b;a = &b;return 33;}", 33);
test_function!(
    ptr_3,
    "int main(){int *a;int b;b = 10;a = &b;*a = 55;return b;}",
    55
);

test_function!(
    ptr_4,
    "int main(){int **a;int *b;int c;c = 0;a = &b;*a = &c;**a = 34;return c;}",
    34
);

test_function!(
    ptr_5,
    "int main(){int *a;int b;a = &b;a = a+1;return 10;}",
    10
);

test_function!(sizeof_1, "int main(){return sizeof(1);}", 4);
test_function!(
    sizeof_2,
    "int main(){int a;a=10;return sizeof(a=20) + a;}",
    14
);

test_function!(array_1, "int main(){int a[10];return 10;}", 10);
test_function!(
    array_2,
    "int main(){int a[10];a[5] = 33;return a[5]+7;}",
    40
);

test_function!(global_var_1, "int a;int main(){return 10;}", 10);
test_function!(global_var_2, "int a;int main(){a=37;return a+3;}", 40);
test_function!(
    global_var_3,
    "int *a;int b;int main(){b=0;a = &b;*a = 25;return b+5;}",
    30
);

test_function!(shadowing_1, "int a;int main(){int a;return 33;}", 33);
test_function!(
    shadowing_2,
    "int a;int main(){a=0;int a; a=10;return a;}",
    10
);

test_function!(char_1, "int main(){char a;return 10;}", 10);
test_function!(char_2, "int main(){char a;a = 10;return a;}", 10);
test_function!(char_3, "char a;int main(){a=10;return a;}", 10);
test_function!(
    char_4,
    "int main(){char *a;char b;b=0;a = &b;*a = 33;return b;}",
    33
);

test_function!(
    str_literal_1,
    "int main(){char *str;str = \"012345\"; return 10;}",
    10
);

test_function!(
    str_literal_2,
    "int main(){char *str;str = \"012345\"; return str[0];}",
    0x30
);

test_function!(ex_001, "int main(){return 123;}", 123);
test_function!(ex_002, "int main(){return (123);}", 123);
test_function!(ex_003, "int main(){return ((((123))));}", 123);
test_function!(ex_004, "int main(){return 123+51;}", 174);
test_function!(ex_005, "int main(){return 123+56-5;}", 174);
test_function!(ex_006, "int main(){return 175-(4-3);}", 174);
test_function!(ex_007, "int main(){return 181-4-3;}", 174);
test_function!(ex_008, "int main(){return 0x29*3+7*8-5*1;}", 174);
test_function!(ex_009, "int main(){return 6*(3+7)-5*1;}", 55);
test_function!(ex_010, "int main(){return 43,6*(3+7)-5*1;}", 55);

test_function!(ex_011, "int main(){return 43,6*(3+(4|3))-(5|1)*1;}", 55);
test_function!(
    ex_012,
    "int main(){return 043,41*3+07*010-0Xa/(010%(1+2));}",
    174
);
test_function!(
    ex_013,
    "int main(){return 7*5,(12,41*3)+7*16/(9,2)-10/(8%3);}",
    174
);
test_function!(ex_013_1, "int main(){return 173+ (1<2);}", 174);
test_function!(ex_014,"int main(){return 7*5 	,	(0xC,(41   )*(4-(011>8)))+7*(((1+2)>=3)<<4)/(9,(4>>(10<=10))+(3<3))-10/(	  ( 	1  <<3)	%3);}",174);
test_function!(
    ex_015,
    "int main(){return 35,	((	41|	(8   !=     15))*  ((3==3)+2))+((5|2)*(9&10))   -   (10/(8%3));}",
    174
);
test_function!(
    ex_016,
    "int main(){return 043,41*3+07*010-0Xa/(010%(!!1+2));}",
    174
);
test_function!(ex_017,"int main(){return 7*5 	,	(0xC,(41   )*(4-(011>8)))+7*(((1-~1)>=3)<<4)/(9,(4>>(10<=10))+(3<3))-10/(	  ( 	!0  <<3)	%3);}",174);
test_function!(ex_018, "int main(){return +174;}", 174);
test_function!(ex_019, "int main(){return -(1-175);}", 174);
test_function!(
    ex_020,
    "int main(){23; 45+37; ((12-1)*75); return -(1-175);}",
    174
);
