mod common;

macro_rules! test_function {
    ($func_name:ident,$code:literal,$status:expr) => {
        #[test]
        fn $func_name() {
            println!("{}", $code);
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

test_function!(
    ex_021,
    "int main(){23; 45+37; return -(1-175); ((12-1)*75);}",
    174
);
test_function!(
    ex_022,
    "int main(){int a; int b; return (a = b = 9, a = 41*3, 55 - (b = 4) + a);}",
    174
);
test_function!(ex_023,"int main(){int a; int b; int c; int d; int _q432; a = b = c = 9; d = 5; a = 41*3; return (c, _q432 = 8, d = 11*5) - (b = 4) + a;}",174);
test_function!(ex_024, "int main(){return 175^1;}", 174);
test_function!(ex_025, "int main(){return 2 + (1? 100 + 72 : 17);}", 174);
test_function!(
    ex_026,
    "int main(){return (0? 234 : 2) + (1? 100 + 72 : 17);}",
    174
);

test_function!(ex_032, "int main(){3; {5; 7; 11; } return 175^1;}", 174);
test_function!(
    ex_034,
    "int add_(int x, int y){4; return x+y;} int main(){3; return add_(87,87);}",
    174
);
test_function!(
    ex_035,
    "int fib(int n){ return n < 2? n : fib(n - 1) + fib(n - 2); } int main(){3; return fib(10);}",
    55
);
test_function!(ex_036,"int tarai(int x,int y,int z){ return x <= y? y : tarai(tarai(x-1, y, z), tarai(y-1, z, x), tarai(z-1, x, y)); } int main(){return tarai(12,6,0);}", 12);
test_function!(ex_037, "int main() { return (3 && 2 && 5) + 173; }", 174);
test_function!(
    ex_038,
    "int main() { return (3 && 2) + !(3 && 0) + !(0 && 3)+ !(0 && 0) + 170; }",
    174
);
test_function!(ex_039, "int main() { return (3 || 2 || 5) + 173; }", 174);
test_function!(
    ex_040,
    "int main() { return (3 || 2) + (3 || 0) + (0 || 3)+ !(0 || 0) + 170; }",
    174
);

test_function!(
    ex_041,
    "int main() {int a; a = 3; a += 5;  return a + 166; }",
    174
);
test_function!(
    ex_042,
    "int main() {int a; int b; a = 3; b = (a += 5);  return a + b + 158; }",
    174
);
test_function!(
    ex_043,
    "int main() {int a; int b; a = 3; b = 1; b *= (a += 5);  return a + b + 158; }",
    174
);
test_function!(
    ex_044,
    "int main() {int a; int b; a = 11; a -=5; a /= 2; b = 1; b *= (a += 5);  return a + b + 158; }",
    174
);
test_function!(ex_045, "int main() {int a; int b; int c; a = 7; a &= ~2; a <<= 2; a |=2; a >>= 1; a -=5; a /= 2; b = 3; c = 8; b ^= (c%=3); b *= (a += 5);  return a + b + 158; }",174);
test_function!(ex_046, "int foo(){ return 2;} int main() {int a; int b; int c; a = 3;b = 5;c = 2;if(a) {b = foo();} else { }    return 172+b;}",174);
test_function!(ex_047, "int foo(){ return 2;} int main() {int a; int b; int c; a = 3;b = 5;c = 2;if(a) {b = foo();}   return 172+b;}",174);
test_function!(ex_048, "int foo(){ return 2;} int bar(){ return 7;} int main() {int a; int b; int c; a = 3;b = 5;c = 2;if(a) {b = foo();} else { c = bar();}    return 172+b;}",174);
test_function!(ex_049, "int foo(){ return 2;} int bar(){ return 7;} int main() {int a; int b; int c; a = 0;b = 5;c = 2;if(a) {b = foo();} else { c = bar();}    return 162+b+c;}",174);
test_function!(ex_050, "int foo(){ return 2;} int bar(){ return 7;} int main() {int a; int b; int c; a = 3;b = 5;c = 2;if(a) if(0) { b = foo(); } else {  c = bar(); }    return 162+b+c;}",174);

test_function!(ex_051, "int foo(){ return 2;} int bar(){ return 7;} int main() {int a; int b; int c; a = 3;b = 5;c = 2;if(a) if(0)b=foo();else c = bar();return 162+b+c;}",174);
test_function!(ex_052, "int main() {int a; a = 4; if(1){return 170+a; a = 7; }else{return 170-a; a = 9;} a = 5; return a;}",174);
test_function!(
    ex_053,
    "int foo(){return 1;} int main(){int a; a=0;do{a=3;}while(a==foo());return 174;}",
    174
);
test_function!(
    ex_054,
    "int main(){int a; a=0;do{a+=1;}while(a && a < 174);return a;}",
    174
);
test_function!(
    ex_055,
    "int main(){int a; a=-8;do{a+=1;}while(a);return a+174;}",
    174
);
test_function!(
    ex_056,
    "int foo(){return 3;} int main() {int a; a = 0;while(a == foo()) {a = 3;}return 174;}",
    174
);
test_function!(
    ex_057,
    "int main(){int a; int b; a = 0; b = 0; while(a <= 10) {b += a; a += 1;}return b;}",
    55
);
test_function!(
    ex_058,
    "int main(){int a; a = 3;while (a) {a = 2;if (a - 3) {break;}a += 3;}return 174;}",
    174
);
test_function!(ex_059, "int main(){int a; int b; int c; a = 3; b = 5; c = 0;while(a){while(b) {c += b;b-=1;if(b == 3) break;}b = 7;a-=1;if(a == 1) break;} return a*7+b*15+c*2;}",174);
test_function!(
    ex_060,
    "int main(){int a; a = 3;while (a) {a = 2;if (a - 3) {break;}a += 3;}return 174;}",
    174
);

test_function!(ex_061,"int main(){int a; int b; a=11; b=0; while(a){a-=1;b+=a;if(a)continue;break; a+=100;} return b;}",55);
test_function!(ex_062,"int main(){int a; int b; a =0; b=0; do{a-=1;b+=a;if(a)continue;break; a+=100;}while(a+3); return -a;}",3);
test_function!(ex_063,"int main(){int a; int b; a =0; b=0; do{a-=1;b+=a;if(a)continue;break; a+=100;}while(a+3); return -b;}",6);
test_function!(
    ex_064,
    "int main(){int a; int b; a =-3; b=-6; return a*b*10+a+b+3;}",
    174
);
test_function!(ex_065,"int main(){int a; int b; a =0; b=0; do{a-=1;b+=a;if(a)continue;break; a+=100;}while(a+3); return a*b*10;}",180);
test_function!(ex_066,"int main(){int a; int b; a =0; b=0; do{a-=1;b+=a;if(a)continue;break; a+=100;}while(a+3); return a*b*10+a+b+3;}",174);
test_function!(ex_067,"int main(){int a; int b; a =0; b=0; do{a-=1;b+=a;if(a)continue;break; a+=100;}while(a+3); return a*b;}",18);
test_function!(ex_068,"int main(){int a; int b; a =0; b=0; do{a-=1;b+=a;if(a)continue;break; a+=100;}while(a+3); return b*a;}",18);
test_function!(ex_069,"int main(){int a; int b; a =0; b=0; do{a-=1;b+=a;if(a)continue;break; a+=100;}while(a+3); return b*a*10;}",180);
test_function!(ex_070,"int main(){int a; int b; a =0; b=0; do{a-=1;b+=a;if(a)continue;break;}while(a+3); return a*b;}",18);

test_function!(
    ex_071,
    "int main(){int a; int b; a =0; b=0; do{a-=1;b+=a;if(!a)break;}while(a+3); return a*b;}",
    18
);
test_function!(
    ex_072,
    "int main(){int a; int b; a =0; b=0; do{a-=1;b+=a;}while(a+3); return a*b;}",
    18
);
test_function!(
    ex_073,
    "int main(){int a; int b; a =0; b=0; do{a-=1;b+=a;}while(a+3); return b*a;}",
    18
);
test_function!(
    ex_074,
    "int main(){int a; int b; a=3; b=0; b+= ++a; return a*b*11-2;}",
    174
);
test_function!(
    ex_075,
    "int main(){int a; int b; a=3; b=0; b+= a++; return !(b-3)+!(a-4)+172;}",
    174
);
test_function!(
    ex_076,
    "int main(){int a; int b; a =0; b=0; do{b+=--a;}while(a+3); return b*a;}",
    18
);
test_function!(
    ex_077,
    "int main(){int a; for (a = 3;a;) {a = 2;if (a - 3) {break;}a += 3;}return 174;}",
    174
);
test_function!(
    ex_078,
    "int main(){int a; for (a = 3;;) {a = 2;if (a - 3) {break;}a += 3;}return 174;}",
    174
);
test_function!(
    ex_079,
    "int main(){int a; int b; for(a=0,b=0;a <= 10;) {b += a; a += 1;}return b;}",
    55
);
test_function!(
    ex_080,
    "int main(){int a; int b; for(a=11, b=0;a;){a-=1;b+=a;if(a)continue;break; a+=100;} return b;}",
    55
);
