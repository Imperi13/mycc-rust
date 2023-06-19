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

test_function!(
    ex_081,
    "int main(){int a; int b; for(a=0,b=0;a <= 10;++a) {b += a;}return b;}",
    55
);
test_function!(
    ex_082,
    "int main(){int a; int b; for(a=0,b=0;a <= 10;a++) {b += a;}return b;}",
    55
);
test_function!(ex_083,"int main(){int a; int b; int c; int d; d=0; b = 5; c = 0;for(a = 3;a;d++){for(;b;++d) {c += b;b-=1;if(b == 3) break;}b = 7;a-=1;if(a == 1) break;} return a*7+b*15+c*2;}",174);
test_function!(
    ex_084,
    "int main(){int a; int b; for(a=0,b=0;a<10;a++){ if(a ==5)continue;b+=a;} return b;}",
    40
);
test_function!(
    ex_085,
    "int main(){int a; a = 174; {int a; a = 3;} return a;}",
    174
);
test_function!(
    ex_086,
    "int main(){int a; a = 3; { a = 174;} return a;}",
    174
);
test_function!(
    ex_087,
    "int main() {int *b; int a; a = 3; a += 5;  return a + 166; }",
    174
);
test_function!(
    ex_088,
    "int main() {int *******b; int a; a = 3; a += 5;  return a + 166; }",
    174
);
test_function!(
    ex_089,
    "int main() {int a; a = 174; int *b; b = &a; return a;}",
    174
);
test_function!(
    ex_090,
    "int main(){int x;x = 86;int *y;y = &x; return (*y) + x + 2;}",
    174
);

test_function!(
    ex_091,
    "int main(){int x;x = 86;int *y;y = &x; return (*y) + (*y) + 2;}",
    174
);
test_function!(
    ex_092,
    "int main(){int x;x = 86;int *y;y = &x;int **z;z = &y;return (*y) + (**z) + 2;}",
    174
);
test_function!(
    ex_093,
    "int main(){int x;x = 86;int *y;y = &x;int **z;z = &y;return*y+**z+2;}",
    174
);
test_function!(
    ex_094,
    "int main() {int x;int *y;x = 3;y = &x;*y = 174;return x;}",
    174
);
test_function!(
    ex_095,
    "int main() {int x;int *y;x = 3;y = &x;*y = 171;*y += 3;return x;}",
    174
);
test_function!(
    ex_096,
    "int main(){int x; int y; int *z; int*a; z=&x; a=&y; *z=*a=87; return(x+y);}",
    174
);
test_function!(
    ex_097,
    "int main(){int x; int *y; int **z; z = &y; *z = &x; *y = 174; return x;}",
    174
);
test_function!(
    ex_098,
    "int foo(int* p){return 3;} int main(){int x; return 174;}",
    174
);
test_function!(
    ex_099,
    "int foo(int* p){return *p;} int main(){int x; x = 174; return foo(&x);}",
    174
);
test_function!(
    ex_100,
    "int foo(int* p){*p = 172; return *p+2;} int main(){int x; return foo(&x);}",
    174
);

test_function!(
    ex_101,
    "int *foo(int *p){*p = 4;return p;} int main(){int x;int *y;y = foo(&x); *y+= 170;return x;}",
    174
);
test_function!(
    ex_102,
    "int *foo(int *p){*p = 4;return p;} int main(){int x;int y;*foo(&x) += 170;return x;}",
    174
);

test_function!(ex_113,"int *foo(int *p){*p = 4;return p;} int main(){int x;int y; int **z; *foo(&x) += 170;return x;}",174);
test_function!(ex_114, "int main(){int a[2][3]; return 174;}", 174);
test_function!(ex_115, "int x; int *y; int main(){return 174;}", 174);
test_function!(ex_116, "int x; int *y; int main(){return x+174;}", 174);
test_function!(
    ex_117,
    "int x; int *y; int main(){x=3; int a; a=2; y=&a; return x+*y+169;}",
    174
);

test_function!(
    ex_118,
    "int main(){int a[1]; int *p; p = a; *p=2; return 174;}",
    174
);
test_function!(ex_119, "int main(){int a[1]; *(a+0)=2;return 174;}", 174);
test_function!(
    ex_120,
    "int x; int *y; int main(){x=3; int a[1]; *a=2; y=a; return x+*y+169;}",
    174
);

test_function!(
    ex_121,
    "int x; int main(){x=3; int *y; y=&x; return *y+171;}",
    174
);
test_function!(ex_122, "int a[1]; int main(){ *a=2;return 174;}", 174);
test_function!(
    ex_123,
    "int main(){int a[1][2];int *q;q = *a;return 174;}",
    174
);
test_function!(
    ex_124,
    "int main(){int a[1][2];int *q;q = *a; *q=174; return **a;}",
    174
);
test_function!(
    ex_125,
    "int main(){int a[86][2];int *q;q = *(a+1); *q=174; return **(a+1);}",
    174
);
test_function!(
    ex_126,
    "int main(){int a[5][6];int *q;q = *(a+1); *(2+q)=174; return *(*(1+a)+2);}",
    174
);
test_function!(ex_127,"int changeBoard(int board[30][30], int i, int j, int d, int N){int k;for (k = 0; k < N; k++) {*(*(board + i) + k) += d;*(*(board + k) + j) += d;}if (i > j) {for (k = 0; k < N - (i - j); k++) {*(*(board + k + (i - j)) + k) += d;}} else {for (k = 0; k < N - (j - i); k++) {*(*(board + k) + k + (j - i)) += d;}}if (i + j < N) {for (k = 0; k <= i + j; k++) {*(*(board + i + j - k) + k) += d;}} else {for (k = i + j - N + 1; k < N; k++) {*(*(board + i + j - k) + k) += d;}}return 0;}int setQueen(int board[30][30], int num_placed, int *ptr_sol_num, int N){int j;if (num_placed == N) {(*ptr_sol_num)+=1;return 0;}for (j = 0; j < N; j++) {if (*(*(board+num_placed)+j) == 0) {changeBoard(board, num_placed, j, +1, N);setQueen(board, num_placed + 1, ptr_sol_num, N);changeBoard(board, num_placed, j, -1, N);}}return 0;}int board_[30][30];int main(){int sol_num;sol_num = 0;setQueen(board_, 0, &sol_num, 8);return sol_num;}",92);
test_function!(ex_128,"int count;int solve(int n, int col, int *hist){if (col == n) {count+=1;return 0;}int i;int j;for (i = 0, j = 0; i < n; i++) {for (j = 0; j < col && *(hist + j) != i && (*(hist + j) - i) != col - j && (*(hist + j) - i) != j - col; j++){}if (j < col)continue;*(hist+col) = i;solve(n, col + 1, hist);}return 0;}int main(){int hist[8];solve(8, 0, hist);return count;}",92);
test_function!(ex_129,"int count;int solve(int n, int col, int *hist){if (col == n) {count+=1;return 0;}int i;int j;for (i = 0, j = 0; i < n; i++) {for (j = 0; j < col && *(hist + j) != i && (hist [j] - i) != col - j && (*(hist + j) - i) != j - col; j++){}if (j < col)continue;*(hist+col) = i;solve(n, col + 1, hist);}return 0;}int main(){int hist[8];solve(8, 0, hist);return count;}",92);
test_function!(ex_130,"int count;int solve(int n, int col, int *hist){if (col == n) {count+=1;return 0;}int i;int j;for (i = 0, j = 0; i < n; i++) {for (j = 0; j < col && hist [j] != i && (hist [j] - i) != col - j && (hist[j] - i) != j - col; j++){}if (j < col)continue;hist[col] = i;solve(n, col + 1, hist);}return 0;}int main(){int hist[8];solve(8, 0, hist);return count;}",92);

test_function!(ex_131,"int changeBoard(int board[30][30], int i, int j, int d, int N){int k;for (k = 0; k < N; k++) {board[i][k] += d;board[k][j] += d;}if (i > j) {for (k = 0; k < N - (i - j); k++) {board [k + (i - j)][k] += d;}} else {for (k = 0; k < N - (j - i); k++) {board[k][k + (j - i)] += d;}}if (i + j < N) {for (k = 0; k <= i + j; k++) {board[i + j - k][k] += d;}} else {for (k = i + j - N + 1; k < N; k++) {board[i + j - k][k] += d;}}return 0;}int setQueen(int board[30][30], int num_placed, int *ptr_sol_num, int N){int j;if (num_placed == N) {(*ptr_sol_num)+=1;return 0;}for (j = 0; j < N; j++) {if (board[num_placed][j] == 0) {changeBoard(board, num_placed, j, +1, N);setQueen(board, num_placed + 1, ptr_sol_num, N);changeBoard(board, num_placed, j, -1, N);}}return 0;}int board_[30][30];int main(){int sol_num;sol_num = 0;setQueen(board_, 0, &sol_num, 8);return sol_num;}",92);
test_function!(
    ex_132,
    "int main(){int a[5][6];int *q;q = a[1]; 2[q]=174; return 1[a][2];}",
    174
);

test_function!(
    ex_133,
    "char foo(){char a; return a;} int main(){foo(); return 174;}",
    174
);
test_function!(
    ex_134,
    "char foo(char *p){char a; return a;} int main(){char q; foo(&q); return 174;}",
    174
);
test_function!(
    ex_135,
    "char foo(char *p){char a; a = 5; return a;} int main(){char q; foo(&q); return 174;}",
    174
);
test_function!(
    ex_136,
    "int main(){char x[3]; x[0] = -1; x[1] = 2; int y; y = 4; return x[0] + y + 171;}",
    174
);
test_function!(ex_137,"char foo(char *p){*p = 5; char a;a = 3; return a;} int main(){char q; char r; r = foo(&q); return 172-r+q;}",174);
test_function!(ex_138,"char a;char foo(char *p){*p = 5; a = 3; return a;} int main(){char q; char r; r = foo(&q); return 172-r+q;}",174);
test_function!(ex_139,"int foo(char a){int d;d = 3;char c;c = a+d;return c;} int main(){char f;f=3;return foo(f)*4+150;}",174);
test_function!(ex_140,"int foo(char a){int d;d = 3;char c;c = a+d;return c*4;} int main(){char f;f=3;return foo(f)+150;}",174);

test_function!(
    ex_143,
    "int foo(char a, char b){return 23;} int main(){char f;f=3;return foo(f,4)+151;}",
    174
);
test_function!(
    ex_144,
    "int foo(char a, char b){return a*4+11;} int main(){char f;f=3;return foo(f,4)+151;}",
    174
);
test_function!(
    ex_145,
    "int foo(char a, char b){return a*4+12;} int main(){char f;f=3;return foo(f,4)+150;}",
    174
);
test_function!(
    ex_146,
    "int foo(char a, char b){return (a+3)*4;} int main(){char f;f=3;return foo(f,4)+150;}",
    174
);
test_function!(ex_147,"int foo(char a, char b){char c;c = a+3;return c*4;} int main(){char f;f=3;return foo(f,4)+150;}",174);
test_function!(ex_148,"int foo(char a, char b){int d;d = 3;char c;c = a+d;return c*4;} int main(){char f;f=3;return foo(f,4)+150;}",174);
test_function!(ex_149,"int foo(char a, char b){int d;d = 3;char c;c = a+d;return c*b;} int main(){char f;f=3;return foo(f,4)+150;}",174);
test_function!(ex_150,"char foo() { char *x;x = \"1ab\"; return x[0]; }int main(){ char *y;y = \"a2b\"; int z;z = 12; char a;a = y[1]; return (a-foo())*z+162;}",174);

test_function!(
    ex_151,
    "int printf();int main(){printf(\"%d %s\", 1, \"a\");return 174;}",
    174
);
test_function!(ex_152,"int printf();int puts();int A[200][200];int main() {int i; for (i = 1; i <= 12; i++) { printf(\"%d %d\", i, i); puts(\"\"); } return 0;}",0);
test_function!(ex_153,"int printf();int puts();int a(int b, int c) {return 3;}int main() {int i; for (i = 1; i <= 12; i++) { int j;j = a(0, i); printf(\"%d %d\", i, j); puts(\"\");} return 0;}",0);
test_function!(ex_154,"int printf();int puts();int A[200][200];int dfs(int row, int N) { if (row == N) return 1; int ret;ret = 0; int col;for (col = 0; col < N; col++) { int ok; ok = 1; int i; for (i = 1; i < N; i++) { if (row - i >= 0 && col - i >= 0) { ok = ok && A[row - i][col - i] == 0; } if (row - i >= 0) { ok = ok && A[row - i][col] == 0; } if (row - i >= 0 && col + i < N) { ok = ok && A[row - i][col + i] == 0; } } if (ok) { A[row][col] = 1; ret += dfs(row + 1, N); A[row][col] = 0; } } return ret;}int main() {int i; for (i = 1; i < 11; i++) { int j; j = dfs(0, i); printf(\"%d queen: %d\", i, j); puts(\"\");} return 0;}",0);
test_function!(ex_155,"int printf();int puts();int count;int solve(int n, int col, int *hist){if (col == n) {count+=1;return 0;}int i;int j;for (i = 0, j = 0; i < n; i++) {for (j = 0; j < col && hist [j] != i && (hist [j] - i) != col - j && (hist[j] - i) != j - col; j++){}if (j < col)continue;hist[col] = i;solve(n, col + 1, hist);}return 0;}int main(){int i; int hist[20]; for (i = 2; i < 11; i++) { count=0; solve(i, 0, hist); printf(\"%d queens: %d\", i, count); puts(\"\");} return 0;}",0);
test_function!(ex_156, "int main(){/**/return 123;}", 123);
test_function!(
    ex_157,
    "int main(){/*u89g3wihu-@w3erolk*/ return (123);}",
    123
);
test_function!(
    ex_158,
    "int/*/* 0^[o;:._/-*/main(){return ((((123))));}",
    123
);
test_function!(
    ex_159,
    "int a; int main(){int *p; p = &a; int i; for(i=0;i<174;i++){++*p;} return a;}",
    174
);
test_function!(
    ex_160,
    "int a; int main(){int *p; p = &a; int i; for(i=0;i<174;((i))++){++*p;} return a;}",
    174
);

test_function!(
    ex_161,
    "int main(){int a[10]; a[5] = 173; int b; b = a[5]++; return a[5]*!(a[5]-b-1);}",
    174
);

test_function!(
    ex_162,
    "int printf();int a() {return 3;}int main() {int i; printf(\"%d %d\", i, a()); return 0;}",
    0
);
test_function!(ex_163,"int foo(char *a, int b, int c){return 0;} int a(int N) {return 3;}int main() {int i; foo(\"%d %d\", i, a(i)); return 0;}",0);

test_function!(ex_165,"int printf();int a(int N) {return 3;}int main() {int i; printf(\"%d %d\", i, a(i)); return 0;}",0);
test_function!(ex_166,"int printf();int puts();int a(int N) {return 3;}int main() {int i; for (i = 1; i <= 12; i++) { printf(\"%d %d\", i, a(i)); puts(\"\");} return 0;}",0);
test_function!(ex_167,"int printf();int puts();int A[200][200];int a(int row, int N) {return 3;}int main() {int i; for (i = 1; i <= 12; i++) { printf(\"%d %d\", i, a(0, i)); puts(\"\");} return 0;}",0);
test_function!(ex_168,"int printf();int puts();int A[200][200];int dfs(int row, int N) { if (row == N) return 1; int ret;ret = 0; int col;for (col = 0; col < N; col++) { int ok; ok = 1; int i; for (i = 1; i < N; i++) { if (row - i >= 0 && col - i >= 0) { ok = ok && A[row - i][col - i] == 0; } if (row - i >= 0) { ok = ok && A[row - i][col] == 0; } if (row - i >= 0 && col + i < N) { ok = ok && A[row - i][col + i] == 0; } } if (ok) { A[row][col] = 1; ret += dfs(row + 1, N); A[row][col] = 0; } } return ret;}int main() {int i; for (i = 1; i < 12; i++) { printf(\"%d queen: %d\", i, dfs(0, i)); puts(\"\");} return 0;}",0);

test_function!(
    ex_169,
    "int a(int b){ return b; }int main(){int i; i=1; a(i == 1? 1 : 2); return 0;}",
    0
);
test_function!(ex_170,"int a(int b){ return b; }int main(){int i; for (i = 1; i < 11; i++) { a(i == 1? 1 : 2); } return 0;}",0);

test_function!(
    ex_171,
    "int a(int b){ return b; }int main(){int i; i=1; return a(i == 1? 174 : 2);}",
    174
);

test_function!(ex_172,"int printf();int puts();int count;int main(){int i; int hist[20]; for (i = 1; i < 11; i++) { printf(i == 1? \"a\" : \"b\"); puts(\"\");} return 0;}",0);
test_function!(ex_173,"int printf();int puts();int count;int main(){int i; int hist[20]; for (i = 1; i < 11; i++) { printf(\"%s\", (i == 1? \"a\" : \"b\")); puts(\"\");} return 0;}",0);
test_function!(ex_174,"int printf();int puts();int count;int main(){int i; int hist[20]; for (i = 1; i < 11; i++) { printf(\"%d %s: %d\", i, (i == 1? \" \" : \"s \"), i); puts(\"\");} return 0;}",0);
test_function!(ex_175,"int printf();int puts();int count;int solve(int n, int col, int *hist){if (col == n) {count+=1;return 0;}int i;int j;for (i = 0, j = 0; i < n; i++) {for (j = 0; j < col && hist [j] != i && (hist [j] - i) != col - j && (hist[j] - i) != j - col; j++){}if (j < col)continue;hist[col] = i;solve(n, col + 1, hist);}return 0;}int main(){int i; int hist[20]; for (i = 1; i < 11; i++) { count=0; solve(i, 0, hist); printf(\"%d queen%s: %d\", i, (i == 1? \" \" : \"s \"), count); puts(\"\");} return 0;}",0);
test_function!(
    ex_176,
    "int main(){int a; int *p; p = &a; *p = 2; int *q; q = &*p; *q = 174; return a;}",
    174
);
test_function!(
    ex_177,
    "int main(){int a; int *p; p = &a; *p = 2; int *q; q = &(*p); *q = 174; return a;}",
    174
);
test_function!(
    ex_178,
    "char foo(char *p){char a; return a;} int main(){char q; foo(&(q)); return 174;}",
    174
);

test_function!(ex_179,"char; char     ; char; int; int ; int; int;int;char foo(char *p){char a; return a;} int main(){char q; foo(&(q)); return 174;}",174);
test_function!(ex_180," struct A; char; char     ; char; int; int ; int; struct B;  int;int;  struct C; int main(){return 174;}",174);
test_function!(ex_181," struct A{int a; int b;}; char; char     ; char; int; int ; int; struct B{int c; int b;};  int;int;  struct C; int main(){return 174;}",174);
test_function!(ex_182, "int main(){ int; return 174;}", 174);
test_function!(
    ex_183,
    "struct A{int a; int b;}; int main(){ struct A; return 174;}",
    174
);
test_function!(
    ex_184,
    "struct A{int a; int b;}; int main(){ struct A a; return 174;}",
    174
);
test_function!(
    ex_185,
    "struct A{int a; int b;}; int main(){ struct A a[10]; return 174;}",
    174
);
test_function!(
    ex_186,
    "struct A{int a; int b;};  struct A a[10]; int main(){return 174;}",
    174
);

test_function!(ex_196,"int *f(int *p){return p;} int main(){int a[4]; a[0] = 1; f(a)[0]++; f(a)[1] = 172; return a[1]+a[0];}",174);
test_function!(
    ex_197,
    "struct A{char a; int b;}; int main(){struct A a; a.a = 74; return a.a;}",
    74
);
test_function!(
    ex_198,
    "struct A{int a; int b;}; int main(){struct A a; a.a = 174; return a.a;}",
    174
);
test_function!(
    ex_199,
    "struct A{int a; int b;}; int main(){struct A a; a.a = 174; return a.a;}",
    174
);

test_function!(
    ex_204,
    "int main(){int *p; p = 0; if(p) {return 4; } return 174;}",
    174
);
test_function!(
    ex_205,
    "int main(){int *p; int a; p = &a; if(p) {return 4; } return 174;}",
    4
);
test_function!(
    ex_206,
    "int main(){int *p; int a; p = &a; return p && &p;}",
    1
);
test_function!(
    ex_207,
    "int main(){int *p; int a; p = &a; return p || &p;}",
    1
);
test_function!(
    ex_208,
    "int main(){int *p; int a; p = &a; return p?174:1;}",
    174
);
test_function!(ex_209, "int main(){int *p; p = 0; return p?174:1;}", 1);
test_function!(ex_210, "int main(void){return 174;}", 174);
test_function!(
    ex_211,
    "int main(void){void *p; p = 0; p = p; return 174;}",
    174
);
test_function!(ex_212,"struct A{int a; int b;}; int main(){ struct A *p; void *q1; void *q2; q1 = p; q2 = p+1; char *r1; char *r2; r1 = q1; r2 = q2; return r2-r1;}",8);
test_function!(
    ex_213,
    "void f(int *p){*p = 174; return;} int main(void){ int a; f(&a); return a;}",
    174
);
