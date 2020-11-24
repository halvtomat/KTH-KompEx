
/* This file tests various syntax aspects of 
   a Cigrid parser.*/


// Note that this include statment should be ignored
#include <stdio.h>


// Empty function (S)
void empty(){
}
//
// Function Declaration
void declared();


// Test one integer paramter and return statement (S)
int one_param_return(int x){
  return x;
}


// Test two parameters (S)
int two_params_return(int x, int y){
  return x;
}


// Function call with no arguments (S)
void call_no_args_call_statemant(){
  empty();
}


// Function call with one argument (S)
int call_one_args(){
  return one_param_return(123);
}


// Function call with two arguments (S)
int call_two_args_var_expr(int x){
  return two_params_return(321,x);
}


// Print characters and return without value (S)
void print_char_consts(){
  putchar('A');
  putchar('\n');
  putchar('\t');
  putchar('.');
  putchar('\\');
  putchar('\'');
  putchar('\"');
  putchar('\n');
  return;
}


// Various arithmetic expressions (S)
void arith_assignments(int x, int y, int z){
  x = x + 1;
  x = x - y * 8;
  y = 12 % z / 3;
}


// Arithmetic expressions including unary minus (G)
void arith_assignments_ext(int x){
  x = 8 + -2 - -8 * (9 + 1);
}


// Various logical operations (S)
void logic_operations(int x, int y, int z){
  x = y | z & 7; 
  x = y || z && y | 1;
  y = x > y && y < x || z >= y && ((x <= y) == 0);
  z = x == y || x != z;
}

/*
// Various logical operations (G)
void logic_operations_ext(int x, int y, int z){
  y = ~z >= y && !(x <= y);
  x = x << 3 & z >> 7;
}
*/


// If statement and if-then-else statement (S)
int if_then_else(int x, int y){
  if(x == y)
    return y;
  
  if(x > 0){
    if(x == 3){
      x = x + 1;
      return x;
    }
  }
  else{
    x = 7;
    return x + 1;
  }
  
  return x;
}


// Correct parsing of dangling else (S)
int if_then_dangeling_else(int x){
  if(x > 0)
    if(x < 10)
      return x + 1;
    else
      return x + 2;
  return x;
}
 

// While loop with break (S)
int while_break(int x){
  while(x < 10){
    if(x == 7)
      break;
    x = x + 1;      
  }
  return x;
}


// postfix ++ and -- operations on identifiers (S)
void pluspluss_minusminus(int x){
  x++;
  x--;
}


// Local variable definitions (S)
void defining_local_variables(){
  int x = 1;
  int y = 5;
}

/*
// Local variable definitions (G)
void hexadecimal_numbers(){
  int x = 0x1a3F;
  int y = 0X001fA;
}

// Global definition (G)
int global_x = 5 + 10;


// External declaration (G)
extern int global_y;

*/
// Use of a global variable (S)
int use_global(int x){
  return global_x + global_y + x;
}

/*
// Global definition after use (G)
int global_y = 7;


// For loops, with and with out scopes + break (G)
// Should be encoded as syntactic sugar
int for_loop_with_break(int x, int y){
  for(int i=0; i < x; i++){
    if(i >= y)
      break;
  }
  for(int i=0; i < x; i++)
    x = x + 1;
 
  return x;
}



// Empty struct (G)
struct S1 {
};


// Struct with elements (G)
struct S2 {
  int x;
  char y;
};


// Access and updates of structures and arrays (G)
void struct_arrays(){ 
  S2 * a1 = new S2[5]; 
  a1[2].x = 3;
  a1[2].x++;
  a1[2].x--;
  int y = a1[2].y + 1;
  delete[] a1;
}


// Integer arrays (G)
void int_arrays(int x, int* p){
  int* a = new int[x];
  int y = x % 10;
  a[7] = a[19+y];
  a[6]++;
  a[6]--;  
}


// Strings (G)
char string_test(int x){
  char * a = "Test\n\t\\\'\"";
  return a[x];
}

*/
// Print a signed integer in decimal form (S)
void print_int(int x){
  if(x < 0){
    putchar('-');
    x = x * (0 - 1);  
  }
  int i = 1000000;
  while(i != 0){
    if(x >= i || (x == 0 && i == 1))
      putchar('0' + x / i);
    x = x % i;
    i = i / 10;
  }
}


// Print integer line (S)
void print_int_ln(int x){
  print_int(x);
  putchar('\n');
}

/*
// Print a string (G)
void print_string(char* s){
  int i = 0;
  while(s[i]){
    putchar(s[i]);
    i++;
  }
}


// Tree structure (G)
struct Tree{
  int val;
  Tree * left;
  Tree * right;  
};


// Help function, construct binary node (G)
Tree* node(Tree* left, int x, Tree* right){
  Tree* t = new Tree[1];
  t[0].val = x;
  t[0].left = left;
  t[0].right = right;
  return t;
}


// Helf function, construct leaf (G)
Tree* leaf(int x){
  Tree* t = new Tree[1];
  t[0].val = x;
  t[0].left = 0;
  t[0].right = 0;
  return t;
}


// Perform inorder traversal and print values (G)
// Tests recursicve calls
void inorder_traversal(Tree* t){
  if(t[0].left == 0){
    print_int(t[0].val);
    putchar(',');
  }
  else{
    inorder_traversal(t[0].left);
    print_int(t[0].val);
    putchar(',');
    inorder_traversal(t[0].right);    
  }
}


// Declare function before definition (G)
extern void delete_tree(Tree* t);


// Test print inorder traversal (G)
void test_recursive_data_structures(){
  print_string("---test-recursive-data-structures---\n");
  Tree* t = node(node(leaf(1), 2, leaf(3)), 4, leaf(5));
  inorder_traversal(t);
  putchar('\n');
  delete_tree(t);
}


// Recursively delete the tree (G)
void delete_tree(Tree* t){
  if(t[0].left != 0){
    delete_tree(t[0].left);
    t[0].left = 0;
    delete_tree(t[0].right);
  }
  delete[] t;
}


// A global string (G)
char* text1 = "Text\n"; 


// Print a local and a global string (G)                                  
void print_test_strings(){
  print_string(text1);
  char * text2 = "Text2\n";
  print_string(text2);
}

*/
// Main function (S) 
int main(){
  test_recursive_data_structures();
  print_test_strings();
  return 0;
}
