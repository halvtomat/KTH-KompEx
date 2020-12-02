

extern int puts(char *str);
extern int putchar(int character);
extern int atoi(char * str);
extern int factorial_message(int n, char* str);

void print_int(int x){
  if(x < 0){
    putchar('-');
    x = x * (0 - 1);  
  }
  int i = 1000000;
  while(i != 0){
    if(x >= i || (x == 0 && i == 1)){
      putchar('0' + x / i);
    }
    x = x % i;
    i = i / 10;
  }
  putchar('\n');
}


void menu(){
  puts(
    "usage: <command> <arg1> <arg2>\n\n" \
    "commands:\n" \
    "  e   Echo arguments arg1 and arg2\n" \
    "  m   Prints out a magic number\n" \
    "  +   Adds together arg1 and arg2\n" \
    "  !   Prints out the factorial value of\n" \
    "      arg1, as well as the message in arg2\n"
  );
}

 
int main(int argc, char **argv){
  
  if(argc < 4){
    menu();
    return 1;
  }

  char* command = argv[1];
  if(command[0] == 'e'){
    puts(argv[2]);
    puts(argv[3]);
    return 0;
  }
  else if(command[0] == 'm'){
    print_int(-126);
    return 0;
  }
  else if(command[0] == '+'){
    print_int(atoi(argv[2]) + atoi(argv[3]));
    return 0;
  }
  else if(command[0] == '!'){
    int v = factorial_message(atoi(argv[2]), argv[3]);
    print_int(v);
    return 0;
  }
  else{
    menu();
    return 1;
  }
}