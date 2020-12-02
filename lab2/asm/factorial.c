

#include <stdio.h>

char* format = "My message is '%s'.\n %d! = ";

int fact(int n){
  if(n <= 1)
    return 1;
  else
    return n*fact(n-1);
}

int factorial_message(int n, char* str){
  printf(format, str, n);
  return fact(n);
}
