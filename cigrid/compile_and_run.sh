#! /bin/bash
./cigrid -a test.c > out
nasm -felf64 out -o o.o
gcc -no-pie o.o 
./a.out