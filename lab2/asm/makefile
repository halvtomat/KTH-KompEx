.PHONY: all clean

all: main-c main-asm

main-c: main-c.o factorial.o
	gcc main-c.o factorial.o -o main-c

main-c.o: main-c.c
	gcc -c main-c.c 

factorial.o: factorial.c
	gcc -c factorial.c

main-asm: main-asm.o factorial.o
	gcc -no-pie main-asm.o factorial.o -o main-asm

main-asm.o: main-asm.asm
	nasm -felf64 main-asm.asm -o main-asm.o

clean:
	rm -f *.o main-c main-asm *~
