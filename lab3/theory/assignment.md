# Theory Tasks - Module 3

## 1. Binutils, Linkers, and Loaders

Please revisit lecture 4, Part II about Linker and Loaders. In the archive file lecture-binutils.zip (available on Canvas on page "Assignment Tasks") there are many different examples. Take a closer look at the Makefile for different commands. For instance, make hello-c compiles file hello-c.c and disassembles the binary file using command objdump.

Go through the different examples, so that you understand the fundamentals of some of the most essential GNU binutils: nm, objdump, size, and strip. You can use the man command to get more info about the command, e.g., man objdump.

### Tasks

In the first exercise of Module 2, you developed assembly code. In that project, there was a pre-existing file
*factorial.c* that you compiled and used in your assembly project. You can find the original files in folder
/template-projects/asm. You can either copy these files from there again or use the files in your
module 2 solution.

NOTE: when you solve the tasks in this exercise, you **MUST** use the Vagrant setup supplied in the course.
The reason for this is that you need to get specific values, which can otherwise depend on the setup.

For all the subtasks below, write down the commands on your answer sheet and explain what the commands
mean.

### Task 1a
Compile file *factorial.c* into an object file called *factorial.o*. The file should be compiled with
maximal optimization enabled, but still conforming to standard compliance (see the man page)

### Task 1b
How large is the code in bytes of the compiled object file (with optimization enabled)? If you compile it again using no optimizations enabled, what is the size of the code then?

### Task 1c
What is the size of the data section? Why is it not zero? In what way are these bytes used (inspect the C file and try to figure out an answer).

### Task 1d
What is the size of the *bss* section? Explain why. When is the *bss* section used?

### Task 1e
If you compile according to step 1a, what is then the first instruction in function *fact* and the first instruction
in function *factorial_message*?

### Task 1f
Suppose you have compiled the main program using command 

    gcc factorial.c main-c.c     
Give the command for listing all the symbols of the generated binary. What is the first symbol in the generated list?
Also, as you can see, there are many more symbols than in the original C program. Where are all these
symbols coming from?

### Task 1g
Give the command that removes all symbols from a binary. Show how you can test that the symbols are
removed. Explain why the symbols are not needed to execute the program, and why they are there, if they are
not needed? Moreover, why *are* symbols indeed needed in an object file?

## 2. Garbage Collection

Go through video tutorial "garbage collection" in detail. Specifically, make sure that you understand the
details of mark-and-sweep collection, and that you have a good general knowledge of the other methods.

### Task 2a
Suppose we have the following definition of a *struct*:

    struct S{
        S * p1;
        S * p2;
    };
Moreover, suppose that the following code is executed. Note that the code is divided into three separate parts:

    /* -- Part 1 -- */
    S* a = new S[1];
    S* b = new S[1];
    S* c = new S[1];
    S* d = new S[1];
    a[0].p1 = NULL;
    a[0].p2 = NULL;
    b[0].p1 = a;
    b[0].p2 = d;
    c[0].p1 = NULL;
    2c[0].p2 = d;
    d[0].p1 = c;
    d[0].p2 = NULL;
    a = NULL;
    d = NULL;
    /* -- Part 2 -- */
    b = NULL;
    /* -- Part 3 -- */
    c = NULL;
In similar details as described in the tutorial video, draw and explain the layout (showing memory addresses,
pointers, and arrows) of the memory, after executing part 1.

Note that you need to add text of what is happening.

### Task 2b
Suppose that part 2 has been executed, and a mark-and-sweep garbage collection is invoked. Describe in detail the mark-and-sweep steps using this example. Clearly show what becomes garbage and why.

### Task 2c
Again, explain what happens when mark-and-sweep GC executes after part 3 has been executed.

### Task 2d
After part 3, what would happen if reference counting had been used instead? Clearly explain why that is the case.

### Task 2e
Explain in your own words the difference between a copying garbage collector and a mark-and-sweep collector.

### Task 2f
Explain in your own words why generational collectors and incremental collectors have some benefits.

Discuss the main differences between them.

## 3. Register Allocation
Go through lecture 6 "Register Allocation" in detail. Specifically, make sure that you understand the details of
liveness analysis, and that you have a good general knowledge about interference graphs and register allocation using graph coloring.

### Task 3a
Consider the following Cigrid program:

    int bar(int n){
        int i = 0;
        int x = 2;

        while(i < n){
            if(i > 10){
                x = x + n;
                i = i + 2;
            }
            else{
                x = x * 3;
                i++;
            }
        }
        return x;
    }
Do the following:
- Divide the program into basic blocks and draw the control-flow graph of the program. Make sure to include the program statements in the blocks.
- For all basic blocks, clearly write out *def* and *use* sets.
- Illustrate how the liveness analysis algorithm works for this example. Clearly show each iteration, and how *live-in* and *live-out* sets are computed in each iteration.
- Show the end result when the algorithm has reached a fixed point.

### Task 3b
Construct the interference graph for the control-flow graph above.

### Task 3c
Informally explain how an interference graph can be used when performing register allocation using graph coloring. You do not need to show it with the example above. Instead, describe shortly the intuition of how the interference graph is used in register allocation.