        global main
        extern puts
        extern putchar
        extern atoi
        extern factorial_message

        section .text

menu_str:
        db      "usage: <command> <arg1> <arg2>",10,10
        db      "commands:",10
        db      "  e   Echo arguments arg1 and arg2",10,
        db      "  m   Prints out a magic number",10,
        db      "  +   Adds together arg1 and arg2",10,
        db      "  !   Prints out the factorial value of",10,
        db      "      arg1, as well as the message in arg2",10,0

menu:
        mov     rdi, menu_str   ;add menu_str as argument 
        call    puts            ;call puts with menu_str as argument
        mov     rax, 1          ;set return value to 1
        ret                     ;return

echo:
        mov     rdi, [rbx+8]    ;add argv[2] as argument 
        call    puts            ;call puts with argv[2] as argument
        mov     rdi, [rbx+16]   ;add argv[3] as argument
        call    puts            ;call puts 
        mov     rax, 0          ;set return value to 0
        ret                     ;return

magic_number:
        mov     rdi, -126       ;set -126 as argument
        jmp     print_int       ;jump to print_int

add_args:
        mov     rdi, [rbx+8]    ;set argv[2] as argument
        call    atoi            ;call atoi
        push    rax             ;push the result
        mov     rdi, [rbx+16]   ;set argv[3] as argument
        call    atoi            ;call atoi
        mov     rdi, rax        ;set result as argument
        pop     rax             ;pop the first result
        add     rdi, rax        ;add the first result to the argument
        jmp     print_int       ;jump to print_int with argv[2]+argv[3] as argument

factorial:
        mov     rdi, [rbx+8]    ;set argv[2] as argument
        call    atoi            ;call atoi
        mov     rdi, rax        ;set result as argument 1
        mov     rsi, [rbx+16]   ;set argv[3] as argumnet 2
        call    factorial_message;call factorial_message
        mov     rdi, rax        ;set result as argument
        jmp     print_int       ;jump to print_int 

print_int:
        mov     rbp, rdi        ;move int x to rbp register
        mov     rbx, 1000000    ;int i = 1000000
        cmp     rbp, 0          ;compare x and 0
        jge     L1              ;if x>=0 jump to L1
        mov     rdi, '-'        ;set '-' as argument
        call    putchar         ;call putchar with '-' as argument
        neg     rbp             ;x = -x
L1:     
        cmp     rbx, 0          ;compare int i and 0
        je      L4              ;if i == 0 jump to L4 (after loop)
        cmp     rbp, rbx        ;compare x and i
        jge     L2              ;if x is greater or equal to i, jump to L2
        cmp     rbp, 0          ;compare x and 0
        jne     L3              ;if not equal jump to L3 (after if statement)
        cmp     rbx, 1          ;compare i and 1
        jne     L3              ;if not equal jump to L3 (after if statement)
L2:     
        mov     rax, rbp        ;move x into rax register
        div     rbx             ;divide x by i
        mov     rdi, rax        ;set result as argument
        add     rdi, '0'        ;add '0' to argument
        call    putchar         ;call putchar with '0' + x/i as argument
L3:     
        mov     rax, rbp        ;move x into rax register
        cdq                     ;sign extend 32-bit register (???)
        xor     rdx, rdx
        div     rbx             ;divide x by i
        mov     rbp, rdx        ;move remainder to rbp register
        mov     rax, rbx        ;move i into rax register
        mov     r10, 10         ;move 10 inte r10 register
        cdq                     ;sign extend 32-bit register (???)
        xor     rdx, rdx
        div     r10             ;divide i by 10
        mov     rbx, rax        ;move result to rbx register
        jmp     L1              ;jump to beginning of loop
L4:     
        mov     rdi, 10         ;set '\n' as argument
        call    putchar         ;call putchar with '\n' as argument
        mov     rax, 0          ;set return value to 0
        ret                     ;return

main:
        cmp     rdi, 4          ;compare argc with 4
        jl      menu            ;if argc < 4, go to menu

        mov     rbp, rdi        ;move argc to rbp register
        mov     rbx, rsi        ;move argv to rbx register

        add     rbx, 8          ;increment argv pointer to 1 (argv[1])
        mov     r10, [rbx]      ;r10 = argv[1]
        cmp     byte [r10], 'e' ;compare argv[1] with 'e'
        je      echo            ;if argv[1] == 'e', go to echo

        cmp     byte [r10], 'm' ;compare argv[1] with 'm'
        je      magic_number    ;if argv[1] == 'm', go to magic_number

        cmp     byte [r10], '+' ;compare argv[1] with '+'
        je      add_args        ;if argv[1] == '+', go to add_args

        cmp     byte [r10], '!' ;compare argv[1] with '!'
        je      factorial       ;if argv[1] == '!', go to factorial

        jmp     menu            ;else go to menu