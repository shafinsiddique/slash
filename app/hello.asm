section   .text
global    _main
default rel
extern _printf

_main:
    push rbp
    mov rdi, format
    mov rsi, message
    call _printf 
    pop rbp
    mov rax, 0
    ret
    
section .data
    message:  db "Hello, Shafin", 0
    format:  db "Replacing with String: %s", 0xA

//          +
// 2 + 2        4 + 3

// evaluation of left side is put in r1.
// So if 2 is an integer, I know it has been put in r1.
// r2
// mov r1 2
// mov r2 2
// add r1, r2, r3 ## store in r3.



