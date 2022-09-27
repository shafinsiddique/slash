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
    mov r8, 2
    mov r9, 2
    add r8, r9
    mov r8, r9
    mov rdi, numberFormat
    mov rsi, r8
    call _printf
    mov rax, 0
    ret
    
section .data
    message:  db "Hello, Shafin", 0
    format:  db "Replacing with String: %s", 0xA
    numberFormat: db "Value = %d", 0xA




