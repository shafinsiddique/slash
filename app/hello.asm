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

