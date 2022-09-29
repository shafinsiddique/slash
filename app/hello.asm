section .text
global _main
default rel
extern _printf
_main:
    push rbp
    mov rax, 4
    sub rsp, 16
    mov [rsp], rax
    mov rax, 8
    mov [rsp+8], rax
    mov rdi, __slash_integer_format
    mov rsi, [rsp+8]
    call _printf
    add rsp, 16
    pop rbp
    mov rax, 0
    ret
section .data
    __slash_integer_format: db "%d", 10