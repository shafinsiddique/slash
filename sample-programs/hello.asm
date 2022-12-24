global _main
extern _printf

section .text

_main:
    push rbp
    mov rbp, rsp
    mov rdx, 0
    mov rax, 16
    mov rcx, 3
    div rcx
    mov rdi, msg
    mov rsi, rax
    call _printf
    pop rbp 
    mov rax, 0
    ret

section .data

msg:    db      "%d", 10, 0
