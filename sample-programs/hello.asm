global _main
extern _printf
default rel
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
    finit
    mov rdi, msg2
    movups xmm0, [doublesSection + 1 * 8]
    mov rax, 1
    call _printf
    pop rbp 
    mov rax, 0
    ret

section .data

msg:    db      "%d", 10, 0
msg2: db "%f", 10,0
doublesSection: dq 3.12, 3.21
