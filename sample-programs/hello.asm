global _main
extern _printf

section .text

_main:
    push rbp
    mov rbp, rsp
    mov rdi, msg
    call _printf
    pop rbp 
    mov rax, 0
    ret

section .data

msg:    db      "Hello World!", 10, 0
