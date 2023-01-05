section .text
global _main
default rel
extern _printf
extern _strcmp
_main:
push rbp
mov rbp, rsp
mov eax, 2
mov r8, eax
pop rbp
mov rax, 0
ret
section .data

