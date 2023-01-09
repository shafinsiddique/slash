section .text
global _main
default rel
extern _printf
extern _strcmp
_main:
push rbp
mov rbp, rsp
xor r8, r8
pop rbp
mov rax, 0
ret
section .data
const_1: db "%d", 10, 0
__slash_doubles_array: dq 