section .text
global _main
default rel
extern _printf
extern _strcmp
_main:
push rbp
mov rbp, rsp
mov rdi, const_1
call _printf
pop rbp
mov rax, 0
ret
section .data
const_1: db `Here's a string with a newline character\nHello World\nWhat is up!`, 10, 0
__slash_doubles_array: dq 12.3,31.4,2.3

