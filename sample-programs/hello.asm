section .text
global _main
default rel
extern _printf
extern _strcmp
_main:
push rbp
mov rbp, rsp
mov r8, 12
mov r9, 14
sub rsp, 16
mov [rbp-8], r8
mov [rbp-16], r9
mov rsi, [rbp-8]
mov rdx, [rbp-16]
mov rdi, const_2
call _printf
add rsp, 16
pop rbp
mov rax, 0
ret
section .data
const_1: db "hello world!", 10, 0
const_2: db "%d %d", 10, 0
__slash_doubles_array: dq 2.1,4.9,3.22
