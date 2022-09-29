section .text
global _main
default rel
extern _printf
_main:
push rbp
mov rbp, rsp
sub rsp, 16
mov r8, 2
push r8
mov r9, 2
pop r8
add r8, r9
mov r8, r8
mov [rbp-(8*1)], r8
mov r8, const_1
mov rdi, r8
call _printf
add rsp, 16
pop rbp
mov rax, 0
ret
section .data
__slash_integer_format: db "%d", 10
const_1: db "hello", 10