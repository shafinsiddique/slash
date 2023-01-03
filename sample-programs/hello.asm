section .text
global _main
default rel
extern _printf
extern _strcmp
_main:
push rbp
mov rbp, rsp
sub rsp, 16
sub rsp, 16
mov rdi, const_1
mov rsi, 2
mov rdx, 4
mov rcx, 5
mov r8, 6
mov r9, 8
push r8
mov r8, 10
mov [rsp+(8*0)], r8
pop r8
add rsp, 16
mov rax, 0
call _printf
add rsp, 16
pop rbp
mov rax, 0
ret
section .data
__slash_integer_format: db "%d", 10, 0
__slash_double_format: db "%f", 10, 0
const_1: db "%d %d %d %d %d %d", 10, 0
__slash_doubles_array: dq 