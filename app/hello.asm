section .text
global _main
default rel
extern _printf
extern _strcmp
_main:
push rbp
mov rbp, rsp
sub rsp, 32
mov r8, const_1
mov [rbp-(8*1)], r8
mov r8, [rbp-(8*1)]
mov rdi, r8
call _printf
mov r8, 10
push r8
mov r9, 20
pop r8
add r8, r9
mov r8, r8
mov [rbp-(8*2)], r8
mov r8, [rbp-(8*2)]
mov rdi, __slash_integer_format
mov rsi, r8
call _printf
add rsp, 32
pop rbp
mov rax, 0
ret
section .data
__slash_integer_format: db "%d", 10, 0
const_1: db "shafin", 10, 0