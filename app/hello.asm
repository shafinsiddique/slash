section .text
global _main
default rel
extern _printf
extern _strcmp
_main:
push rbp
mov rbp, rsp
sub rsp, 16
mov r10, const_1
mov r11, const_2
mov rdi, r10
mov rsi, r11
call _strcmp
mov r8, rax
mov rdi, __slash_integer_format
mov rsi, r8
call _printf
add rsp, 16
pop rbp
mov rax, 0
ret
section .data
__slash_integer_format: db "%d", 10, 0
const_1: db "bob", 10, 0
const_2: db "sha", 10, 0