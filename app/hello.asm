section .text
global _main
default rel
extern _printf
_main:
push rbp
mov rbp, rsp
sub rsp, 16
mov r10, 2
mov r11, 2
sub r10, r11
mov r8, r10
mov rdi, __slash_integer_format
mov rsi, r8
call _printf
add rsp, 16
pop rbp
mov rax, 0
ret
section .data
__slash_integer_format: db "%d", 10