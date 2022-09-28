section .text
global _main
default rel
extern _printf
_main:
push rbp
mov r8, 2
push r8
mov r9, 2
pop r8
add r8, r9
mov r8, r8
push r8
mov r9, 2
pop r8
add r8, r9
mov r8, r8
mov rdi, __slash_integer_format
mov rsi, r8
call _printf
pop rbp
mov rax, 0
ret
section .data
__slash_integer_format: db "%d", 10