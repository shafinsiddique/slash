section .text
global _main
default rel
extern _printf
extern _strcmp
_main:
push rbp
mov rbp, rsp
sub rsp, 2
mov r8b, 255
mov [rsp], r8b
mov r9b, 12
mov [rsp+1], r9b
mov rdi, const_1
xor rsi, rsi 
mov sil, [rsp]
sub rsp, 14
call _printf
add rsp, 16
pop rbp
mov rax, 0
ret
section .data
const_1: db "%d", 10, 0
__slash_doubles_array: dq 