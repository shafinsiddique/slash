section .text
global _main
default rel
extern _printf
extern _strcmp
_main:
push rbp
mov rbp, rsp
sub rsp, 16
mov r10, 2
mov r11, 5
sub r10, r11
mov r10, r10
cmp r10, 0
jz if_branch_1
jmp else_branch_1
if_branch_1:
mov r8, const_1
mov rdi, r8
call _printf
jmp continue_1
else_branch_1:
mov r8, const_2
mov rdi, r8
call _printf
jmp continue_1
continue_1:
add rsp, 16
pop rbp
mov rax, 0
ret
section .data
__slash_integer_format: db "%d", 10, 0
const_1: db "match", 10, 0
const_2: db "unmatch", 10, 0