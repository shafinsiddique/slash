section .text
global _main
default rel
extern _printf
extern _strcmp
_main:
push rbp
mov rbp, rsp
mov rdi, const_1
mov r10, 16
mov rax, rsp
mov rdx, 0
div r10
sub rsp, rdx
sub rsp, 16
mov [rsp], rdx
sub rsp, 16
mov [rsp+(8*0)], r8
movsd [rsp+(8*1)], xmm0
sub rsp, 16
mov r8, [rsp+(8*2)]
movsd xmm0, [rsp+(8*3)]
mov rax, 0
call _printf
add rsp, 32
mov rdx, [rsp]
add rsp, 16
add rsp, rdx
pop rbp
mov rax, 0
ret
section .data
const_1: db "hello World", 10, 0
__slash_doubles_array: dq