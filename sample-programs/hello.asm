section .text
global _main
default rel
extern _printf
extern _strcmp
_main:
push rbp
mov rbp, rsp
mov rdi, const_1
mov rsi, 3
push rsi
mov rdx, 4
push rdx
mov rcx, 5
push rcx
pop rcx
pop rdx
pop rsi
mov r11, rdx
mov r10, 16
mov rax, rsp
mov rdx, 0
div r10
sub rsp, rdx
sub rsp, 32
mov [rsp+(8*0)], r8
movsd [rsp+(8*1)], xmm0
mov [rsp+(8*2)], rdx
mov rdx, r11
sub rsp, 16
mov r8, [rsp+(8*2)]
movsd xmm0, [rsp+(8*3)]
mov rax, 0
call _printf
add rsp, 32
pop rdx
add rsp, 8
add rsp, rdx
pop rbp
mov rax, 0
ret
section .data
const_1: db "%d %d %d", 10, 0
__slash_doubles_array: dq 