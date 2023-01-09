section .text
global _main
default rel
extern _printf
extern _strcmp
_main:
push rbp
mov rbp, rsp
mov r8b, 1
sub rsp, 1
mov [rbp-1], r8b
mov rdi, const_1
xor rsi, rsi
mov sil, [rbp-1]
add rsp, 0
push rsi
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
sub rsp, 16
mov rdx, r11
mov r8, [rsp+(8*2)]
movsd xmm0, [rsp+(8*3)]
mov rax, 0
call _printf
add rsp, 32
pop rdx
add rsp, 8
add rsp, rdx
mov r8, const_2
sub rsp, 8
mov [rbp-9], r8
mov rdi, const_3
xor rsi, rsi
mov rsi, [rbp-9]
add rsp, 0
push rsi
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
sub rsp, 16
mov rdx, r11
mov r8, [rsp+(8*2)]
movsd xmm0, [rsp+(8*3)]
mov rax, 0
call _printf
add rsp, 32
pop rdx
add rsp, 8
add rsp, rdx
add rsp, 9
pop rbp
mov rax, 0
ret
section .data
const_1: db "%d", 10, 0
const_2: db "Hello World", 10, 0
const_3: db "%s", 10, 0
__slash_doubles_array: dq 