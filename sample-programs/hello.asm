section .text
global _main
default rel
extern _printf
extern _strcmp
_main:
push rbp
mov rbp, rsp
sub rsp, 32
movsd xmm0, [__slash_doubles_array + 8 * 1]
movsd [rsp], xmm0
movsd xmm0, [__slash_doubles_array + 8 * 2]
movsd [rsp+8], xmm0
movsd xmm0, [__slash_doubles_array + 8 * 3]
movsd [rsp+8*2], xmm0
movsd xmm0, [__slash_doubles_array]
movsd xmm1, [__slash_doubles_array]
movsd xmm2, [__slash_doubles_array]
movsd xmm3, [__slash_doubles_array]
movsd xmm4, [__slash_doubles_array]
movsd xmm5, [__slash_doubles_array]
movsd xmm6, [__slash_doubles_array]
movsd xmm7, [__slash_doubles_array]
mov rdi, __slash_doubles_format
mov rax, 9
call _printf
add rsp, 32
pop rbp
mov rax, 0
ret
section .data
__slash_integer_format: db "%d", 10, 0
__slash_doubles_array: dq 2.0,3.4, 2.9, 2.5
__slash_doubles_format: db "%f %f %f %f %f %f %f %f %f %f %f", 10, 0