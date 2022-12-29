section .text
global _main
default rel
extern _printf
extern _strcmp
_main:
push rbp
mov rbp, rsp
sub rsp, 16
movups xmm0, [__slash_doubles_array + 8 * 0]
sub rsp, 8
movsd [rsp-(8*0)], xmm0
movups xmm1, [__slash_doubles_array + 8 * 1]
movsd xmm0, [rsp-(8*0)]
add rsp, 8
addsd xmm0, xmm1
movsd xmm0, xmm0
mov rdi, __slash_doubles_format
mov rax, 1
call _printf
add rsp, 16
pop rbp
mov rax, 0
ret
section .data
__slash_integer_format: db "%d", 10, 0
__slash_doubles_array: dq 2.0,3.4
__slash_doubles_format: db "%f", 10, 0