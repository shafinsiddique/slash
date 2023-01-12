section .text
global _main
default rel
extern _printf
extern _strcmp
_main:
push rbp
mov rbp, rsp
mov r10, 1
cmp r10, 1
jz if_branch_1
jmp else_branch_1
if_branch_1:
mov r10, 0
cmp r10, 1
jz if_branch_2
jmp else_branch_2
if_branch_2:
mov rdi, const_1
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
jmp continue_2
else_branch_2:
mov rdi, const_2
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
jmp continue_2
continue_2:
jmp continue_1
else_branch_1:
mov rdi, const_3
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
jmp continue_1
continue_1:
add rsp, 0
pop rbp
mov rax, 0
ret
section .data
const_1: db `not here`, 10, 0
const_2: db `where I am supposed to be`, 10, 0
const_3: db `not here`, 10, 0