section .text
global _main
default rel
extern _printf
_main:
push rbp
mov r8, 2
push r8
mov r8, 2
push r8
mov r9, 3
pop r8
imul r8, r9
mov r9, r8
pop r8
add r8, r9
mov r8, r8
push r8
mov r9, 8
pop r8
add r8, r9
mov r8, r8
push r8
mov r9, 9
pop r8
add r8, r9
mov r8, r8
mov rdi, numberFormat
mov rsi, r8
call _printf 
pop rbp
mov rax, 0
ret

section .data
    message:  db "Hello, Shafin", 0
    format:  db "Replacing with String: %s", 0xA
    numberFormat: db "Value = %d", 0xA




