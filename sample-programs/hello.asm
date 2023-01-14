section .text
global _main
default rel
extern _printf
extern _strcmp
_main:
push rbp
mov rbp, rsp

ret
section .data
const_1: db `not here`, 10, 0
const_2: db `where I am supposed to be`, 10, 0
const_3: db `not here`, 10, 0