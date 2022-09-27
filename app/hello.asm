section   .text
global    _main
default rel
extern _printf

_main:
    push rbp
    mov rdi, format
    mov rsi, message
    call _printf 
    pop rbp
    mov rax, 0
    ret
    
section .data
    message:  db "Hello, Shafin", 0
    format:  db "Replacing with String: %s", 0xA

//       + (r1)
//  + (r1)     + (r2)
//2   2      4   3
// evaluation of left side is put in r1.
// So if 2 is an integer, I know it has been put in r1.
// r2
// mov r1 2
// mov r2 2
// add r1, r2, r3 ## store in r3.
// 

// mov r1, 2
// mov r2, 2
// add r1, r2 # Store in r1.
// mov r1, 4
// mov r2, 3
// add r2, r1 # Stroe in r1.
// add r1, r2 # Store in r1

// return assembly.
// p


