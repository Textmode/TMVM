;; RTC device test.

LBL loop, null
MOV 42, A       ; RTC device
IN A, B
MOV ACC, A
EQL A, B, RET
JNZ RET, loop
MOV 1, A        ; Stream device
OUT A, B
MOV B, ACC
JMP loop

HLT             ; We should never get here

