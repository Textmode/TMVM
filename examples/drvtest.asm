LET terminal, 1
LET drvsector, 16
LET drvblock, 17
LET drvbyte, 18
LET drvio, 19

; zero out the selectors
MOV 0, B
MOV drvsector, A
OUT A, B
MOV drvblock, A
OUT A, B
MOV drvbyte, A
OUT A, B

SWP B, D ; set D to zero

MOV terminal, A
MOV drvio, B
SWP A, C
SWP B, D

LBL loop, null
MOV drvbyte, B
OUT B, ACC
IN C, B
OUT D, B
MOV SIG, A
MOV D, B ; reset B
INC ACC
EQL A, B, RET
JMP loop

HLT

