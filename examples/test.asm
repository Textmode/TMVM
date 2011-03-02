; b = 0
MOV 0, A
SWP A, B

; a = 1
MOV 1, A

; RET = A == B
EQL A, B

; Show RET
MOV RET, A
SHW A

; Halt the machine.
HLT

