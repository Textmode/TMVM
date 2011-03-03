; set A = 1, B=42
MOV 42, A
SWP A, B
MOV 1, A
; if A!=0 then Move B to &100
MNZ A, B, 100

MOV 0, A
; if A!=0 then Move B to &111
MNZ A, B, 111

; show the results
MOV &100, A
SHW A
MOV &111, A
SHW A

