;; A test/example of long-moving data, and jumppads
;; Licence: CC0


LET helloseg, 1

LBL init, initseg
MOV data, A
MOV A, ACC
LBL initloop, null
MOV helloseg, B
LMOV SEG, A, B, A
MOV dataend, B
MOV ACC, A
LTE A, B, RET
INC ACC
JNZ RET, initloop

;; okay, lets do this
MOV helloseg, A
MOV 0, B
LJMP A, B
LBL return, retseg

; print ' again'
MOV 1, A         ; Stream device port.
MOV $20, B ; space
OUT A, B
MOV 'a', B
OUT A, B
MOV 'g', B
OUT A, B
MOV 'a', B
OUT A, B
MOV 'i', B
OUT A, B
MOV 'n', B
OUT A, B

; CR, LF :3
MOV  $d, B
OUT A, B
MOV $a, B
OUT A, B

HLT

; data
LBL data, null
; print 'hello'
MOV 1, A         ; Stream device port.
MOV 'h', B
OUT A, B
MOV 'e', B
OUT A, B
MOV 'l', B
OUT A, B
OUT A, B
MOV 'o', B
OUT A, B
MOV retseg, A
MOV return, B
LJMP A, B
HLT
LBL dataend, null

