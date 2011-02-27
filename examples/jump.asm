;###################################################
;##  A program to demonstrate jumping to a label  ##
;###################################################
MOV 42, A
JMP dest			; Even though the label hasn't been seen yet, it may be referenced

; if the jump fails or misaims, we'll halt here.
HLT
HLT
HLT
HLT
HLT
HLT
HLT
HLT

; and heres our label, it sets both the position
; and segment in seperate symbols. we can ommit the latter if we don't care.
LBL dest, destseg

; show 42, to prove this worked.
SHW A

; finally, halt like the selfish bastards we are.
HLT

