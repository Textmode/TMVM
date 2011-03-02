;####################################################
;#      Program to count to ten, using a loop.     ##
;####################################################

; (Re)set the accumulator
MOV 0,A
MOV A,ACC

; put 1 in B
; (since I don't have an increment instruction yet,
;  or a free-register nn mov, we take the scenic route)
MOV 1,A
SWP A,B

; put the accumulated value in A, and put A+B in ACC 
MOV ACC,A
ADD A,B,ACC		; result in ACC

; put 10 in b, and acc in A. again, scenic route
MOV 10,A
SWP A,B
MOV ACC,A

; Set RET to 1 if A < B
LES A,B,RET

; show the result so far with our handy Debugging freind: SHoW
SHW A

; if the test above passed, Set IP to 4 (MOV 1,A)
JNZ RET,4

; Halt the machine, becuase we are selfish bastards who think of no one but ourselves
HLT

