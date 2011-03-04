;; That esteemed classic of computing, 'Hello, world!'

MOV 1, A ; set the device port, the terminal is device 1
SWP A, B	; swap it into B

LBL start, null
MOV 'H', A    ; Move a letter into A, and...
OUT B, A	     ; Push it to the port
MOV 'e', A    ; Rince and...
OUT B, A      ; Repeat!
MOV 'l', A
OUT B, A
OUT B, A
MOV 'o', A
OUT B, A
MOV $2c, A    ; Comma
OUT B, A
MOV $20, A    ; Space
OUT B, A
MOV 'w', A
OUT B, A
MOV 'o', A
OUT B, A
MOV 'r', A
OUT B, A
MOV 'l', A
OUT B, A
MOV 'd', A
OUT B, A
MOV $21, A    ; Exclamation!
OUT B, A
MOV $0d, A    ; Carrage return
OUT B, A
MOV $0a, A    ; Linefeed.
OUT B, A

HLT           ; and finally halt the machine, since we are bastards.

