* GoXY
* PrintStringsX
* PrintString
 

GoXY	stx $24
	sty $25
	jsr VTAB
	rts

PrintStringsX	stx _printstringsx_horiz	; IGNORED! 4 NOW!

	sta $0
	sty $1
:loop	lda $0	; slower, but allows API reuse
	ldy $1
	jsr PrintString	; y is last val
	iny
	lda ($0),y
	beq :done
	tya	; not done so add strlen to source ptr
	clc
	adc $0
	sta $0
	bcc :nocarry
	inc $1
:nocarry	bra :loop


:done	rts



_printstringsx_horiz db 00

* PrintString (A=Low Byte,  Y=High Byte)
PrintString	sta $0
	sty $1

	ldy #0
:loop	lda ($0),y
	beq :done
	jsr COUT
	iny
	bra :loop
:done	rts

