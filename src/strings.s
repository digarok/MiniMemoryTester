**** MACROS
* GOXY #x;#y
* PRINTXY #x;#y;StringAddrWord
**** FUNCTIONS
* GoXY
* PrintStringsX
* PrintString
 

GOXY	MAC
	ldx ]1
	ldy ]2
	stx $24
	sty $25
	jsr VTAB
	<<<

PRINTXY	MAC
	ldx ]1
	ldy ]2
	stx $24
	sty $25
	jsr VTAB
	lda #]3
	ldy #>]3
	jsr PrintString
	<<<

GoXY	stx $24
	sty $25
	jsr VTAB
	rts

*	lda #MainMenuStrs
*	ldy #>MainMenuStrs
*	ldx #05	; horiz pos
PrintStringsX	stx _printstringsx_horiz

	sta $0
	sty $1
:loop	lda _printstringsx_horiz
	sta $24
	lda $0	; slower, but allows API reuse
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
PrintString	sta :loop+1
	sty :loop+2

	ldy #0
:loop	lda $FFFF,y	; dummy bytes
	beq :done
	jsr COUT
	iny
	bra :loop
:done	rts

