****************************************
* MemoryTester                         *
*                                      *
*  Dagen Brock <dagenbrock@gmail.com>  *
*  2015-02-23                          *
****************************************

	org $2000	; start at $2000 (all ProDOS8 system files)
	typ $ff	; set P8 type ($ff = "SYS") for output file
	dsk mtsystem ; tell compiler what name for output file
	put applerom


MLI	equ $bf00

	jsr DrawMenu
	jsr GetStartBank
	jsr GetThing
	jsr WaitKey


Quit	jsr MLI	; first actual command, call ProDOS vector
	dfb $65	; with "quit" request ($65)
	da QuitParm
	bcs Error
	brk $00	; shouldn't ever  here!

QuitParm	dfb 4	; number of parameters
	dfb 0	; standard quit type
	da $0000	; not needed when using standard quit
	dfb 0	; not used
	da $0000	; not used


Error	brk $00	; shouldn't be here either
GetStartBank	
	lda #13
	sta $24
	lda #10
	sta $25
	jsr VTAB
	lda #"$"
	sta $33
	jsr GetHex
	jsr RDKEY
	jsr PRBYTE
	rts

* Pass desired length in A
GetHex	sta _gethex_maxlen
:input	jsr RDKEY
	cmp #"9"+1
	bcs :notNum	;bge > 9
	cmp #"0"
	bcc :badChar	;
	sec
	sbc #"0"
	bra :gotHex
:notNum	cmp #"a"
	bcc :notLower
	sec
	sbc #$20	; ToUpper
:notLower	cmp #"A"
	bcc :badChar
	cmp #"F"+1
	bcs :badChar
:gotHex	jsr PRHEX
	rts

:badChar	bra :input


_gethex_maxlen	db 1

GetThing	lda #13
	sta $24
	lda #10
	sta $25
	jsr VTAB
	lda #"$"
	sta $33
	jsr GETLN
	rts

DrawMenu	jsr HOME
	lda #MenuStrs
	ldy #>MenuStrs
	ldx #05	; horiz pos
	jsr PrintStringsX
	rts

;	lda #MenuStr1
;	ldy #>MenuStr1
;	jsr PrintString

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

MenuStrs
	asc "        *********************** ",$8D,$00
	asc "       **                     **",$8D,$00
	asc "       **  Mini Memory Tester **",$8D,$00
	asc "       **    Reactive Micro   **",$8D,$00
	asc "       **        (beta)       **",$8D,$00
	asc "       **                     **",$8D,$00
	asc "        *********************** ",$8D,$00
	asc $8D,$8D,$8D,$00
	asc " Start BANK:  ",$8D,$00
	asc "   End BANK:  ",$8D,$8D,$00
	asc " Start ADDR:  ",$8D,$00
	asc "   End ADDR:  ",$8D,$8D,$8D,$00
	asc "  Test Byte:     (Leave empty = random)",$8D,00

	hex 00,00
	



WaitKey
:kloop	lda KEY
	bpl :kloop
	sta STROBE
	rts

* DEFAULTS
StartBank	db  #$02
EndBank	db  #$7F
StartAddr	dw  #$0000
EndAddr	dw  #$FFFF
TestValue	dw  #$00

