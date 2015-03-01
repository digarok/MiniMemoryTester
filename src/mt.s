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
	ldx #13
	ldy #10
	jsr GoXY
	lda #2
	ldx #>StartBank
	ldy #StartBank
	jsr GetHex
	brk $99
	lda StartBank
	rts

* Pass desired length in A
GetHex	sta _gethex_maxlen
	stx _gethex_resultptr
	sty _gethex_resultptr+1
	stz _gethex_current

:input	jsr RDKEY
	cmp #"9"+1
	bcs :notNum	;bge > 9
	cmp #"0"
	bcc :badChar	;
	sec
	sbc #"0"
	bra :storeInput
:notNum	cmp #"a"
	bcc :notLower
	sec
	sbc #$20	; ToUpper
:notLower	cmp #"A"
	bcc :badChar
	cmp #"F"+1
	bcs :badChar
:gotHex	
	sec
	sbc #"A"-10
:storeInput
	jsr PRHEX
	ldy _gethex_current
	sta _gethex_buffer,y
	iny
	cpy #_gethex_internalmax
	bge :internalmax
	cpy _gethex_maxlen
	bge :passedmax
	sty _gethex_current
	bra :input
:internalmax
:passedmax
	lda _gethex_resultptr
	sta $0
	lda _gethex_resultptr+1
	sta $1
	ldx #0
	ldy #0
:copyBuffer	lda _gethex_buffer,x
	asl	; move to upper nibble
	asl
	asl
	asl
	sta ($0),y	; store
	inx
	lda _gethex_buffer,x
	ora ($0),y
	sta ($0),y
	iny
	inx
	cpx _gethex_maxlen
	bcc :copyBuffer
	rts

:badChar	bra :input

_gethex_internalmax equ 8
_gethex_resultptr   da 0000
_gethex_maxlen	db 1
_gethex_current	db 0
_gethex_buffer	ds _gethex_internalmax 
PrHexChar	jsr HexCharForByte

HexCharForByte
	cmp #9
	bcs :alpha
:number	clc
	adc #"0"
	rts
:alpha	clc
	adc #"A"
	rts

GetThing	ldx #13
	ldy #10
	jsr GoXY
	lda #"$"
	sta $33
	jsr GETLN
	rts

DrawMenu	jsr HOME
	lda #MainMenuStrs
	ldy #>MainMenuStrs
	ldx #05	; horiz pos
	jsr PrintStringsX
	lda #MainMenuDefs
	ldy #>MainMenuDefs
	jsr DrawMenuOptions
	rts

BeginTest	brk $ff

* x=x y=y a=len
MenuHighlight

* DEFAULTS
StartBank	db  #$02
EndBank	db  #$7F
StartAddr	dw  #$0000
EndAddr	dw  #$FFFF
TestValue	dw  #$00

DrawMenuOptions	sta $0
	sty $1
	stz _menuOptionPtr
:drawOption
	ldy _menuOptionPtr
	lda ($0),y
	tax
	lda ($0),y
	tay
	jsr GoXY
	ldy _menuOptionPtr
	iny
	iny
	iny
	lda ($0),y
	beq :charItem
	cmp #1
	beq :hexItem
	cmp #2
	beq :jsrItem
:charItem
:hexItem
:jsrItem	
	rts



_menuOptionPtr	dw  00
MainMenuDefs
Menu_StartBank	hex 10,10 ; x,y
	hex 01,01   ; memory size (bytes), 0=char/1=hex input
	da StartBank ; variable storage 

Menu_EndBank	hex 10,11 ; x,y
	hex 01,01   ; memory size (bytes), 0=char/1=hex input
	da EndBank ; variable storage 
Menu_StartAddr	hex 10,13 ; x,y
	hex 02,01   ; memory size (bytes), 0=char/1=hex input
	da StartAddr ; variable storage 
Menu_EndAddr	hex 10,14 ; x,y
	hex 02,01   ; memory size (bytes), 0=char/1=hex input
	da EndAddr ; variable storage 
Menu_BeginTest	hex 12,15 ; x,y
	db MenuStr_BeginTestL ; menu string length
	db 02   ; memory size (bytes), 2=Menu JSR
	da MenuStr_BeginTest ; string storage 



MenuStr_JSR	da BeginTest	; MUST PRECEDE MENU STRING!  Yes, it's magicly inferred. (-2)
MenuStr_BeginTest	asc "BEGIN TEST"
MenuStr_BeginTestL  equ #*-MenuStr_BeginTest

MainMenuStrs
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
	asc "  Test Byte:    (Leave empty = random)",$8D,$8D,$8D,$00
	asc "        USE ARROW KEYS TO MOVE",8D,$00
	asc "       USE ENTER TO SELECT/EDIT",$8D,$00
	
	hex 00,00
	



WaitKey
:kloop	lda KEY
	bpl :kloop
	sta STROBE
	rts

	put strings.s

