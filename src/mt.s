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

Main	jsr Menu_Draw
:menuLoop	jsr Menu_UndrawSelected
	jsr Menu_DrawSelected
	jsr WaitKey
	cmp #$8D	; ENTER
	bne :check1
:enter	jsr Menu_HandleSelection
	bra :menuLoop

:check1	cmp #$8B	; UP
	bne :check2
	jsr Menu_PrevItem
	bra :menuLoop

:check2	cmp #$8A	; DOWN
	bne :noKey
	jsr Menu_NextItem
	bra :menuLoop

:noKey	bra :menuLoop
* LOOOOOOOOOP ^^^^^^







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
	pha
	jsr PRHEX
	pla
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


Menu_Draw	jsr HOME
	lda #MainMenuStrs
	ldy #>MainMenuStrs
	ldx #05	; horiz pos
	jsr PrintStringsX
	lda #MainMenuDefs
	ldy #>MainMenuDefs
	jsr Menu_DrawOptions
	rts

BeginTest	stz _testIteration
	stz _testIteration+1
	ldx #23
	ldy #10
	jsr GoXY
	lda #Mesg_Writing
	ldy #>Mesg_Writing
	jsr PrintString

	clc	; WRITE START
	xce
	rep $10	; long x, short a
	lda StartBank
	sta CurBank
	ldy #0	; update interval counter
:bankloop	lda CurBank
	sta :bankstore+3
	ldx StartAddr
	lda TestValue 
:bankstore	stal $000000,x
	cpx EndAddr
	beq :donebank
	inx
	iny
	cpy #UpdateScanInterval
	bcc :bankstore
	jsr PrintTestCurrent
	ldy #0
	bra :bankstore
:donebank	
	ldy #0	; because i'm anal.. this makes counter align
	inc CurBank
	lda EndBank
	cmp CurBank
	bcs :bankloop
	jsr PrintTestCurrent ; print final score ;)
	sep $10
	sec
	xce	; WRITE END
* TODO DO PAUSE
* TODO DO READ
	ldx #23
	ldy #10
	jsr GoXY
	lda #Mesg_WritingB
	ldy #>Mesg_WritingB
	jsr PrintString
	rts
_testIteration	ds 8
UpdateScanInterval  equ #$0800
Mesg_Writing	asc "Writing: ",00
Mesg_WritingB	asc "                 ",00
	mx %01
PrintTestCurrent	pha
	phy
	stx _stash	; save real X
	sec
	xce
	ldx #33
	ldy #10
	jsr GoXY
	lda CurBank
	jsr PRBYTE
	lda #"/"
	jsr COUT
	lda _stash+1
	jsr PRBYTE
	lda _stash
	jsr PRBYTE

	clc
	xce
	rep $10
	ldx _stash
	ply
	pla
	rts
	mx %11



* DEFAULTS
StartBank	db  #$06
EndBank	db  #$7F
CurBank	db  #0
StartAddr	dw  #$0000
EndAddr	dw  #$FFFF
TestValue	dw  #$00
TestDelay	dw  #$05

Menu_DrawOptions	sta $0
	sty $1
	stz _menuOptionPtr
:drawOption
	ldy _menuOptionPtr
	lda ($0),y
	beq :menuDone
	tax
	iny
	lda ($0),y
	tay
	jsr GoXY
	ldy _menuOptionPtr
	iny
	iny
	lda ($0),y
	beq :charItem
	cmp #1
	beq :hexItem
	cmp #2
	beq :jsrItem
:charItem	
:hexItem	iny
	lda ($0),y	; get len
	sta _menuHexIdx
	iny	
	lda ($0),y	; get da
	sta $2	; storez
	iny	
	lda ($0),y	; get da
	sta $3	; storez
	ldy #0
:prloop	lda ($2),y
	jsr PRBYTE
	iny
	cpy _menuHexIdx
	bne :prloop
	bra :nextMenuItem
:jsrItem
	iny
	iny
	lda ($0),y
	tax
	iny
	lda ($0),y
	tay

	lda $0	; whoops.. save zp ptrs that printstring uses :(
	pha
	lda $1
	pha

	txa
	jsr PrintString
	pla
	sta $1
	pla
	sta $0

:nextMenuItem
	lda _menuOptionPtr
	clc
	adc #6	; len of "struct"
	sta _menuOptionPtr
	bra :drawOption
:menuDone
	rts
_menuHexIdx	dw  0
_menuOptionPtr	dw  00
Menu_UndrawSelected	
	lda #MainMenuDefs
	ldy #>MainMenuDefs
	sta $0
	sty $1
	stz _stash

:undrawLoop	ldy _stash	; struct ptr
	lda ($0),y
	beq :stop
	dec	; x-- (left bracket)
	sta _menuSelectedX1
	iny
	lda ($0),y
	sta _menuSelectedY
	iny 
	lda ($0),y
	bne :notChar
	iny
	lda ($0),y
	inc	;doit
	clc
	adc _menuSelectedX1
	tax
	bra :rightBracket

:notChar	cmp #1
	bne :notHex
	iny 
	lda ($0),y
	asl
	inc	;doit
	clc
	adc _menuSelectedX1
	tax
	bra :rightBracket

:notHex	cmp #2
	bne :wtf
	iny 
	lda ($0),y

	inc
	clc
	adc _menuSelectedX1
	tax
	bra :rightBracket


:wtf

:rightBracket
	ldy _menuSelectedY
	jsr GoXY
	lda #" "
	jsr COUT
:leftBracket	ldx _menuSelectedX1
	ldy _menuSelectedY
	jsr GoXY 
	lda #" "
	jsr COUT
	lda _stash
	clc
	adc #6
	sta _stash
	bra :undrawLoop
:stop
	rts
Menu_DrawSelected	
	lda #MainMenuDefs
	ldy #>MainMenuDefs
	sta $0
	sty $1
	lda #0
	ldx Menu_ItemSelected
:check	beq :foundIdx
	clc
	adc #6	; "struct" size
	dex 
	bra :check

:foundIdx	tay
	lda ($0),y
	dec	; x-- (left bracket)
	sta _menuSelectedX1
	iny
	lda ($0),y
	sta _menuSelectedY
	iny 
	lda ($0),y
	bne :notChar
	iny
	lda ($0),y
	inc	;doit
	clc
	adc _menuSelectedX1
	tax
	bra :rightBracket

:notChar	cmp #1
	bne :notHex
	iny 
	lda ($0),y
	asl
	inc	;doit
	clc
	adc _menuSelectedX1
	tax
	bra :rightBracket

:notHex	cmp #2
	bne :wtf
	iny 
	lda ($0),y

	inc
	clc
	adc _menuSelectedX1
	tax
	bra :rightBracket


:wtf

:rightBracket
	ldy _menuSelectedY
	jsr GoXY
	lda #"]"
	jsr COUT
:leftBracket	ldx _menuSelectedX1
	ldy _menuSelectedY
	jsr GoXY 
	lda #"["
	jsr COUT

	rts
_menuSelectedX1	db 0	; no x2 cuz we be addin'
_menuSelectedY	db 0

Menu_HandleSelection 
	lda #MainMenuDefs
	ldy #>MainMenuDefs
	sta $0
	sty $1
	lda #0
	ldx Menu_ItemSelected
:check	beq :foundIdx
	clc
	adc #6	; "struct" size
	dex 
	bra :check
:foundIdx	sta _stash
	tay
	lda ($0),y
	tax
	iny
	lda ($0),y
	tay
	jsr GoXY
*** HERE
	ldy _stash
	iny
	iny
	lda ($0),y
	bne :notChar

*TODO
:notChar	cmp #1
	bne :notHex
	iny
	lda ($0),y
	asl	;*2
	pha
	iny
	lda ($0),y
	pha
	iny
	lda ($0),y
	tay
	plx
	pla
	jsr GetHex
	rts


:notHex	cmp #2
	bne :wtf
	iny	; skip len byte
	iny
	lda ($0),y
	sta :ACTION+1
	iny
	lda ($0),y
	sta :ACTION+2
	lda :ACTION+1
	sec
	sbc #2
	sta :ACTION+1
	bcs :copy
	dec :ACTION+2
:copy	ldx #0	; this is all so bad
:ACTION	lda $ffff,x
	sta :JSR+1,x
	inx
	cpx #2
	bcc :ACTION
:JSR	jsr $ffff
	rts




:wtf
	rts






Menu_PrevItem	dec Menu_ItemSelected
	bpl :noflip
	lda #MainMenuItems
	dec
	sta Menu_ItemSelected
:noflip	rts


Menu_NextItem	
	inc Menu_ItemSelected
	lda Menu_ItemSelected
	cmp #MainMenuItems
	bcc :noflip
	lda #0
	sta Menu_ItemSelected
:noflip	rts


MainMenuDefs
:StartBank	hex 0D,0A ; x,y
	db 01	; 0=char/1=hex input 2=Menu JSR
	db 01	; memory size (bytes), 0=char/1=hex input
	da StartBank	; variable storage 
:EndBank	hex 0D,0B	; x,y
	db 01	; 0=char/1=hex input 2=Menu JSR
	db 01	; memory size (bytes), 0=char/1=hex input
	da EndBank	; variable storage 
:StartAddr	hex 0D,0D	; x,y
	db 01	; 0=char/1=hex input 2=Menu JSR
	db 02	; memory size (bytes), 0=char/1=hex input
	da StartAddr	; variable storage 
:EndAddr	hex 0D,0E	; x,y
	db 01	; 0=char/1=hex input 2=Menu JSR
	db 02	; memory size (bytes), 0=char/1=hex input
	da EndAddr	; variable storage 
:TestByte	hex 0D,10	; x,y
	db 01	; 0=char/1=hex input 2=Menu JSR
	db 01	; memory size (bytes), 0=char/1=hex input
	da TestValue	; variable storage 
:TestDelay	hex 0D,11	; x,y
	db 01	; 0=char/1=hex input 2=Menu JSR
	db 01	; memory size (bytes), 0=char/1=hex input
	da TestDelay	; variable storage 
:BeginTest	hex 0D,13	; x,y
	db 02	; 0=char/1=hex input 2=Menu JSR
	db MenuStr_BeginTestL ; menu string length
	da MenuStr_BeginTest ; string storage 
MainMenuLen	equ *-MainMenuDefs
MainMenuItems	equ MainMenuLen/6
MainMenuEnd	dw 0000
Menu_ItemSelected	db  0



MenuStr_JSR	da BeginTest	; MUST PRECEDE MENU STRING!  Yes, it's magicly inferred. (-2)
MenuStr_BeginTest	asc "BEGIN TEST"
MenuStr_BeginTestL  equ #*-MenuStr_BeginTest
MenuStr_BeginTestE	db 00
MainMenuStrs
	asc "         *********************** ",$8D,$00
	asc "        **                     **",$8D,$00
	asc "        **  Mini Memory Tester **",$8D,$00
	asc "        **    Reactive Micro   **",$8D,$00
	asc "        **        (beta)       **",$8D,$00
	asc "        **                     **",$8D,$00
	asc "         *********************** ",$8D,$00
	asc $8D,$8D,$8D,$00
	asc " Start BANK:  ",$8D,$00
	asc "   End BANK:  ",$8D,$8D,$00
	asc " Start ADDR:  ",$8D,$00
	asc "   End ADDR:  ",$8D,$8D,$00
	asc "  Test Byte:  ",$8D,$00
	asc " Test Delay:  ",$8D,$8D,$8D,$8D,$8D,$00
	asc "         USE ARROW KEYS TO MOVE",8D,$00
	asc "        USE ENTER TO SELECT/EDIT",$00
	
	hex 00,00
	



WaitKey
:kloop	lda KEY
	bpl :kloop
	sta STROBE
	cmp #"b"	; REMOVE DEBUG
	bne :nobreak
	brk $75
:nobreak
	rts

	put strings.s
	ds \
_stash	ds 255
	ds \

