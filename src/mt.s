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
Init
	LDA #$A0           ;USE A BLANK SPACE TO
	JSR $C300          ;TURN ON THE VIDEO FIRMWARE

	lda $C034	; save border color
	sta BorderColor
	
Main	
:menuLoop	jsr Menu_Draw
:menuNoDrawLoop	jsr Menu_UndrawSelected
	jsr Menu_DrawSelected
	jsr WaitKey
	cmp #$8D	; ENTER
	bne :check1
:enter	jsr Menu_HandleSelection
	bra :menuLoop

:check1	cmp #$8B	; UP
	bne :check2
	jsr Menu_PrevItem
	bra :menuNoDrawLoop

:check2	cmp #$8A	; DOWN
	bne :noKey
	jsr Menu_NextItem
	bra :menuNoDrawLoop

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
	cmp #$9B	;esc = abort
	bne :notesc
	rts
:notesc	cmp #"9"+1
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
	ldx #00	; horiz pos
	jsr PrintStringsX
	lda #MainMenuDefs
	ldy #>MainMenuDefs
	jsr Menu_DrawOptions
	rts

BeginTest	stz _testIteration
	stz _testIteration+1
	ldx #40
	ldy #07
	lda #5
	jsr PrintBox30
BeginTestPass	PRINTXY  #44;#08;Mesg_TestPass
	inc _testIteration
	bne :noroll
	inc _testIteration+1
:noroll	lda _testIteration+1
	jsr PRBYTE
	lda _testIteration
	jsr PRBYTE
	PRINTXY  #44;#10;Mesg_Writing

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
	dec CurBank	; so many bad hacks
	jsr PrintTestCurrent ; print final score ;)
	sep $10
	sec
	xce	; WRITE END

	jsr Pauser	; PAUSE

	PRINTXY  #44;#10;Mesg_Reading ; READ PREP

	clc	; READ START
	xce
	rep $10	; long x, short a
	lda StartBank
	sta CurBank
	ldy #0	; update interval counter
:bankrloop	lda CurBank
	sta :bankread+3
	ldx StartAddr
:bankread	ldal $000000,x
	cmp TestValue 
	beq :testpass
	phx
	sta _stash	; = read value
	lda TestValue
	sta _stash+1	; = expected value
	stx _stash+2
	jsr PrintTestError	; addr in X
	plx
:testpass	cpx EndAddr
	beq :donerbank
	inx
	iny
	cpy #UpdateScanInterval
	bcc :bankread
	jsr PrintTestCurrent
	ldy #0
	bra :bankread
:donerbank	
	ldy #0	; because i'm anal.. this makes counter align
	inc CurBank
	lda EndBank
	cmp CurBank
	bcs :bankrloop
	dec CurBank	; so many bad hacks
	jsr PrintTestCurrent ; print final score ;)
	sep $10
	sec
	xce	; WRITE END


	jsr Pauser	; PAUSE
	lda BorderColor
	sta $C034
	jmp BeginTestPass

	rts

_testIteration	ds 8
UpdateScanInterval  equ #$3000
Mesg_Waiting	asc "Waiting: ",00
Mesg_Writing	asc "Writing: ",00
Mesg_Reading	asc "Reading: ",00
Mesg_Error1	asc " Error at: $",00
Mesg_Error2	asc " Expected: $   %",$00
Mesg_Error3	asc "     Read: $   %",$00
Mesg_TestPass	asc "   Pass:  ",00
Mesg_Blank	asc "                 ",00
Mesg_BoxTop30	asc $1B,'ZLLLLLLLLLLLLLLLLLLLLLLLLLLLL_',$18,$8D,00
Mesg_BoxMid30	asc $1B,'Z',"                            ",'_',$18,$8D,$00
Mesg_BoxBot30	asc $1B,'Z',"____________________________",'_',$18,$8D,$00

* x, y, a=height
PrintBox30	stx _prbox_x
	sta _prbox_height
	jsr GoXY
	lda #Mesg_BoxTop30
	ldy #>Mesg_BoxTop30
	jsr PrintString
:midloop	ldx _prbox_x
	stx $24
	lda #Mesg_BoxMid30
	ldy #>Mesg_BoxMid30
	jsr PrintString
	dec _prbox_height
	bne :midloop

	ldx _prbox_x
	stx $24
	lda #Mesg_BoxBot30
	ldy #>Mesg_BoxBot30
	jsr PrintString
	rts
_prbox_x	db 0
_prbox_height	db 0
	


PrintTestError	
	sec
	xce
	ldx #42
	ldy #13
	lda #4
	jsr PrintBox30
	PRINTXY #45;#14;#Mesg_Error1
	PRINTXY #45;#16;#Mesg_Error2
	PRINTXY #45;#17;#Mesg_Error3
	GOXY #57;#14
	lda CurBank
	jsr PRBYTE
	lda #"/"
	jsr COUT
	lda _stash+3
	jsr PRBYTE
	lda _stash+2
	jsr PRBYTE
	GOXY #57;#16
	lda _stash+1
	jsr PRBYTE
	GOXY #61;#16
	lda _stash+1
	jsr PRBIN
	GOXY #57;#17
	lda _stash
	jsr PRBYTE
	GOXY #61;#17
	lda _stash
	jsr PRBIN
	GOXY #66;#14
	jsr RDKEY

	clc
	xce
	rep $10
	rts
	mx %01
PrintTestCurrent	pha
	phy
	stx _stash	; save real X
	sec
	xce
	GOXY #54;#10
	lda CurBank
	sta :corruptme+3
	jsr PRBYTE
	lda #"/"
	jsr COUT
	lda _stash+1
	sta :corruptme+2
	jsr PRBYTE
	lda _stash
	sta :corruptme+1
	jsr PRBYTE
* CORRUPTOR!
:kloop	lda KEY
	cmp #"c"	; REMOVE DEBUG
	bne :nocorrupt
	jsr GetRandTrash
:corruptme	stal $060000
	inc $c034
	sta STROBE	; we only clear if 'c' is hit
	inc _stash	; \
	beq :noroll	;  |- INX
	inc _stash+1        ; /
:nocorrupt	cmp #"p"	; check lower p
* @TODO make tolower for the comparisons
	bne :nopause
	sta STROBE
:nope	lda KEY
	bpl :nope
	sta STROBE
:nopause
:noroll
	clc
	xce
	rep $10
	ldx _stash
	ply
	pla
	rts
	mx %11
PRBIN	pha
	phx
	ldx #8
:loop	asl
	pha
	bcc :zero
:one	lda #"1"
	jsr COUT
	bra :ok
:zero	lda #"0"
	jsr COUT
:ok	pla
	dex
	bne :loop
	plx
	pla
	rts

Pauser
	PRINTXY #44;#11;Mesg_Waiting
	ldy #60
	ldx TestDelay
	beq :nopauseforyou
	jsr PrintTimerVal	; inaugural print before waiting 1 sec
:secondloop
:wait	ldal $e1c019
	bpl :wait
:wait2	ldal $e1c019
	bmi :wait2
	dey
	bne :secondloop
	dex
	beq :donepause
	jsr PrintTimerVal
	ldy #60
	bra :secondloop
:donepause	
	PRINTXY #44;#11;Mesg_Blank
:nopauseforyou
	rts
PrintTimerVal
	phx
	phy
	txa 
	GOXY #54;#11
	ply
	plx
	txa
	jsr PRBYTE
	rts

**************************************************
* Awesome PRNG thx to White Flame (aka David Holz)
**************************************************
GetRandTrash		; USE ONLY WITH CORRUPTOR
	lda _randomTrashByte
	beq :doEor
	asl
	bcc :noEor
:doEor	eor #$1d
:noEor	sta _randomTrashByte
	rts
_randomTrashByte	db 0

* DEFAULTS
StartBank	db  #$06
EndBank	db  #$1F
CurBank	db  #0
StartAddr	dw  #$0000
EndAddr	dw  #$FFFF
TestValue	dw  #$00
TestDelay	dw  #$03

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
	cmp #3
	beq :listItem
	cmp #4
	beq :boolItem
:charItem	
:boolItem	
:hexItem	jsr Menu_DrawOptionHex
	bra :nextMenuItem
:listItem	jsr Menu_DrawOptionList
	bra :nextMenuItem
:jsrItem	jsr Menu_DrawOptionAction
	bra :nextMenuItem

:nextMenuItem
	lda _menuOptionPtr
	clc
	adc #6	; len of "struct"
	sta _menuOptionPtr
	bra :drawOption
:menuDone
	rts

Menu_DrawOptionHex 	iny
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
	rts

Menu_DrawOptionAction iny
	iny
	lda ($0),y
	tax
	iny
	lda ($0),y
	tay
	txa
	jsr PrintString
	rts

Menu_DrawOptionList iny	; point to da
	iny
	lda ($0),y
	sta $2
	iny
	lda ($0),y
	sta $3	; now ($2) points to item list structure
	ldy #0
	lda ($2),y	; selected index
	asl
	inc
	inc	; add 2 to reach table of addresses
	tay
	lda ($2),y
	pha

	iny
	lda ($2),y
	tay
	pla
	jsr PrintString
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
	bne :notAction
	iny 
	lda ($0),y

	inc
	clc
	adc _menuSelectedX1
	tax
	bra :rightBracket


:notAction
	cmp #3
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
	bne :notAction
	iny 
	lda ($0),y

	inc
	clc
	adc _menuSelectedX1
	tax
	bra :rightBracket


:notAction
	cmp #3
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
_menuSelectedX1	db 0	; no x2 cuz we be addin' dat offset
_menuSelectedY	db 0

MenuOption_Char	equ #0
MenuOption_Hex	equ #1
MenuOption_Action	equ #2
MenuOption_List	equ #3
MenuOption_Bool	equ #4
Menu_TypeTable	da  Menu_TypeChar,Menu_TypeHex,Menu_TypeAction,Menu_TypeList,Menu_TypeBool

* $0 = ptr->MenuDefs
Menu_HandleSelection 
	lda #MainMenuDefs
	ldy #>MainMenuDefs
	sta $0
	sty $1
	lda #0
	ldx Menu_ItemSelected ; odd choice to load again, but preps flags (z) how i likes it
:check	beq :foundIdx	; <-  a=struct offset
	clc
	adc #6	; "struct" size
	dex
	bra :check

:foundIdx	pha
	tay
	iny	;\ 
	iny	; \  
	lda ($0),y	;  > get MenuOption_Type, set up for jmp table
	asl	; /
	tax	;/ 
	pla
	jmp (Menu_TypeTable,x)
	
Menu_TypeChar	rts
Menu_TypeBool	rts
	

Menu_TypeHex	pha
	tay
	lda ($0),y
	tax
	iny
	lda ($0),y
	tay
	jsr GoXY
	pla
	clc
	adc #3	; ->memory size
	tay
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

Menu_TypeAction	iny	; skip len byte
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


Menu_TypeList	rts







Menu_PrevItem	dec Menu_ItemSelected
	bpl :noflip
	lda #MainMenuItems
	dec
	sta Menu_ItemSelected
:noflip	rts


Menu_NextItem	inc Menu_ItemSelected
	lda Menu_ItemSelected
	cmp #MainMenuItems
	bcc :noflip
	lda #0
	sta Menu_ItemSelected
:noflip	rts


MainMenuDefs
:StartBank	hex 13,06 ; x,y
	db MenuOption_Hex	; 1=hex input
	db 01	; memory size (bytes)
	da StartBank	; variable storage 
:EndBank	hex 13,07	; x,y
	db MenuOption_Hex	; 1=hex input
	db 01	; memory size (bytes)
	da EndBank	; variable storage 
:StartAddr	hex 13,09	; x,y
	db MenuOption_Hex	; 1=hex input
	db 02	; memory size (bytes)
	da StartAddr	; variable storage 
:EndAddr	hex 13,0A	; x,y
	db MenuOption_Hex	; 1=hex input
	db 02	; memory size (bytes)
	da EndAddr	; variable storage 
:TestType	hex 13,0C	; x,y
	db MenuOption_List	; 3=list input
	db 08	; max len size (bytes), 3=option list
	da TestType	; params definition & storage 
:TestValue	hex 13,0D	; x,y
	db MenuOption_Hex	; 1=hex input
	db 01	; memory size (bytes)
	da TestValue	; variable storage 
:TestDelay	hex 13,0E	; x,y
	db MenuOption_Hex	; 1=hex input
	db 01	; memory size (bytes)
	da TestDelay	; variable storage 
:BeginTest	hex 0B,12	; x,y
	db MenuOption_Action ; 2=action
	db MenuStr_BeginTestL ; menu string length
	da MenuStr_BeginTest ; string storage 
MainMenuLen	equ *-MainMenuDefs
MainMenuItems	equ MainMenuLen/6
MainMenuEnd	dw 0000
Menu_ItemSelected	db  0


* 00 - Byte : Selected Value
* 01 - Byte : Number of values
* 02... - Words : Table of Addresses of possible values
TestType	db 00	; actual CONST val 
	db 06	; number of possible values
	da _TestType_0,_TestType_1,_TestType_2,_TestType_3,_TestType_4,_TestType_5,00,00
_TestType_0	asc "BYTE",$00
_TestType_1	asc "WORD",$00
_TestType_2	asc "RANDBYTE",$00
_TestType_3	asc "RANDWORD",$00
_TestType_4	asc "CHECKERS",$00
_TestType_5	asc "BANK",$00

MenuStr_JSR	da BeginTest	; MUST PRECEDE MENU STRING!  Yes, it's magicly inferred. (-2)
MenuStr_BeginTest	asc "BEGIN TEST"
MenuStr_BeginTestL  equ #*-MenuStr_BeginTest
MenuStr_BeginTestE	db 00
MainMenuStrs	
	asc "  ____________________________________________________________________________",$8D,$00
	asc " ",$1B,'ZGGGGGGGGGGGGGGGGGGGGGGGGGGG\'," Mini Memory Tester ",'\GGGGGGGGGGGGG\'," ALPHA ",'\GGGGG_',$18,$8D,$00
	asc " ",$1B,'ZWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVW'," ReactiveMicro ",'VW_',$18,$8D,00
	asc " ",$1B,'ZLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL_',$18,$8D,00
	asc " ",$1B,'Z',"  ",' \GGG_',"Test Settings",'ZGGG\ ',"          ABCDEFGHIZKLMNOPQRSTUVWXYZ             ",'_',$18,$8D,00
	asc " ",$1B,'Z',"  ",'_',"                       ",'Z',"          ",'ABCDEFGHIZKLMNOPQRSTUVWXYZ',"             ",'_',$18,$8D,00
	asc " ",$1B,'Z',"  ",'_'," Start BANK:           ",'Z',"                                           ",'_',$18,$8D,00
	asc " ",$1B,'Z',"  ",'_',"   End BANK:           ",'Z',"                                           ",'_',$18,$8D,00
	asc " ",$1B,'Z',"  ",'_',"                       ",'Z',"                                           ",'_',$18,$8D,00
	asc " ",$1B,'Z',"  ",'_'," Start ADDR:           ",'Z',"                                           ",'_',$18,$8D,00
	asc " ",$1B,'Z',"  ",'_',"   End ADDR:           ",'Z',"                                           ",'_',$18,$8D,00
	asc " ",$1B,'Z',"  ",'_',"                       ",'Z',"                                           ",'_',$18,$8D,00
	asc " ",$1B,'Z',"  ",'_',"  Test Type:           ",'Z',"                                           ",'_',$18,$8D,00
	asc " ",$1B,'Z',"  ",'_',"  Test Byte:           ",'Z',"                                           ",'_',$18,$8D,00
	asc " ",$1B,'Z',"  ",'_'," Test Delay:           ",'Z',"                                           ",'_',$18,$8D,00
	asc " ",$1B,'Z',"  ",'_',"                       ",'Z',"                                           ",'_',$18,$8D,00
	asc " ",$1B,'Z',"  ",'_',"                       ",'Z',"                                           ",'_',$18,$8D,00
	asc " ",$1B,'Z',"  ",'_',"                       ",'Z',"                                           ",'_',$18,$8D,00
	asc " ",$1B,'Z',"  ",'_',"                       ",'Z',"                                           ",'_',$18,$8D,00
	asc " ",$1B,'Z',"  ",'_',"                       ",'Z',"                                           ",'_',$18,$8D,00
	asc " ",$1B,'Z',"  ",'_',"                       ",'Z',"                                           ",'_',$18,$8D,00
	asc " ",$1B,'Z',"  ",'LLLLLLLLLLLLLLLLLLLLLLLLL',"                                    ",'_',$18,$8D,00
	asc " ",$1B,'Z',"____________________________________________________________________________",'_',$18,$8D,00

*	asc " ",$1B,'Z',"           USE ARROW KEYS TO MOVE  -  USE ENTER TO SELECT/EDIT              ",'_',$18,$8D,00
*	asc "     ABCDEFGHIZKLMNOPQRSTUVWXYZ ",$8D,$00
*	asc $1B,'     ABCDEFGHIZKLMNOPQRSTUVWXYZ ',$1B,$8D,$00
	
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
BorderColor	db 0
	ds \
_stash	ds 255
	ds \

