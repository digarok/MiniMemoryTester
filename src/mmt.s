****************************************
* MiniMemoryTester                     *
*                                      *
*  Dagen Brock <dagenbrock@gmail.com>  *
*  2015-09-16                          *
****************************************

                    org          $2000                  ; start at $2000 (all ProDOS8 system files)
                    typ          $ff                    ; set P8 type ($ff = "SYS") for output file
                    dsk          mtsystem               ; tell compiler what name for output file
                    put          applerom

MLI                 equ          $bf00
Init
                    sei                                 ; disable interrupts
                    LDA          #$A0                   ;USE A BLANK SPACE TO
                    JSR          $C300                  ;TURN ON THE VIDEO FIRMWARE

                    lda          $C034                  ; save border color
                    sta          BorderColor



                    lda          #MainMenuDefs
                    ldx          #>MainMenuDefs
                    jsr          Menu_InitMenu
Main
:menuLoop           jsr          DrawMenuBackground
                    jsr          DrawRomMessage

:menuNoDrawLoop     jsr          MenuCheckKeyColor
                    bcc          :menuNoDrawLoop        ;hmm?
:keyHit             cmp          #$8D                   ; ENTER
                    bne          :check1
:enter              jsr          Menu_HandleSelection
                    bra          :menuLoop
:check1             cmp          #$8B                   ; UP
                    bne          :check2
                    jsr          Menu_PrevItem
                    bra          :menuNoDrawLoop
:check2             cmp          #$8A                   ; DOWN
                    bne          :noKey
                    jsr          Menu_NextItem
                    bra          :menuNoDrawLoop
:noKey              bra          :menuLoop
* LOOOOOOOOOP ^^^^^^

ColorizeMenu
:loop
                    lda          #$07
                    jsr          WaitSCB
                    lda          #$c0                   ; green
                    sta          $c022

                    lda          #$09
                    jsr          WaitSCB
                    lda          #$d0                   ; yello
                    sta          $c022

                    lda          #$0A
                    jsr          WaitSCB
                    lda          #$90                   ; orange
                    sta          $c022


                    lda          #$0B
                    jsr          WaitSCB
                    lda          #$10                   ; red
                    sta          $c022

                    lda          #$0C
                    jsr          WaitSCB
                    lda          #$30                   ; purple
                    sta          $c022

                    lda          #$0D
                    jsr          WaitSCB
                    lda          #$70                   ; bblue
                    sta          $c022

                    lda          #$0E
                    jsr          WaitSCB
                    lda          #$f0                   ; white
                    sta          $c022
                    rts

WaitSCB
                    sta          :val+1
                    ldx          #2                     ; to check twice
:waitloop           lda          $c02f
                    asl
                    lda          $c02e
                    rol
:val                cmp          #$00
                    bne          :waitloop
                    dex
                    bne          :waitloop
                                                        ; the problem is we can get the LAST
                                                        ; horizcnt even/odd right as it changes
                                                        ; and start early or something?

                    rts
MAXSCB              db           0

DrawMenuBackground  jsr          HOME
                    lda          #MainMenuStrs
                    ldy          #>MainMenuStrs
                    ldx          #00                    ; horiz pos
                    jsr          PrintStringsX
*                    lda          #MainMenuDefs
*                    ldy          #>MainMenuDefs
*                jsr          Menu_DrawOptions
                    rts


DrawRomMessage
                    PRINTXY      #55;#05;Mesg_Rom
                    lda          GSROM
                    jsr          PRBYTE
                    rts

LOG                 MAC
                    lda          #]1
                    ldy          #>]1
                    jsr          ConsoleLog
                    <<<


* Write out to console window
ConsoleLog          pha
                    phy
                    jsr          WinConsole
                    lda          #0                     ;settings to bottom-left of window
                    sta          $24
                    lda          #20
                    sta          $25
                    jsr          VTAB
                    lda          #$8D                   ;pre-fix CR
                    jsr          COUT
                    ply
                    pla
                    jsr          PrintString
                    jsr          WinFull
                    rts

* Set console windowing
WinConsole          lda          #52
                    sta          $20                    ;left edge
                    lda          #26
                    sta          $21                    ;width
                    lda          #5
                    sta          $22                    ;top edge
                    lda          #16
                    sta          $23                    ;bottom edge
                    rts

* Set info windowing
WinInfo             lda          #52
                    sta          $20                    ;left edge
                    lda          #26
                    sta          $21                    ;width
                    lda          #5
                    sta          $22                    ;top edge
                    lda          #16
                    sta          $23                    ;bottom edge
                    rts

* Restore full screen windowing
WinFull             stz          $20
                    stz          $22
                    lda          #80
                    sta          $21
                    lda          #24
                    sta          $23
                    rts









Quit                jsr          MLI                    ; first actual command, call ProDOS vector
                    dfb          $65                    ; with "quit" request ($65)
                    da           QuitParm
                    bcs          Error
                    brk          $00                    ; shouldn't ever  here!

QuitParm            dfb          4                      ; number of parameters
                    dfb          0                      ; standard quit type
                    da           $0000                  ; not needed when using standard quit
                    dfb          0                      ; not used
                    da           $0000                  ; not used


Error               brk          $00                    ; shouldn't be here either


BeginTest           LOG          Mesg_Starting
                    stz          _errorCounter
                    stz          _testIteration
                    stz          _testIteration+1
                    ldx          #36
                    ldy          #04
                    lda          #5
                    jsr          PrintBox30


BeginTestPass       PRINTXY      #38;#05;Mesg_TestPass
                    inc          _testIteration
                    bne          :noroll
                    inc          _testIteration+1
:noroll             lda          _testIteration+1
                    ldx          _testIteration
                    jsr          PRNTAX
                    PRINTXY      #38;#7;Mesg_Writing

                    clc                                 ; WRITE START
                    xce
                    rep          $10                    ; long x, short a
                    lda          StartBank
                    sta          CurBank
                    ldy          #0                     ; update interval counter
:bankloop           lda          CurBank
                    sta          :bankstore+3
                    ldx          StartAddr
                    lda          HexPattern
:bankstore          stal         $000000,x
                    cpx          EndAddr
                    beq          :donebank
                    inx
                    iny
                    cpy          #UpdateScanInterval
                    bcc          :bankstore
                    jsr          PrintTestCurrent
                    bcc          :noquit1
                    jmp          :escpressed
:noquit1            ldy          #0
                    bra          :bankstore
:donebank
                    ldy          #0                     ; because i'm anal.. this makes counter align
                    inc          CurBank
                    lda          EndBank
                    cmp          CurBank
                    bcs          :bankloop
                    dec          CurBank                ; so many bad hacks
                    jsr          PrintTestCurrent       ; print final score ;)
                    bcc          :noquit2
                    jmp          :escpressed
:noquit2            sep          $10
                    sec
                    xce                                 ; WRITE END

                    jsr          Pauser                 ; PAUSE

                    PRINTXY      #38;#7;Mesg_Reading    ; READ PREP

                    clc                                 ; READ START
                    xce
                    rep          $10                    ; long x, short a
                    lda          StartBank
                    sta          CurBank
                    ldy          #0                     ; update interval counter
:bankrloop          lda          CurBank
                    sta          :bankread+3
                    ldx          StartAddr
:bankread           ldal         $000000,x
                    cmp          HexPattern
                    beq          :testpass
                    phx
                    sta          _stash                 ; = read value
                    lda          HexPattern
                    sta          _stash+1               ; = expected value
                    stx          _stash+2
                    jsr          PrintTestError         ; addr in X
                    plx
:testpass           cpx          EndAddr
                    beq          :donerbank
                    inx
                    iny
                    cpy          #UpdateScanInterval
                    bcc          :bankread
                    jsr          PrintTestCurrent
                    ldy          #0
                    bra          :bankread
:donerbank
                    ldy          #0                     ; because i'm anal.. this makes counter align
                    inc          CurBank
                    lda          EndBank
                    cmp          CurBank
                    bcs          :bankrloop
                    dec          CurBank                ; so many bad hacks
                    jsr          PrintTestCurrent       ; print final score ;)
                    sep          $10
                    sec
                    xce                                 ; WRITE END


                    jsr          Pauser                 ; PAUSE
                    lda          BorderColor
                    sta          $C034
                    jmp          BeginTestPass
:escpressed         sep          $10
                    sec
                    xce
                    rts

_testIteration      ds           8
_errorCounter       ds           8
UpdateScanInterval  equ          #$1000
Mesg_Rom            asc          "Apple IIgs ROM ",00
Mesg_UserManual     asc          "USE ARROW KEYS TO MOVE  -  USE ENTER TO SELECT/EDIT",00
Mesg_Starting       asc          $8D,"Starting Test",$8D,"Press P to pause, ESC to stop.",$8D,$8D,00
Mesg_Waiting        asc          "Waiting: ",00
Mesg_Writing        asc          "Writing: ",00
Mesg_Reading        asc          "Reading: ",00
Mesg_Errors         asc          " Errors:  ",$00
Mesg_TestPass       asc          "   Pass:  ",00
Mesg_Blank          asc          "                 ",00
Mesg_BoxTop30       asc          $1B,'ZLLLLLLLLLLLLLLLLLLLLLLLLLLLL_',$18,$8D,00
Mesg_BoxMid30       asc          $1B,'Z',"                            ",'_',$18,$8D,$00
Mesg_BoxBot30       asc          $1B,'Z',"____________________________",'_',$18,$8D,$00
*Mesg_ConsoleTop	asc $1B,'ZLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL_',$18,$8D,00
Mesg_ConsoleTop     asc          $1B,'ZLLLLLLLLLLLLLLL',$18,' Console Log ',$1B,'LLLLLLLLLLLLLLLLL_',$18,$8D,00
Mesg_ConsoleMid     asc          $1B,'Z',"                                             ",'_',$18,$8D,00
Mesg_ConsoleBot     asc          $1B,'Z',"_____________________________________________",'_',$18,$8D,00

* x, y, a=height
PrintBox30          stx          _prbox_x
                    sta          _prbox_height
                    jsr          GoXY
                    lda          #Mesg_BoxTop30
                    ldy          #>Mesg_BoxTop30
                    jsr          PrintString
:midloop            ldx          _prbox_x
                    stx          $24
                    lda          #Mesg_BoxMid30
                    ldy          #>Mesg_BoxMid30
                    jsr          PrintString
                    dec          _prbox_height
                    bne          :midloop

                    ldx          _prbox_x
                    stx          $24
                    lda          #Mesg_BoxBot30
                    ldy          #>Mesg_BoxBot30
                    jsr          PrintString
                    rts
* x, y, a=height
PrintConsole        stx          _prbox_x
                    sta          _prbox_height
                    jsr          GoXY
                    lda          #Mesg_ConsoleTop
                    ldy          #>Mesg_ConsoleTop
                    jsr          PrintString
:midloop            ldx          _prbox_x
                    stx          $24
                    lda          #Mesg_ConsoleMid
                    ldy          #>Mesg_ConsoleMid
                    jsr          PrintString
                    dec          _prbox_height
                    bne          :midloop

                    ldx          _prbox_x
                    stx          $24
                    lda          #Mesg_ConsoleBot
                    ldy          #>Mesg_ConsoleBot
                    jsr          PrintString
                    rts
_prbox_x            db           0
_prbox_height       db           0


* called with short M,  long X
PrintTestError
                    sec
                    xce
                    sep          $30
                    inc          _errorCounter
                    bne          :noRoll
                    inc          _errorCounter+1
:noRoll             PRINTXY      #38;#6;Mesg_Errors
                    ldx          _errorCounter
                    lda          _errorCounter+1
                    jsr          PRNTAX
                    jsr          WinConsole
                    LOG          Mesg_E1
                    ldx          _testIteration
                    lda          _testIteration+1
                    jsr          PRNTAX
                    PRINTSTRING  Mesg_E2

                    lda          CurBank
                    jsr          PRBYTE
                    lda          #"/"
                    jsr          COUT
                    lda          _stash+3
                    ldx          _stash+2
                    jsr          PRNTAX
                    lda          #$8D
                    jsr          COUT
                    jsr          WinFull
                    LOG          Mesg_E3
                    lda          _stash+1
                    jsr          PRBYTE
                    lda          #" "
                    jsr          COUT
                    lda          #"%"
                    jsr          COUT
                    lda          _stash+1
                    jsr          PRBIN
                    PRINTSTRING  Mesg_E4
                    lda          _stash
                    jsr          PRBYTE
                    lda          #" "
                    jsr          COUT
                    lda          #"%"
                    jsr          COUT
                    lda          _stash
                    jsr          PRBIN
                    clc
                    xce
                    rep          $10
                    rts
Mesg_E1             asc          "Bad Read - Pass ",00
Mesg_E2             asc          "   Location: ",00
Mesg_E3             asc          "Wrote: $",00
Mesg_E4             asc          " ",$1B,'SU',$18," Read: $",00
Mesg_Arrow          asc          $1B,'SU',$18,00

*Mesg_Error0	asc "Error: Bad Read Pass 0000  Location: 00/1234"
*Mesg_Error0	asc "Wrote: $00 %12345678    Read: $00 %12345678"





                    mx           %01
PrintTestCurrent    pha
                    phy
                    stx          _stash                 ; save real X
                    sec
                    xce
                    GOXY         #48;#7
                    lda          CurBank
                    sta          :corruptme+3
                    jsr          PRBYTE
                    lda          #"/"
                    jsr          COUT
                    lda          _stash+1
                    sta          :corruptme+2
                    jsr          PRBYTE
                    lda          _stash
                    sta          :corruptme+1
                    jsr          PRBYTE
* CORRUPTOR!
:kloop              lda          KEY
                    cmp          #"c"                   ; REMOVE DEBUG
                    beq          :corruptor
                    cmp          #"C"
                    beq          :corruptor
                    bra          :nocorrupt
:corruptor          jsr          GetRandTrash
:corruptme          stal         $060000                ; addr gets overwritten
                    inc          $c034
                    sta          STROBE                 ; we only clear if 'c' is hit
                    inc          _stash                 ; \
                    beq          :noroll                ;  |- INX
                    inc          _stash+1               ; /
:nocorrupt          cmp          #"p"                   ; check lower p
* @TODO make tolower for the comparisons
                    beq          :pause
                    cmp          #"P"
                    beq          :pause
                    bra          :nopause
:pause              sta          STROBE
                    jsr          WaitKey
:nopause
                    cmp          #$9B
                    bne          :noquit
                    clc
                    xce
                    rep          $10
                    ldx          _stash
                    ply
                    pla
                    sec
                    rts
:noquit
:noroll
                    clc
                    xce
                    rep          $10
                    ldx          _stash
                    ply
                    pla
                    clc
                    rts



                    mx           %11
PRBIN               pha
                    phx
                    ldx          #8
:loop               asl
                    pha
                    bcc          :zero
:one                lda          #"1"
                    jsr          COUT
                    bra          :ok
:zero               lda          #"0"
                    jsr          COUT
:ok                 pla
                    dex
                    bne          :loop
                    plx
                    pla
                    rts

Pauser
                    PRINTXY      #38;#8;Mesg_Waiting
                    ldy          #60
                    ldx          TestDelay
                    beq          :donepause
                    jsr          PrintTimerVal          ; inaugural print before waiting 1 sec
:secondloop
:wait               ldal         $e1c019
                    bpl          :wait
:wait2              ldal         $e1c019
                    bmi          :wait2
                    dey
                    bne          :secondloop
                    dex
                    beq          :donepause
                    jsr          PrintTimerVal
                    ldy          #60
                    bra          :secondloop
:donepause
                    PRINTXY      #38;#8;Mesg_Blank
                    rts
PrintTimerVal
                    phx
                    phy
                    txa
                    GOXY         #48;#8
                    ply
                    plx
                    txa
                    jsr          PRBYTE
                    rts

**************************************************
* Awesome PRNG thx to White Flame (aka David Holz)
**************************************************
GetRandTrash                                            ; USE ONLY WITH CORRUPTOR
                    lda          _randomTrashByte
                    beq          :doEor
                    asl
                    bcc          :noEor
:doEor              eor          #$1d
:noEor              sta          _randomTrashByte
                    rts
_randomTrashByte    db           0








* SETTINGS!!!
* DEFAULTS
StartBank           db           #$06
EndBank             db           #$1F
CurBank             db           #0
StartAddr           dw           #$0000
EndAddr             dw           #$FFFF
HexPattern          dw           #$0000
TestDelay           dw           #$01



MainMenuDefs
:StartBank          hex          19,05                  ; x,y
                    db           MenuOption_Hex         ; 1=hex input
                    db           01                     ; memory size (bytes)
                    da           StartBank              ; variable storage
:EndBank            hex          22,05                  ; x,y
                    db           MenuOption_Hex         ; 1=hex input
                    db           01                     ; memory size (bytes)
                    da           EndBank                ; variable storage
:StartAddr          hex          19,06                  ; x,y
                    db           MenuOption_Hex         ; 1=hex input
                    db           02                     ; memory size (bytes)
                    da           StartAddr              ; variable storage
:EndAddr            hex          20,06                  ; x,y
                    db           MenuOption_Hex         ; 1=hex input
                    db           02                     ; memory size (bytes)
                    da           EndAddr                ; variable storage
:TestType           hex          19,07                  ; x,y
                    db           MenuOption_List        ; 3=list input
                    db           11                     ; max len size (bytes), 3=option list
                    da           TestType               ; params definition & storage
:HexPattern         hex          19,08                  ; x,y
                    db           MenuOption_Hex         ; 3=list input
                    db           02                     ; max len size (bytes), 3=option list <- can change when 8 bit??
                    da           HexPattern             ; params definition & storage
:BinPattern         hex          19,09                  ; x,y
                    db           MenuOption_Bin         ; 5?=list input
                    db           02                     ; max len size (bytes), 3=option list <- can change when 8 bit??
                    da           HexPattern             ; params definition & storage <- uses same space as above!! just different representation

:TestSize           hex          13,0C                  ; x,y
                    db           MenuOption_List        ; 3=list input
                    db           08                     ; max len size (bytes), 3=option list
                    da           TestSize               ; params definition & storage
:TestDelay          hex          13,0F                  ; x,y
                    db           MenuOption_Hex         ; 1=hex input
                    db           01                     ; memory size (bytes)
                    da           TestDelay              ; variable storage
:BeginTest          hex          0B,14                  ; x,y
                    db           MenuOption_Action      ; 2=action
                    db           MenuStr_BeginTestL     ; menu string length
                    da           MenuStr_BeginTest      ; string storage
MainMenuLen         equ          *-MainMenuDefs
MainMenuItems       equ          MainMenuLen/6
MainMenuEnd         dw           0000
Menu_ItemSelected   db           0



* 00 - Byte : Selected Value
* 01 - Byte : Number of values
* 02... - Words : Table of Addresses of possible values
TestType            db           00                     ; actual CONST val
                    db           06                     ; number of possible values
                    da           _TestType_0,_TestType_1,_TestType_2,_TestType_3,_TestType_4,_TestType_5,00,00
_TestType_0         asc          "BYTE",$00
_TestType_1         asc          "WORD",$00
_TestType_2         asc          "RANDBYTE",$00
_TestType_3         asc          "RANDWORD",$00
_TestType_4         asc          "CHECKERS",$00
_TestType_5         asc          "BANK",$00

TestSize            db           00
                    db           02
                    da           _TestSize_0,_TestSize_1
_TestSize_0         asc          "BYTE",$00
_TestSize_1         asc          "WORD",$00

MenuStr_JSR         da           BeginTest              ; MUST PRECEDE MENU STRING!  Yes, it's magicly inferred. (-2)
MenuStr_BeginTest   asc          "BEGIN TEST"
MenuStr_BeginTestL  equ          #*-MenuStr_BeginTest
MenuStr_BeginTestE  db           00
MainMenuStrs
                    asc          " ______________________________________________________________________________",$8D,$00
                    asc          $1B,'ZV_@ZVWVWVWV_',"Mini Memory Tester v0.3",'ZVWVWVWVWVWVWVWVWVWVW_',"UltimateMicro",'ZWVWVWVW_',$18,$00
                    asc          $1B,'ZLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL_',$18,00
                    asc          $1B,'ZZ \GGGGGGGGGGGGG_',"Test Settings",'ZGGGGGGGGGGGGG\ _',"  ",'Z \GGGGGGGG_',"Info",'ZGGGGGGGG\ _'," ",'_',$18,00
                    asc          $1B,'ZZ',"                                             ",'_',"  ",'Z',"                          ",'_'," ",'_',$18,00
                    asc          $1B,'ZZ',"  Start/End Bank    : [06]  /  [1F]          ",'_',"  ",'Z',"                          ",'_'," ",'_',$18,00
                    asc          $1B,'ZZ',"  Start/End Address : [0000]/[FFFF]          ",'_',"  ",'Z',"                          ",'_'," ",'_',$18,00
                    asc          $1B,'ZZ',"  Test Type         : [bit pattern]          ",'_',"  ",'Z',"                          ",'_'," ",'_',$18,00
                    asc          $1B,'ZZ',"       Hex Pattern  : [0000]                 ",'_',"  ",'Z',"                          ",'_'," ",'_',$18,00
                    asc          $1B,'ZZ',"       Bin Pattern  : [0000 0000 0000 0000]  ",'_',"  ",'Z',"                          ",'_'," ",'_',$18,00
                    asc          $1B,'ZZ',"                                             ",'_',"  ",'Z',"                          ",'_'," ",'_',$18,00
                    asc          $1B,'ZZ',"  Direction    [up]    Parallel R/W  [off]   ",'_',"  ",'Z',"                          ",'_'," ",'_',$18,00
                    asc          $1B,'ZZ',"  Adjacent Wr. [off]   Refresh Pause [000]   ",'_',"  ",'Z',"                          ",'_'," ",'_',$18,00
                    asc          $1B,'ZZ',"  Read Repeat  [000]   Write Repeat  [000]   ",'_',"  ",'Z',"                          ",'_'," ",'_',$18,00
                    asc          $1B,'ZZ',"  Iterations   [000]                         ",'_',"  ",'Z',"                          ",'_'," ",'_',$18,00
                    asc          $1B,'ZZ',"                                             ",'_',"  ",'Z',"                          ",'_'," ",'_',$18,00
                    asc          $1B,'ZLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL_',$18,00
                    asc          $1B,'Z',"                                                                              ",'_',$18,00
                    asc          $1B,'Z',"                                                                              ",'_',$18,00
                    asc          $1B,'Z',"                                                                              ",'_',$18,00
                    asc          $1B,'Z',"                                                                              ",'_',$18,00
                    asc          $1B,'Z',"                                                                              ",'_',$18,00
                    asc          $1B,'Z',"______________________________________________________________________________",'_',$18,00

*	asc "     ABCDEFGHIZKLMNOPQRSTUVWXYZ ",$8D,$00
*	asc $1B,'     ABCDEFGHIZKLMNOPQRSTUVWXYZ ',$1B,$8D,$00

                    hex          00,00


CheckKey            lda          KEY
                    bpl          :noKey
                    sta          STROBE
                    sec
                    rts
:noKey              clc
                    rts


MenuCheckKeyColor   jsr          ColorizeMenu
                    lda          _ticker
                    bne          :skipDraw              ; we want to avoid updating when nothing is happening... "Save the Cycles!!" ;)
                    jsr          Menu_DrawSelected
:skipDraw           cmp          #14
                    bne          :skipUndraw
                    jsr          Menu_UndrawSelectedAll
:skipUndraw         cmp          #$20
                    bne          :noReset
                    stz          _ticker
                    jmp          CheckKey               ; Will RTS from CheckKey
:noReset            inc          _ticker
                    jmp          CheckKey               ; Will RTS from CheckKey

_ticker             dw           0



WaitKey
:kloop
                    jsr          ColorizeMenu
                    lda          KEY
                    bpl          :kloop
                    sta          STROBE
                    cmp          #"b"                   ; REMOVE DEBUG
                    bne          :nobreak
                    brk          $75
:nobreak
                    rts

                    put          strings.s
                    put          menu.s
BorderColor         db           0
                    ds           \
_stash              ds           255
                    ds           \