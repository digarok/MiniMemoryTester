
MenuCheckKeyColor   jsr       ColorizeMenu

*                    lda       #55
*                    jsr       WaitSCB
*                    lda       #80
*                    jsr       WaitSCB
*                    lda       #55
*                    jsr       WaitSCB
*                    lda       #80
*                    jsr       WaitSCB

                    lda       _ticker
                    bne       :skipDraw               ; we want to avoid updating when nothing is happening... "Save the Cycles!!" ;)
                    jsr       Menu_HighlightSelected
:skipDraw           cmp       #12
                    bne       :skipUndraw
                    jsr       Menu_UndrawSelectedAll
:skipUndraw         cmp       #16
                    bne       :noReset
                    stz       _ticker
                    jmp       CheckKey                ; Will RTS from CheckKey
:noReset            inc       _ticker
                    jmp       CheckKey                ; Will RTS from CheckKey
_ticker             dw        0

CheckKey            lda       KEY
                    bpl       :noKey
                    sta       STROBE
                    sec
                    rts
:noKey              clc
                    rts

ToLower             cmp       #"Z"
                    bcs       :notUpper
                    cmp       #"A"
                    bcc       :notUpper
                    clc
                    adc       #$20                    ;add 32 to get lower char
:notUpper           rts


WaitKey
:kloop
                    jsr       ColorizeMenu
                    lda       KEY
                    bpl       :kloop
                    sta       STROBE
                    cmp       #"b"                    ; REMOVE DEBUG
                    bne       :nobreak
                    brk       $75
:nobreak
                    rts
MiniWait            nop
                    nop
                    nop
                    nop
                    rts

                    mx        %11
ColorizeMenu
                    lda       #6
                    ldx       #$A0                    ; lt gray
                    jsr       WaitScanline

                    lda       #7
                    ldx       #$A0                    ; lt gray
                    jsr       WaitScanline

                    lda       #8
                    ldx       #$C0                    ; green
                    jsr       WaitScanline

                    lda       #9
                    ldx       #$C0                    ; green
                    jsr       WaitScanline

                    lda       #10
                    ldx       #$C0                    ; green
                    jsr       WaitScanline

                    lda       #11
                    ldx       #$d0                    ; yello
                    jsr       WaitScanline

                    lda       #12
                    ldx       #$90                    ; orange
                    jsr       WaitScanline

                    lda       #13
                    ldx       #$10                    ; red
                    jsr       WaitScanline

                    lda       #14
                    ldx       #$30                    ; purple
                    jsr       WaitScanline


                    lda       #15
                    ldx       #$70                    ; bblue
                    jsr       WaitScanline

                    lda       #16
                    ldx       #$50                    ; grey
                    jsr       WaitScanline

                    lda       #17
                    ldx       #$f0                    ; white
                    jsr       WaitScanline
                    rts

* now stores x immediately
WaitScanline                                          ;jmp       WaitSCB
                    sta       :val+1

:waitloop           ldal      $e0c02f
                    asl
                    ldal      $e0c02e
                    rol
:val                cmp       #$00
                    bne       :waitloop
                    stx       $c022
                    rts

ColorizeMenuOld
:loop
                    lda       #6
                    jsr       WaitSCB
                    lda       #$A0                    ; green
                    sta       $c022

                    lda       #7
                    jsr       WaitSCB
                    lda       #$c0                    ; green
                    sta       $c022

                    lda       #9
                    jsr       WaitSCB
                    lda       #$d0                    ; yello
                    sta       $c022

                    lda       #10
                    jsr       WaitSCB
                    lda       #$90                    ; orange
                    sta       $c022


                    lda       #11
                    jsr       WaitSCB
                    lda       #$10                    ; red
                    sta       $c022

                    lda       #12
                    jsr       WaitSCB
                    lda       #$30                    ; purple
                    sta       $c022

                    lda       #13
                    jsr       WaitSCB
                    lda       #$70                    ; bblue
                    sta       $c022

                    lda       #15
                    jsr       WaitSCB
                    lda       #$50                    ; grey
                    sta       $c022

                    lda       #16
                    jsr       WaitSCB
                    lda       #$f0                    ; white
                    sta       $c022
                    rts

VBlankForce
:vbl                ldal      $00c019
                    bmi       :vbl
                    jmp       VBlank
                    rts
VBlank
:vbl                ldal      $00c019
                    bpl       :vbl
                    rts

WaitSCB             sta       :val+1
                    ldx       #2                      ; to check twice
:waitloop           lda       $c02f
                    asl
                    lda       $c02e
                    rol
:val                cmp       #$00
                    bne       :waitloop
                    dex
                    bne       :waitloop
                                                      ; the problem is we can get the LAST
                                                      ; horizcnt even/odd right as it changes
                                                      ; and start early or something?
                    rts
MAXSCB              db        0


WaitSome            ldy       #$07
:loop               lda       #55
                    jsr       WaitSCB
                    lda       #198
                    jsr       WaitSCB
                    dey
                    bne       :loop
                    rts

Full16              MAC
                    clc
                    xce
                    rep       #$30
                    <<<

ShortMX             MAC
                    sep       #$30
                    <<<

PushAll             MAC
                    pha
                    phx
                    phy
                    <<<

PopAll              MAC
                    ply
                    plx
                    pla
                    <<<
ColorAll            MAC
                    lda       #]1
                    sta       $C022
                    and       #$0F
                    sta       $C034
                    <<<

Intro               lda       #$f5                    ;gray
                    sta       $c022
                    lda       #$05
                    sta       $c034
                    jsr       WaitSome


                    lda       #$0A
                    sta       $c034
                    lda       #$FA
                    sta       $c022                   ;lt gray
                    jsr       WaitSome

                    ColorAll  #$FF                    ;lt gray


                    jsr       PrepDLR80Col
                    jsr       DrawUMLogo
                    jsr       WaitSome                ;just for VSYNC
                    jsr       DL_SetDLRMode
                    sta       MIXSET
                    jsr       MakeUMSound             ;play sounds

                    jsr       Intro_WhiteMixText
                    sta       $C00f
                    sta       TXTPAGE1

                    jsr       Clicky
                    PRINTXY   #25;#20;Mesg_Ultimate0
                    jsr       Clicky
                    PRINTXY   #25;#21;Mesg_Ultimate1
                    jsr       CheckKey                ;
                    bcs       :pauseover              ;SKIP THE REST!

                    lda       #$05
:longerwait         pha
                    jsr       WaitSome
                    pla
                    dec
                    bne       :longerwait

                    lda       #$AF
                    sta       $c022                   ;lt gray
                    jsr       WaitSome
                    jsr       WaitSome
                    lda       #$5F
                    sta       $c022                   ;lt gray
                    jsr       WaitSome
                    jsr       WaitSome
                    lda       #$0F
                    sta       $c022                   ;lt gray
                    jsr       WaitSome
                    jsr       WaitSome

                    jsr       CheckKey                ;
                    bcs       :pauseover              ;SKIP THE REST!


                    sta       TXTPAGE2

                    GOXY      #26;#23
                    ldx       #$0
:prloop
                    lda       Mesg_Programmed,x
                    beq       :done
                    phx
                    jsr       COUT
                    cmp       #" "
                    bcc       :invisible
                    jsr       Clicky
:invisible
                    jsr       VBlankForce

                    jsr       CheckKey                ;
                    bcs       :pauseover              ;SKIP THE REST!

                    plx
                    inx
                    bra       :prloop
:done

                    ldx       #$e0
:keyloop

                    jsr       CheckKey                ;
                    bcs       :pauseover              ;SKIP THE REST!

                    dex
                    beq       :pauseover
                    phx
                    jsr       VBlankForce

                    plx
                    jsr       CheckKey
                    bcc       :keyloop
:pauseover
                    sta       TXTSET
                    lda       #$00
                    sta       $c034                   ; black border
                    lda       #$f0
                    sta       $c022
                    rts

MAXGAP              =         #800
MAXLEN              =         #300
ErrorNoise
                    clc
                    xce
                    rep       #$10
                    ldx       #0
                    stx       _shiftingGap2

                    ldx       #MAXGAP
                    stx       _shiftingGap
:loop
                    sta       SPEAKER

                    ldx       _shiftingGap
:pause1             dex
                    bne       :pause1

                    sta       SPEAKER

                    ldx       _shiftingGap2
:pause2             dex
                    bne       :pause2


                    ldx       _shiftingGap
                    dex
                    beq       :done
                    stx       _shiftingGap

                    ldx       _shiftingGap2
                    inx
                    cpx       #MAXLEN
                    beq       :done
                    stx       _shiftingGap2
                    bra       :loop

:done               sec
                    xce
                    sep       #$30
                    rts


_shiftingGap        dw        0
_shiftingGap2       dw        0

Clicky              ldx       #$FE
:nox                txy
                    nop
                    nop
                    nop
                    nop

                    sta       SPEAKER
:noy                dey
                    dey
                    dey
                    dey
                    dey
                    lda       #2
:long               nop
                    nop
                    nop
                    nop
                    nop
                    nop
                    nop
                    nop
                    nop
                    nop
                    nop
                    dec
                    bne       :long
                    cpy       #5
                    bcs       :noy
                    dex
                    dex
                    cpx       #$f0
                    bne       :nox
                    ldx       #$00
:wait               inx
                    bne       :wait
                    rts

MakeUMSound
:loops
                    ldy       #$66
                    sty       :fo+1
                    lda       #$ff
:first              ldx       #$f9
:fo                 ldy       #$86
                    sta       $C030
:quick              dex
                    nop
                    nop
                    nop

                    nop
                    nop
                    nop
                    nop
                    nop
                    nop
                    nop
                    bne       :quick
                    sta       $C030
:l2                 dey
                    nop
                    nop
                    nop

                    nop
                    nop
                    nop
                    bne       :l2
                    dec
                    inc       :fo+1
                    inc       :fo+1
                    bne       :first
                    rts

Mesg_Testchars      asc       $1b,'Uu ',"Uu ",$18,'Uu ',"Uu ",00
Mesg_Ultimate0      asc       $18,                    "     in association with    ",00
Mesg_Ultimate1      asc       $18,                    "U l t i m a t e      M i c r o",00
Mesg_Programmed     asc       $18,                    "  Programmed by Dagen Brock",00
Intro_WhiteMixText  lda       #" "
                    sta       TXTPAGE1

                    ldy       #2                      ;loop checker
:pageloop           ldx       #0
:loop               sta       Lo21,x
                    sta       Lo22,x
                    sta       Lo23,x
                    sta       Lo24,x
                    inx
                    cpx       #40
                    bne       :loop
                    sta       TXTPAGE2
                    dey
                    bne       :pageloop
                    rts
* To allow us to uncompress to 80col text before turning on DLR (I hope)

PrepDLR80Col
                    lda       CLRAN3                  ;enables DLR
                    sta       SET80VID
                    sta       C80STOREON              ; enable aux/page1,2 mapping

                    LDA       #$A0                    ;USE A BLANK SPACE TO
                    JSR       $C300                   ;TURN ON THE VIDEO FIRMWARE

                    rts

DL_SetDLRMode
                    lda       LORES                   ;set lores
                    sta       TXTCLR
                    lda       CLRAN3                  ;enables DLR
                    sta       SET80VID
                    sta       C80STOREON              ; enable aux/page1,2 mapping
                    sta       MIXCLR                  ;make sure graphics-only mode
                    rts
DrawUMLogo
                    lda       #<UMLOGOLOW_MAINRLE
                    sta       $2
                    lda       #>UMLOGOLOW_MAINRLE
                    sta       $3
                    sta       TXTPAGE1
                    jsr       DKUnpackRLEToLoRes


                    lda       #<UMLOGOLOW_AUXRLE
                    sta       $2
                    lda       #>UMLOGOLOW_AUXRLE
                    sta       $3
                    sta       TXTPAGE2
                    jsr       DKUnpackRLEToLoRes
                    rts


* MAIN ... Size RAW 800 -> Compressed RLE 306   (38.25%)
UMLOGOLOW_MAINRLE
                    hex       2EFF05CA04FF0FCA02AF0EFF05CC04FF
                    hex       11CC81AF0DFF05CC04FF12CC815F0CFF
                    hex       05DD04FF12DD81550CFF05DD04FF05DD
                    hex       02AD05DD81AD05DD81550CFF05DD04FF
                    hex       05DD82FFAA05DD82FFAA04DD81550CFF
                    hex       059904FF059982FFAA059982FFAA0499
                    hex       81550CFF059904FF059982FFAA059982
                    hex       FFAA049981550CFF059904FF059982FF
                    hex       AA059982FFAA049981550CFF051104FF
                    hex       051182FFAA051182FFAA041181550CFF
                    hex       051104FF051182FFAA051182FFAA0411
                    hex       81550CFF051104FF051182FFAA051182
                    hex       FFAA041181550CFF053304AF053382FF
                    hex       AA053382FFAA043381550CFF0E3382FF
                    hex       AA053382FFAA043381550CFF0E3382FF
                    hex       AA053382FFAA043381550CFF81FA0D77
                    hex       82FFAA057782FFAA047781550DFF81FA
                    hex       0C7782FFAA057782FFAA047781550EFF
                    hex       81FA0B5782FFAA055782FFAA04578155
                    hex       2EFF

* AUX  ... Size RAW 800 -> Compressed RLE 358   (44.75%)
UMLOGOLOW_AUXRLE
                    hex       2EFF8155046581A503FF81550F65815F
                    hex       0EFF8155046681AA03FF815510668165
                    hex       0DFF8155046681AA03FF815511668165
                    hex       0CFF815504EE81AA03FF815512EE0CFF
                    hex       815504EE81AA03FF815505EE815E05EE
                    hex       025E05EE0CFF815504EE81AA03FF8155
                    hex       04EE82AAFF05EE82AAFF05EE0CFF8155
                    hex       04CC81AA03FF815504CC82AAFF05CC82
                    hex       AAFF05CC0CFF815504CC81AA03FF8155
                    hex       04CC82AAFF05CC82AAFF05CC0CFF8155
                    hex       04CC81AA03FF815504CC82AAFF05CC82
                    hex       AAFF05CC0CFF8155048881AA03FF8155
                    hex       048882AAFF058882AAFF05880CFF8155
                    hex       048881AA03FF8155048882AAFF058882
                    hex       AAFF05880CFF8155048881AA03FF8155
                    hex       048882AAFF058882AAFF05880CFF8155
                    hex       0499819A035F8195049982AAFF059982
                    hex       AAFF05990CFF81550D9982AAFF059982
                    hex       AAFF05990CFF81550D9982AAFF059982
                    hex       AAFF05990DFF815B0CBB82AAFF05BB82
                    hex       AAFF05BB0EFF815B0BBB82AAFF05BB82
                    hex       AAFF05BB0FFF81FA0AAB82AAFF05AB82
                    hex       AAFF05AB2EFF
*** DK! DLR Kit *********
* DKUnpackRLEToLoRes
* Word ($2) = Address of pack data
* Expects you to have already set main or aux
DKUnpackRLEToLoRes
                    lda       #0
                    sta       _line
                    tay

                    lda       LoLineTable,y
                    sta       $0
                    lda       LoLineTable+1,y
                    sta       $1

_getUnpackCommand   ldx       #$0
                    lda       ($2,x)
                    bmi       :uncoded
:repeat             sta       _repeat
                    inc       $2                      ;\
                    bne       :noroll                 ; >-- increment pack data ptr
                    inc       $3                      ;/
:noroll             lda       ($2,x)                  ; get color value

                    ldx       _repeat                 ; x=repeat count

:repeatLoop         sta       ($0),y                  ; write to screen
                    iny
                    cpy       #40
                    bne       :notEOL
                    pha
                    inc       _line
                    lda       _line
                    cmp       #24
                    bne       :notDoneLines
                    pla
                    rts                               ; DONE?  this might be impossible
:notDoneLines       asl
                    tay
                    lda       LoLineTable,y
                    sta       $0
                    lda       LoLineTable+1,y
                    sta       $1
                    ldy       #0
                    pla                               ;restore a
:notEOL             dex
                    bne       :repeatLoop
                    inc       $2
                    beq       :roll
                    jmp       _getUnpackCommand
:roll               inc       $3
                    jmp       _getUnpackCommand


:uncoded            and       #$7f                    ;strip high bit
                    sta       _repeat                 ; re-use repeat for uncoded count
:repeatLoop2
                    inc       $2
                    bne       :noroll2
                    inc       $3
:noroll2            lda       ($2,x)                  ; get color value
                    sta       ($0),y                  ; write to screen
                    iny
                    cpy       #40
                    bne       :notEOL2
                    pha
                    inc       _line
                    lda       _line
                    cmp       #24
                    bne       :notDoneLines2
                    pla
                    rts                               ; DONE?  this might be impossible
:notDoneLines2      asl
                    tay
                    lda       LoLineTable,y
                    sta       $0
                    lda       LoLineTable+1,y
                    sta       $1
                    ldy       #0
                    pla                               ;restore a
:notEOL2            dec       _repeat
                    bne       :repeatLoop2
                    inc       $2
                    beq       :roll2
                    jmp       _getUnpackCommand
:roll2              inc       $3
                    jmp       _getUnpackCommand




_repeat             db        0
_line               db        0




LoLineTable         da        Lo01,Lo02,Lo03,Lo04,Lo05,Lo06
                    da        Lo07,Lo08,Lo09,Lo10,Lo11,Lo12
                    da        Lo13,Lo14,Lo15,Lo16,Lo17,Lo18
                    da        Lo19,Lo20,Lo21,Lo22,Lo23,Lo24
** Here we split the table for an optimization
** We can directly get our line numbers now
** Without using ASL
LoLineTableH        db        >Lo01,>Lo02,>Lo03,>Lo04,>Lo05,>Lo06
                    db        >Lo07,>Lo08,>Lo09,>Lo10,>Lo11,>Lo12
                    db        >Lo13,>Lo14,>Lo15,>Lo16,>Lo17,>Lo18
                    db        >Lo19,>Lo20,>Lo21,>Lo22,>Lo23,>Lo24
LoLineTableL        db        <Lo01,<Lo02,<Lo03,<Lo04,<Lo05,<Lo06
                    db        <Lo07,<Lo08,<Lo09,<Lo10,<Lo11,<Lo12
                    db        <Lo13,<Lo14,<Lo15,<Lo16,<Lo17,<Lo18
                    db        <Lo19,<Lo20,<Lo21,<Lo22,<Lo23,<Lo24

