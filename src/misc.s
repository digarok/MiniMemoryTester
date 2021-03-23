
MenuCheckKeyColor   jsr       ColorizeMenu

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


                    mx        %11
ColorizeMenu        sei
                    XSCANLINE #$6;#$A0               ;lt grey
                    XSCANLINE #$7;#$C0               ;grn
                    XSCANLINE #$8;#$D0               ;yello
                    XSCANLINE #$A;#$90               ;orange
                    XSCANLINE #$B;#$10               ;red
                    XSCANLINE #$C;#$30               ;purple
                    XSCANLINE #$E;#$70               ;blue
                    XSCANLINE #$F;#$50               ;grey
                    XSCANLINE #$10;#$F0               ;white
                    cli
                    rts

* I think this still has latent issues with a desync'ed bit 0 (race condition)
XSCANLINE           MAC
                    ldx #]2
:waitloop           lda      $c02f
                    asl
                    lda      $c02e
                    rol
:val                cmp       #]1
                    bne       :waitloop
                    stx       $c022
                    <<<


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
                    lda       #$01
                    bit       $c025                   ; check shift key
                    beq       :no_egg
:egg                jsr       DrawUMLogo
                    bra       :skip
:no_egg             jsr       DrawMMTLogo
:skip               jsr       WaitSome                ;just for VSYNC
                    jsr       DL_SetDLRMode
                    sta       MIXSET
                    jsr       MakeUMSound             ;play sounds

                    jsr       Intro_WhiteMixText
                    sta       $C00f
                    sta       TXTPAGE1

                    jsr       Clicky
                    PRINTXY   #32;#20;Mesg_Ultimate0
                    jsr       Clicky
                    PRINTXY   #20;#21;Mesg_Ultimate1
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
                    plx

                    jsr       CheckKey                ;
                    bcs       :pauseover              ;SKIP THE REST!


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

*Mesg_Testchars      asc       $1b,'Uu ',"Uu ",$18,'Uu ',"Uu ",00
Mesg_Ultimate0      asc       $18,                    "an Apple IIgs",00
Mesg_Ultimate1      asc       $18,                    "Memory Expansion (RAM) Card Test Utility",00
Mesg_Programmed     asc       $18,                    " Programmed by Dagen Brock",00

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

DrawMMTLogo
                    lda       #<MMT_MAINRLE
                    sta       $2
                    lda       #>MMT_MAINRLE
                    sta       $3
                    sta       TXTPAGE1
                    jsr       DKUnpackRLEToLoRes

                    lda       #<MMT_AUXRLE
                    sta       $2
                    lda       #>MMT_AUXRLE
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

* MAIN ... Size RAW 800 -> Compressed RLE 508   (63.50%)
MMT_MAINRLE
                    hex       06FF85AAAFFFAFAA02FF85AAAFFFAFAA
                    hex       02FF85AAAFFFAFAA02FF85AAAFFFAFAA
                    hex       0EFF02AA81FF02AA02FF02AA81FF02AA
                    hex       02FF02AA81FF02AA02FF02AA81FF02AA
                    hex       0EFF02AA81FF02AA02FF02AA81FF02AA
                    hex       02FF02AA81FF02AA02FF02AA81FF02AA
                    hex       0BFF81AF020F0205810F0205020F0205
                    hex       810F0205020F0205810F0205020F0205
                    hex       810F0205040F07FF210007FF210007FF
                    hex       070084EECE44E002CE8500EECE44E002
                    hex       CE8200EE05CE060007FF8150060081EE
                    hex       02CC81EE02CC8200EE02CC81EE02CC87
                    hex       00EECC4CCC4CCC060007FF8155060094
                    hex       EECCECCCC4CC00EECCECCCE4CC000C04
                    hex       EECC0004060007FF025505008DEECC4E
                    hex       4CEECC00EECC4E4CEECC030082EECC08
                    hex       0007FF825505050082EECC020085EECC
                    hex       00EECC020082EECC030082EECC080007
                    hex       FF8155060082EECC020085EECC00EECC
                    hex       020082EECC030082EECC080007FF0700
                    hex       82EECC020085EECC00EECC020082EECC
                    hex       030082EECC080007FF070082CE4C0200
                    hex       85CE4C00CE4C020082CE4C030082CE4C
                    hex       080007FF210007FF21500AFF02A581FF
                    hex       02A502FF02A581FF02A502FF02A581FF
                    hex       02A502FF02A581FF02A50EFF02AA81FF
                    hex       02AA02FF02AA81FF02AA02FF02AA81FF
                    hex       02AA02FF02AA81FF02AA0EFF02AA81FF
                    hex       02AA02FF02AA81FF02AA02FF02AA81FF
                    hex       02AA02FF02AA81FF02AA0EFF81FA03FF
                    hex       81FA02FF81FA03FF81FA02FF81FA03FF
                    hex       81FA02FF81FA03FF81FA08FF

* AUX  ... Size RAW 800 -> Compressed RLE 510   (63.75%)
MMT_AUXRLE
                    hex       06FF825F5502FF85555FFF5F5502FF85
                    hex       555FFF5F5502FF85555FFF5F5502FF82
                    hex       555F0DFF025502FF025581FF025502FF
                    hex       025581FF025502FF025581FF025502FF
                    hex       02550DFF025502FF025581FF025502FF
                    hex       025581FF025502FF025581FF025502FF
                    hex       02550AFF030F020A020F020A810F020A
                    hex       020F020A810F020A020F020A810F020A
                    hex       020F020A040F06FF220006FF220006FF
                    hex       08000267850077672600026785007767
                    hex       260005678126060006FF81A007000266
                    hex       81220266822200026681220266882200
                    hex       662666266622060006FF02AA06008E66
                    hex       22662666220066226626662200020282
                    hex       66220202060006FF02AA06008D662226
                    hex       22662200662226226622030082662208
                    hex       0006FF02AA0600826622020085662200
                    hex       662202008266220300826622080006FF
                    hex       82AA0A06008266220200856622006622
                    hex       02008266220300826622080006FF0800
                    hex       82662202008566220066220200826622
                    hex       0300826622080006FF08008226220200
                    hex       85262200262202008226220300822622
                    hex       080006FF220006FF21A0810009FF025A
                    hex       02FF025A81FF025A02FF025A81FF025A
                    hex       02FF025A81FF025A02FF025A0DFF0255
                    hex       02FF025581FF025502FF025581FF0255
                    hex       02FF025581FF025502FF02550DFF0255
                    hex       02FF025581FF025502FF025581FF0255
                    hex       02FF025581FF025502FF02550EFF81F5
                    hex       02FF81F503FF81F502FF81F503FF81F5
                    hex       02FF81F503FF81F502FF81F508FF

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
