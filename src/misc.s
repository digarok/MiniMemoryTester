
MenuCheckKeyColor  jsr   ColorizeMenu
                   lda   _ticker
                   bne   :skipDraw               ; we want to avoid updating when nothing is happening... "Save the Cycles!!" ;)
                   jsr   Menu_HighlightSelected
:skipDraw          cmp   #12
                   bne   :skipUndraw
                   jsr   Menu_UndrawSelectedAll
:skipUndraw        cmp   #16
                   bne   :noReset
                   stz   _ticker
                   jmp   CheckKey                ; Will RTS from CheckKey
:noReset           inc   _ticker
                   jmp   CheckKey                ; Will RTS from CheckKey
_ticker            dw    0

CheckKey           lda   KEY
                   bpl   :noKey
                   sta   STROBE
                   sec
                   rts
:noKey             clc
                   rts

ToLower            cmp   #"Z"
                   bcs   :notUpper
                   cmp   #"A"
                   bcc   :notUpper
                   clc
                   adc   #$20                    ;add 32 to get lower char
:notUpper          rts


WaitKey
:kloop
                   jsr   ColorizeMenu
                   lda   KEY
                   bpl   :kloop
                   sta   STROBE
                   cmp   #"b"                    ; REMOVE DEBUG
                   bne   :nobreak
                   brk   $75
:nobreak
                   rts


ColorizeMenu
:loop
                   lda   #6
                   jsr   WaitSCB
                   lda   #$A0                    ; green
                   sta   $c022

                   lda   #7
                   jsr   WaitSCB
                   lda   #$c0                    ; green
                   sta   $c022

                   lda   #9
                   jsr   WaitSCB
                   lda   #$d0                    ; yello
                   sta   $c022

                   lda   #10
                   jsr   WaitSCB
                   lda   #$90                    ; orange
                   sta   $c022


                   lda   #11
                   jsr   WaitSCB
                   lda   #$10                    ; red
                   sta   $c022

                   lda   #12
                   jsr   WaitSCB
                   lda   #$30                    ; purple
                   sta   $c022

                   lda   #13
                   jsr   WaitSCB
                   lda   #$70                    ; bblue
                   sta   $c022

                   lda   #15
                   jsr   WaitSCB
                   lda   #$50                    ; grey
                   sta   $c022

                   lda   #16
                   jsr   WaitSCB
                   lda   #$f0                    ; white
                   sta   $c022
                   rts




WaitSCB            sta   :val+1
                   ldx   #2                      ; to check twice
:waitloop          lda   $c02f
                   asl
                   lda   $c02e
                   rol
:val               cmp   #$00
                   bne   :waitloop
                   dex
                   bne   :waitloop
                                                 ; the problem is we can get the LAST
                                                 ; horizcnt even/odd right as it changes
                                                 ; and start early or something?
                   rts
MAXSCB             db    0

Full16             MAC
                   clc
                   xce
                   rep   #$30
                   <<<

ShortMX            MAC
                   sep   #$30
                   <<<

PushAll            MAC
                   pha
                   phx
                   phy
                   <<<

PopAll             MAC
                   ply
                   plx
                   pla
                   <<<



