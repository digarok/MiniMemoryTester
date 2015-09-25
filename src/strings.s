**** MACROS
* GOXY #x;#y
* PRINTXY #x;#y;StringAddrWord
* PRINTSTRING #addr
**** FUNCTIONS
* GoXY
* PrintStringsX
* PrintString

PRINTSTRING           MAC
                      lda   #]1
                      ldy   #>]1
                      jsr   PrintString
                      <<<

GOXY                  MAC
                      ldx   ]1
                      ldy   ]2
                      stx   $24
                      sty   $25
                      jsr   VTAB
                      <<<

PRINTXY               MAC
                      ldx   ]1
                      ldy   ]2
                      stx   $24
                      sty   $25
                      jsr   VTAB
                      lda   #]3
                      ldy   #>]3
                      jsr   PrintString
                      <<<

GoXY                  stx   $24
                      sty   $25
                      jsr   VTAB
                      rts

*	lda #MainMenuStrs
*	ldy #>MainMenuStrs
*	ldx #05	; horiz pos
PrintStringsX         stx   _printstringsx_horiz

                      sta   $0
                      sty   $1
:loop                 lda   _printstringsx_horiz
                      sta   $24
                      lda   $0                    ; slower, but allows API reuse
                      ldy   $1
                      jsr   PrintString           ; y is last val
                      iny
                      lda   ($0),y
                      beq   :done
                      tya                         ; not done so add strlen to source ptr
                      clc
                      adc   $0
                      sta   $0
                      bcc   :nocarry
                      inc   $1
:nocarry              bra   :loop


:done                 rts



_printstringsx_horiz  db    00

* PrintString (A=Low Byte,  Y=High Byte)
PrintString           sta   :loop+1
                      sty   :loop+2

                      ldy   #0
:loop                 lda   $FFFF,y               ; dummy bytes
                      beq   :done
                      jsr   COUT
                      iny
                      bra   :loop
:done                 rts






LOG                   MAC
                      lda   #]1
                      ldy   #>]1
                      jsr   ConsoleLog
                      <<<

_consoleBottom        =     #23
* Write out to console window
ConsoleLog            pha
                      phy
                      lda   #0                    ;settings to bottom-left of window
                      sta   $24
                      lda   #_consoleBottom-1
                      sta   $25
                      jsr   VTAB
                      lda   #$8D                  ;pre-fix CR
                      jsr   COUT
                      ply
                      pla
                      jsr   PrintString
                      rts

* Set console windowing
WinConsole            lda   #3
                      sta   $20                   ;left edge
                      lda   #75
                      sta   $21                   ;width
                      lda   #17
                      sta   $22                   ;top edge
                      lda   #_consoleBottom
                      sta   $23                   ;bottom edge
                      rts

* Set info windowing
WinInfo               lda   #52
                      sta   $20                   ;left edge
                      lda   #26
                      sta   $21                   ;width
                      lda   #5
                      sta   $22                   ;top edge
                      lda   #16
                      sta   $23                   ;bottom edge
                      rts

* Restore full screen windowing
WinFull               stz   $20
                      stz   $22
                      lda   #80
                      sta   $21
                      lda   #24
                      sta   $23
                      rts

