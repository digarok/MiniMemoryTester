
***  MENU LIBRARY

** MENU USES ZP $F0-$F1 as ptr to MenuDefs
Menu_InitMenu          sta   $F0
                       stx   $F1
                       rts




Menu_PrevItem          dec   Menu_ItemSelected
                       bpl   :noflip
                       lda   #MainMenuItems
                       dec
                       sta   Menu_ItemSelected
:noflip                rts


Menu_NextItem          inc   Menu_ItemSelected
                       lda   Menu_ItemSelected
                       cmp   #MainMenuItems
                       bcc   :noflip
                       lda   #0
                       sta   Menu_ItemSelected
:noflip                rts


Menu_DrawOptions

                       stz   _menuOptionPtr
:drawOption
                       ldy   _menuOptionPtr
                       lda   ($F0),y
                       beq   :menuDone
                       tax
                       iny
                       lda   ($F0),y
                       tay
                       jsr   GoXY
                       ldy   _menuOptionPtr
                       iny
                       iny
                       lda   ($F0),y
                       beq   :charItem
                       cmp   #1
                       beq   :hexItem
                       cmp   #2
                       beq   :jsrItem
                       cmp   #3
                       beq   :listItem
                       cmp   #4
                       beq   :boolItem
:charItem
:boolItem
:hexItem               jsr   Menu_DrawOptionHex
                       bra   :nextMenuItem
:listItem              jsr   Menu_DrawOptionList
                       bra   :nextMenuItem
:jsrItem               jsr   Menu_DrawOptionAction
                       bra   :nextMenuItem

:nextMenuItem
                       lda   _menuOptionPtr
                       clc
                       adc   #6                     ; len of "struct"
                       sta   _menuOptionPtr
                       bra   :drawOption
:menuDone
                       rts

Menu_DrawOptionHex     iny
                       lda   ($F0),y                ; get len
                       sta   _menuHexIdx
                       iny
                       lda   ($F0),y                ; get da
                       sta   $F2                    ; storez
                       iny
                       lda   ($F0),y                ; get da
                       sta   $F3                    ; storez
                       ldy   #0
:prloop                lda   ($F2),y
                       jsr   PRBYTE
                       iny
                       cpy   _menuHexIdx
                       bne   :prloop
                       rts

Menu_DrawOptionAction  iny
                       iny
                       lda   ($F0),y
                       tax
                       iny
                       lda   ($F0),y
                       tay
                       txa
                       jsr   PrintString
                       rts

Menu_DrawOptionList    iny                          ; point to da
                       iny
                       lda   ($F0),y
                       sta   $F2
                       iny
                       lda   ($F0),y
                       sta   $F3                    ; now ($2) points to item list structure
                       ldy   #0
                       lda   ($F2),y                ; selected index
                       asl
                       inc
                       inc                          ; add 2 to reach table of addresses
                       tay
                       lda   ($F2),y
                       pha

                       iny
                       lda   ($F2),y
                       tay
                       pla
                       jsr   PrintString
                       rts

_menuHexIdx            dw    0
_menuOptionPtr         dw    00
Menu_UndrawSelected
                       stz   _stash

:undrawLoop            ldy   _stash                 ; struct ptr
                       lda   ($F0),y
                       beq   :stop
                       dec                          ; x-- (left bracket)
                       sta   _menuSelectedX1
                       iny
                       lda   ($F0),y
                       sta   _menuSelectedY
                       iny
                       lda   ($F0),y
                       bne   :notChar
                       iny
                       lda   ($F0),y
                       inc                          ;doit
                       clc
                       adc   _menuSelectedX1
                       tax
                       bra   :rightBracket

:notChar               cmp   #1
                       bne   :notHex
                       iny
                       lda   ($F0),y
                       asl
                       inc                          ;doit
                       clc
                       adc   _menuSelectedX1
                       tax
                       bra   :rightBracket

:notHex                cmp   #2
                       bne   :notAction
                       iny
                       lda   ($F0),y

                       inc
                       clc
                       adc   _menuSelectedX1
                       tax
                       bra   :rightBracket


:notAction
                       cmp   #3
                       bne   :wtf
                       iny
                       lda   ($F0),y

                       inc
                       clc
                       adc   _menuSelectedX1
                       tax
                       bra   :rightBracket

:wtf

:rightBracket
                       ldy   _menuSelectedY
                       jsr   GoXY
                       lda   #" "
                       jsr   COUT
:leftBracket           ldx   _menuSelectedX1
                       ldy   _menuSelectedY
                       jsr   GoXY
                       lda   #" "
                       jsr   COUT
                       lda   _stash
                       clc
                       adc   #6
                       sta   _stash
                       bra   :undrawLoop
:stop
                       rts



Menu_DrawSelected
                       lda   #0
                       ldx   Menu_ItemSelected
:check                 beq   :foundIdx
                       clc
                       adc   #6                     ; "struct" size
                       dex
                       bra   :check

:foundIdx              tay
                       lda   ($F0),y
                       dec                          ; x-- (left bracket)
                       sta   _menuSelectedX1
                       iny
                       lda   ($F0),y
                       sta   _menuSelectedY
                       iny
                       lda   ($F0),y
                       bne   :notChar
                       iny
                       lda   ($F0),y
                       inc                          ;doit
                       clc
                       adc   _menuSelectedX1
                       tax
                       bra   :rightBracket

:notChar               cmp   #1
                       bne   :notHex
                       iny
                       lda   ($F0),y
                       asl
                       inc                          ;doit
                       clc
                       adc   _menuSelectedX1
                       tax
                       bra   :rightBracket

:notHex                cmp   #2
                       bne   :notAction
                       iny
                       lda   ($F0),y

                       inc
                       clc
                       adc   _menuSelectedX1
                       tax
                       bra   :rightBracket


:notAction
                       cmp   #3
                       bne   :wtf
                       iny
                       lda   ($F0),y

                       inc
                       clc
                       adc   _menuSelectedX1
                       tax
                       bra   :rightBracket

:wtf

:rightBracket
                       ldy   _menuSelectedY
                       jsr   GoXY
                       lda   #"]"
                       jsr   COUT
:leftBracket           ldx   _menuSelectedX1
                       ldy   _menuSelectedY
                       jsr   GoXY
                       lda   #"["
                       jsr   COUT

                       rts
_menuSelectedX1        db    0                      ; no x2 cuz we be addin' dat offset
_menuSelectedY         db    0

MenuOption_Char        equ   #0
MenuOption_Hex         equ   #1
MenuOption_Action      equ   #2
MenuOption_List        equ   #3
MenuOption_Bool        equ   #4
Menu_TypeTable         da    Menu_TypeChar,Menu_TypeHex,Menu_TypeAction,Menu_TypeList,Menu_TypeBool

* $0 = ptr->MenuDefs
Menu_HandleSelection
                       lda   #0
                       ldx   Menu_ItemSelected      ; odd choice to load again, but preps flags (z) how i likes it
:check                 beq   :foundIdx              ; <-  a=struct offset
                       clc
                       adc   #6                     ; "struct" size
                       dex
                       bra   :check

:foundIdx              pha
                       tay
                       iny                          ;\ 
                       iny                          ; \  
                       lda   ($F0),y                ;  > get MenuOption_Type, set up for jmp table
                       asl                          ; /
                       tax                          ;/ 
                       pla
                       jmp   (Menu_TypeTable,x)

Menu_TypeChar          rts
Menu_TypeBool          rts

Menu_TypeHex           pha
                       tay
                       lda   ($F0),y
                       tax
                       iny
                       lda   ($F0),y
                       tay
                       jsr   GoXY
                       pla
                       clc
                       adc   #3                     ; ->memory size
                       tay
                       lda   ($F0),y
                       asl                          ;*2
                       pha
                       iny
                       lda   ($F0),y
                       pha
                       iny
                       lda   ($F0),y
                       tay
                       plx
                       pla
                       jsr   GetHex
                       rts

Menu_TypeAction        iny                          ; skip len byte
                       iny
                       lda   ($F0),y
                       sta   :ACTION+1
                       iny
                       lda   ($F0),y
                       sta   :ACTION+2
                       lda   :ACTION+1
                       sec
                       sbc   #2
                       sta   :ACTION+1
                       bcs   :copy
                       dec   :ACTION+2
:copy                  ldx   #0                     ; this is all so bad
:ACTION                lda   $ffff,x
                       sta   :JSR+1,x
                       inx
                       cpx   #2
                       bcc   :ACTION
:JSR                   jsr   $ffff
                       rts

* Selecting from a List
* look for key
* update cursor
* if up then prev item   \_ draw menu options
* if down then next item / 
* if enter, done - when it gets back to menu loop, we should handle special logic there
Menu_TypeList
                       rts
*** INPUT LIBRARY FOR MENU
* Pass desired length in A
GetHex                 sta   _gethex_maxlen
                       stx   _gethex_resultptr
                       sty   _gethex_resultptr+1
                       stz   _gethex_current

:input                 jsr   RDKEY
                       cmp   #$9B                   ;esc = abort
                       bne   :notesc
                       rts
:notesc                cmp   #"9"+1
                       bcs   :notNum                ;bge > 9
                       cmp   #"0"
                       bcc   :badChar               ;
                       sec
                       sbc   #"0"
                       bra   :storeInput
:notNum                cmp   #"a"
                       bcc   :notLower
                       sec
                       sbc   #$20                   ; ToUpper
:notLower              cmp   #"A"
                       bcc   :badChar
                       cmp   #"F"+1
                       bcs   :badChar
:gotHex
                       sec
                       sbc   #"A"-10
:storeInput
                       pha
                       jsr   PRHEX
                       pla
                       ldy   _gethex_current
                       sta   _gethex_buffer,y
                       iny
                       cpy   #_gethex_internalmax
                       bge   :internalmax
                       cpy   _gethex_maxlen
                       bge   :passedmax
                       sty   _gethex_current
                       bra   :input
:internalmax
:passedmax
                       lda   _gethex_resultptr
                       sta   $0
                       lda   _gethex_resultptr+1
                       sta   $1
                       ldx   #0
                       ldy   #0
:copyBuffer            lda   _gethex_buffer,x
                       asl                          ; move to upper nibble
                       asl
                       asl
                       asl
                       sta   ($0),y                 ; store
                       inx
                       lda   _gethex_buffer,x
                       ora   ($0),y
                       sta   ($0),y
                       iny
                       inx
                       cpx   _gethex_maxlen
                       bcc   :copyBuffer
                       rts

:badChar               bra   :input

_gethex_internalmax    equ   8
_gethex_resultptr      da    0000
_gethex_maxlen         db    1
_gethex_current        db    0
_gethex_buffer         ds    _gethex_internalmax
PrHexChar              jsr   HexCharForByte

HexCharForByte
                       cmp   #9
                       bcs   :alpha
:number                clc
                       adc   #"0"
                       rts
:alpha                 clc
                       adc   #"A"
                       rts



