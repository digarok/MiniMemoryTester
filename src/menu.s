
***  MENU LIBRARY
sizeof_ItemStruct            =     #6
*MyItem    hex 19,07            ;x,y positions
*          db  Menu_TypeList  ;type of input (see Menu_Inputs)
*          db  11               ;max size in bytes
*          da  MyItemOptions    ;params definition & storage

** MENU USES ZP $F0-$F1 as ptr to MenuDefs
Menu_InitMenu                sta   $F0
                             stx   $F1
                             rts




Menu_PrevItem                dec   Menu_ItemSelected
                             bpl   :noflip
                             lda   #MainMenuItems
                             dec
                             sta   Menu_ItemSelected
:noflip                      rts


Menu_NextItem                inc   Menu_ItemSelected
                             lda   Menu_ItemSelected
                             cmp   #MainMenuItems
                             bcc   :noflip
                             lda   #0
                             sta   Menu_ItemSelected
:noflip                      rts


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

                             asl                              ; *2 for table index
                             tax
                             pea   #:nextMenuItem-1           ; push this address to return from jmp tbl, yay 65816
                             jmp   (Menu_DrawRoutinesTbl,x)

:nextMenuItem                lda   _menuOptionPtr
                             clc
                             adc   #sizeof_ItemStruct         ;len of "struct"
                             sta   _menuOptionPtr
                             bra   :drawOption
:menuDone                    rts

Menu_DrawRoutinesTbl         da    Menu_DrawOptionHex         ;char
                             da    Menu_DrawOptionHex
                             da    Menu_DrawOptionAction
                             da    Menu_DrawOptionList
                             da    Menu_DrawOptionBool
                             da    Menu_DrawOptionBin
                             da    Menu_DrawOptionInt
                             da    Menu_DrawOptionHexByteOrder
Menu_DrawOptionBin           iny
                             lda   ($F0),y                    ;get len
                             sta   _menuOptionLen
                             iny
                             lda   ($F0),y                    ;get da
                             sta   $F2                        ;storez
                             iny
                             lda   ($F0),y                    ;get da
                             sta   $F3                        ;storez
                             ldy   #0

:prloop                      lda   ($F2),y
                                                              ; print 4 bits
                             phy                              ; for safety.   might not be needed
                             asl
                             jsr   PRCARRYBIT
                             asl
                             jsr   PRCARRYBIT
                             asl
                             jsr   PRCARRYBIT
                             asl
                             jsr   PRCARRYBIT
                             pha
                             lda   #" "
                             jsr   COUT
                             pla
                             asl
                             jsr   PRCARRYBIT
                             asl
                             jsr   PRCARRYBIT
                             asl
                             jsr   PRCARRYBIT
                             asl
                             jsr   PRCARRYBIT

                             ply
                             iny
                             cpy   _menuOptionLen
                             beq   :done
                             lda   #" "                       ;print space between octets of bits
                             jsr   COUT
                             bra   :prloop
:done                        rts
PRCARRYBIT                   pha
                             bcs   :is1
:is0                         lda   #"0"
                             bra   :out
:is1                         lda   #"1"
:out                         jsr   COUT
                             pla
                             rts


Menu_DrawOptionBool          iny
                             lda   ($F0),y                    ;get len
                             sta   _menuOptionLen
                             iny
                             lda   ($F0),y                    ;get da
                             sta   $F2                        ;storez
                             iny
                             lda   ($F0),y                    ;get da
                             sta   $F3                        ;storez
                             ldy   #0
:prloop                      lda   ($F2),y                    ;get our bool value
                             bne   :on
:off                         lda   #_menuBoolOffStr
                             ldy   #>_menuBoolOffStr
                             jmp   PrintString                ;auto-rts
:on                          lda   #_menuBoolOnStr
                             ldy   #>_menuBoolOnStr
                             jmp   PrintString                ;auto-rts

_menuBoolOnStr               asc   " on",00
_menuBoolOffStr              asc   "off",00
* @todo make this more configurable

Menu_DrawOptionInt           iny
                             lda   ($F0),y                    ;get len
                             sta   _menuOptionLen
                             iny
                             lda   ($F0),y                    ;get da
                             sta   $F2                        ;storez
                             iny
                             lda   ($F0),y                    ;get da
                             sta   $F3                        ;storez

                             ldy   #0
                             lda   ($F2),y
                             tax
                             iny
                             lda   ($F2),y
                             tay


                                                              ;convert to BCD
                             jsr   BINtoBCD


                             stx   _menuBCDInt+1
                             sty   _menuBCDInt



                             ldy   #0
                             ldx   #0
                             lda   _menuOptionLen
                             lsr
                             bcs   :oddmax

:prloop                      lda   _menuBCDInt,y
                             lsr
                             lsr
                             lsr
                             lsr
                             phy
                             phx
                             jsr   PRHEX
                             plx
                             ply
                             inx
:oddmax                      lda   _menuBCDInt,y
                             and   #$0F
                             phy
                             phx
                             jsr   PRHEX
                             plx
                             ply
                             inx
                             iny
                             cpx   _menuOptionLen
                             bne   :prloop
                             rts
_menuBCDInt                  ds    4
Menu_DrawOptionHex           iny
                             lda   ($F0),y                    ;get len
                             sta   _menuOptionLen
                             iny
                             lda   ($F0),y                    ;get da
                             sta   $F2                        ;storez
                             iny
                             lda   ($F0),y                    ;get da
                             sta   $F3                        ;storez
                             ldy   #0
:prloop                      lda   ($F2),y
                             jsr   PRBYTE
                             iny
                             cpy   _menuOptionLen
                             bne   :prloop
                             rts
Menu_DrawOptionHexByteOrder  iny
                             lda   ($F0),y                    ;get len
                             sta   _menuOptionLen
                             iny
                             lda   ($F0),y                    ;get da
                             sta   $F2                        ;storez
                             iny
                             lda   ($F0),y                    ;get da
                             sta   $F3                        ;storez
                             ldy   _menuOptionLen             ;we start at n-1 and work down to 0
:prloop                      dey
                             lda   ($F2),y
                             jsr   PRBYTE
                             cpy   #0
                             bne   :prloop
                             rts

Menu_DrawOptionAction        iny
                             iny
                             lda   ($F0),y
                             tax
                             iny
                             lda   ($F0),y
                             tay
                             txa
                             jsr   PrintString
                             rts

Menu_DrawOptionList          iny                              ;point to da
                             iny
                             lda   ($F0),y
                             sta   $F2
                             iny
                             lda   ($F0),y
                             sta   $F3                        ;now ($2) points to item list structure
                             ldy   #0
                             lda   ($F2),y                    ;selected index
                             asl
                             inc
                             inc                              ;add 2 to reach table of addresses
                             tay
                             lda   ($F2),y
                             pha

                             iny
                             lda   ($F2),y
                             tay
                             pla
                             jsr   PrintString
                             rts

_menuOptionLen               dw    0
_menuOptionPtr               dw    00
Menu_UndrawSelectedAll
                             stz   _stash

:undrawLoop                  ldy   _stash                     ;struct ptr

                             lda   ($F0),y
                             beq   :stop

                             dec                              ;move left 1 space
                             sta   _menuLBracketX
                             iny
                             lda   ($F0),y                    ;next param, y value
                             sta   _menuSelectedY
                             iny
                             lda   ($F0),y                    ;next param, type
                             tax
                             iny
                             lda   ($F0),y                    ;next param, size (bytes)
                             jsr   Menu_GetItemScreenWidth    ;get the real width
                             inc                              ;add 1
                             clc
                             adc   _menuLBracketX             ;add the left bracket position
                             sta   _menuRBracketX             ;and we should be in the right place
                             jsr   Menu_UndrawBrackets

                             lda   _stash
                             clc
                             adc   #sizeof_ItemStruct
                             sta   _stash
                             bra   :undrawLoop


:stop
                             rts

* max 256 byte struct table unless i go 16 bit
Menu_GetSelectedStructPtr    lda   #0
                             ldx   Menu_ItemSelected
:check                       beq   :foundIdx
                             clc
                             adc   #sizeof_ItemStruct         ;"struct" size
                             dex
                             bra   :check
:foundIdx                    rts

** RETURN THE SCREEN WIDTH FOR VARIOUS INPUT TYPES
* X= ItemType  A= SizeInBytes
Menu_GetItemScreenWidth
                             cpx   Menu_TypeChar
                             bne   :notChar
                             rts                              ;size already correct for char
:notChar                     cpx   Menu_TypeHex
                             bne   :notHex
                             asl                              ;*2 for printing 2 char per byte
                             rts
:notHex                      cpx   Menu_TypeBin
                             bne   :notBin
                             asl                              ; logic for binary is a little more detailed
                             asl                              ; because i add spacing for readability
                             asl                              ;*8 for byte
                             inc                              ; add a space so "0000 0000"
                             cmp   #9
                             bne   :bigger
                             rts
:bigger                      inc
                             inc                              ; add 2 more spaces.
                             rts
:notBin                      cpx   Menu_TypeInt
                             bne   :notInt
                             rts                              ;input width... internally maxint = FFFF
:notInt                      bne   :notAction
                             rts                              ;should be defined in param from string length
:notAction                   cpx   Menu_TypeList
                             bne   :notList
                             rts                              ;should be defined in param from string length
:notList                     cpx   Menu_TypeBool
                             bne   :notBool
                             lda   #3                         ;@todo: we'll use "off"/"on" for now.. revisit?
                             rts                              ;hmm.. undefined?   @TODO!!!
:notBool                     cpx   Menu_TypeHexByteOrder
                             bne   :notHexByteOrder
                             asl                              ;*2 for printing 2 char per byte
                             rts
:notHexByteOrder
:wtf
                             rts

Menu_HighlightSelected       jsr   Menu_GetSelectedStructPtr  ;get ptr to selected item
                             tay
                             lda   ($F0),y                    ;start parsing the struct with x value
                             dec                              ;move left 1 space
                             sta   _menuLBracketX
                             iny
                             lda   ($F0),y                    ;next param, y value
                             sta   _menuSelectedY
                             iny
                             lda   ($F0),y                    ;next param, type
                             tax
                             iny
                             lda   ($F0),y                    ;next param, size (bytes)
                             jsr   Menu_GetItemScreenWidth    ;get the real width
                             inc                              ;add 1
                             clc
                             adc   _menuLBracketX             ;add the left bracket position
                             sta   _menuRBracketX             ;and we should be in the right place
                             jsr   Menu_DrawBrackets
                             rts

Menu_UndrawBrackets          ldx   _menuRBracketX
                             ldy   _menuSelectedY
                             jsr   GoXY
                             lda   #"]"
                             jsr   COUT
                             ldx   _menuLBracketX
                             ldy   _menuSelectedY
                             jsr   GoXY
                             lda   #"["
                             jsr   COUT
                             rts
Menu_DrawBrackets            ldx   _menuRBracketX
                             ldy   _menuSelectedY
                             jsr   GoXY
                             lda   #">"
                             jsr   COUT
                             ldx   _menuLBracketX
                             ldy   _menuSelectedY
                             jsr   GoXY
                             lda   #"<"
                             jsr   COUT
                             rts
_menuLBracketX               db    0
_menuRBracketX               db    0
_menuSelectedX1              db    0
_menuSelectedY               db    0

* THESE ARE ALL OF THE MENU INPUT TYPES
Menu_Inputs
Menu_InputTable              da    Menu_InputChar,Menu_InputHex,Menu_InputAction,Menu_InputList,Menu_InputBool,Menu_InputBin,Menu_InputInt,Menu_InputHexByteOrder
Menu_TypeChar                equ   #0
Menu_TypeHex                 equ   #1
Menu_TypeHexByteOrder        equ   #7
Menu_TypeBin                 equ   #5
Menu_TypeInt                 equ   #6
Menu_TypeAction              equ   #2
Menu_TypeList                equ   #3
Menu_TypeBool                equ   #4


* $0 = ptr->MenuDefs
Menu_HandleSelection
                             lda   #0
                             ldx   Menu_ItemSelected          ;odd choice to load again, but preps flags (z) how i likes it
:check                       beq   :foundIdx                  ;<-  a=struct offset
                             clc
                             adc   #sizeof_ItemStruct         ;"struct" size
                             dex
                             bra   :check

:foundIdx                    pha
                             tay
                             iny                              ;\
                             iny                              ; \
                             lda   ($F0),y                    ;  > get Menu_TypeType, set up for jmp table
                             asl                              ; /
                             tax                              ;/
                             pla
                             jmp   (Menu_InputTable,x)


* A= struct index for all of these.
Menu_InputChar               rts
Menu_InputBool               tay
                             iny                              ;skip x
                             iny                              ;skip y
                             iny                              ;skip length
                             iny

                             lda   ($F0),y
                             sta   $F2
                             iny
                             lda   ($F0),y
                             sta   $F3
                             lda   #1
                             eor   ($f2)
                             sta   ($f2)
                             rts


Menu_InputBin                rts
Menu_InputInt                pha
                             tay
                             lda   ($F0),y
                             tax
                             iny
                             lda   ($F0),y
                             tay
                             jsr   GoXY
                             pla
                             clc
                             adc   #3                         ;get max input length for int
                             tay                              ; |
                             lda   ($F0),y                    ; |
                             pha                              ; store length
                             iny
                             lda   ($F0),y                    ;get low nibble of storage location
                             pha                              ; store low nibble
                             iny
                             lda   ($F0),y                    ;get high nibble of storage location
                             tay
                             plx                              ;get low nibble
                             pla                              ;get length
                             jsr   GetInt
                             rts

Menu_InputHexByteOrder       pha
                             tay
                             lda   ($F0),y
                             tax
                             iny
                             lda   ($F0),y
                             tay
                             jsr   GoXY
                             pla
                             clc
                             adc   #3                         ; ->memory size
                             tay
                             lda   ($F0),y
                             asl                              ;*2
                             pha
                             iny
                             lda   ($F0),y
                             pha
                             iny
                             lda   ($F0),y
                             tay
                             plx
                             pla
                             jsr   GetHexByteOrder
                             rts

Menu_InputHex                pha
                             tay
                             lda   ($F0),y
                             tax
                             iny
                             lda   ($F0),y
                             tay
                             jsr   GoXY
                             pla
                             clc
                             adc   #3                         ; ->memory size
                             tay
                             lda   ($F0),y
                             asl                              ;*2
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

Menu_InputAction             iny                              ;skip len byte
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
:copy                        ldx   #0                         ;this is all so bad - maybe rts is better
:ACTION                      lda   $ffff,x
                             sta   :JSR+1,x
                             inx
                             cpx   #2
                             bcc   :ACTION
:JSR                         jsr   $ffff
                             rts

* Selecting from a List
* look for key
* update cursor
* if up then prev item   \_ draw menu options
* if down then next item /
* if enter, done - when it gets back to menu loop, we should handle special logic there
Menu_InputList
                             tay
                             iny                              ;skip x
                             iny                              ;skip y
                             iny                              ;skip length
                             iny

                             lda   ($F0),y                    ;get ptr to tbl
                             sta   $F2                        ; |
                             iny                              ;
                             lda   ($F0),y                    ;
                             sta   $F3                        ;


                             ldy   #$0

                             lda   ($F2),y                    ;get new current value
                             inc                              ;selecteditem++
                             sta   ($F2),y                    ;store new current value
                             iny
                             cmp   ($F2),y                    ;compare to max
                             bcc   :done                      ;current value < max
                             ldy   #0                         ;reset selected item to 0 (rollover)
                             tya
                             sta   ($F2),y
:done                        rts

*** INPUT LIBRARY FOR MENU
* Pass desired length in A
* x/y= storage area
GetHexByteOrder
                             pha
                             lda   #$1                        ;1=6502 ordering
                             sta   _gethex_byteorder
                             pla
                             bra   GetHexStart

GetHex                       stz   _gethex_byteorder          ;0=linear order (01 23 45 67 etc, in memory)
GetHexStart
                             sta   _gethex_maxlen
                             stx   _gethex_resultptr
                             sty   _gethex_resultptr+1
                             stz   _gethex_current
                             lda   $24
                             sta   _gethex_screenx            ;stash x.  gets clobbered by RDKEY

:input                       jsr   RDKEY

                             cmp   #$9B                       ;esc = abort
                             bne   :notesc
                             rts
:notesc                      cmp   #$FF                       ;del
                             beq   :goBack
                             cmp   #$88
                             bne   :notBack
:goBack
                             lda   _gethex_current
                             beq   :badChar                   ; otherwise result = -1
                             dec   _gethex_current
                             dec   _gethex_screenx
                             GOXY  _gethex_screenx;$25
                             bra   :input
:notBack                     cmp   #"9"+1
                             bcs   :notNum                    ;bge > 9
                             cmp   #"0"
                             bcc   :badChar                   ;
                             sec
                             sbc   #"0"
                             bra   :storeInput
:notNum                      cmp   #"a"
                             bcc   :notLower
                             sec
                             sbc   #$20                       ;ToUpper
:notLower                    cmp   #"A"
                             bcc   :badChar
                             cmp   #"F"+1
                             bcs   :badChar
                             bcc   :gotHex
***
:badChar                     jmp   :input                     ;jmp out of here
***
:gotHex
                             sec
                             sbc   #"A"-10
:storeInput
                             pha
                             jsr   PRHEX
                             pla
                             ldy   _gethex_current
                             sta   _gethex_buffer,y
                             inc   _gethex_screenx
                             iny
                             cpy   #_gethex_internalmax
                             bge   :internalmax
                             cpy   _gethex_maxlen
                             bge   :passedmax
                             sty   _gethex_current
                             bra   :input
:internalmax
:passedmax
                             lda   _gethex_resultptr          ;set destination buffer to result memory
                             sta   $0
                             lda   _gethex_resultptr+1
                             sta   $1
                                                              ;prep to copy back to result
                             lda   _gethex_byteorder
                             beq   :linearcopy
:byteordercopy               ldx   #0
                             lda   _gethex_maxlen
                             lsr                              ;/2 ... took a looong time to find this bug
                             tay
                             dey                              ;we start at n-1 and work down to 0
:copyBufferBackwardsByte     lda   _gethex_buffer,x
                             asl                              ; move to upper nibble
                             asl
                             asl
                             asl
                             sta   ($0),y                     ; store
                             inx
                             lda   _gethex_buffer,x
                             ora   ($0),y
                             sta   ($0),y
                             dey
                             inx
                             cpx   _gethex_maxlen
                             bcc   :copyBufferBackwardsByte
                             rts



:linearcopy                  ldx   #0
                             ldy   #0
:copyBuffer                  lda   _gethex_buffer,x
                             asl                              ; move to upper nibble
                             asl
                             asl
                             asl
                             sta   ($0),y                     ; store
                             inx
                             lda   _gethex_buffer,x
                             ora   ($0),y
                             sta   ($0),y
                             iny
                             inx
                             cpx   _gethex_maxlen
                             bcc   :copyBuffer
                             rts



_gethex_internalmax          equ   8
_gethex_resultptr            da    0000
_gethex_maxlen               db    1
_gethex_current              db    0
_gethex_buffer               ds    _gethex_internalmax
_gethex_screenx              db    0
_gethex_byteorder            db    0                          ;0=linear, 1=6502 address byte order.. Least significant byte first.

*** INPUT LIBRARY FOR MENU
* Pass desired length in A
* x/y= storage area
GetInt
                             sta   _getint_maxlen
                             stx   _getint_resultptr
                             sty   _getint_resultptr+1
                             stz   _getint_current
                             lda   $24
                             sta   _getint_screenx            ;stash x.  gets clobbered by RDKEY
                             ldx   _getint_maxlen
:clearbuffer                 dex                              ;we need to zero our buffer
                             stz   _getint_buffer,x           ;
                             bne   :clearbuffer               ;

:input                       jsr   RDKEY

                             cmp   #KEY_ESC                   ;9B = abort
                             bne   :notesc
                             rts
:notesc                      cmp   #KEY_DEL                   ;del
                             beq   :goBack
                             cmp   #KEY_LTARROW
                             bne   :notBack
:goBack
                             lda   _getint_current
                             beq   :badChar                   ; otherwise result = -1
                             dec   _getint_current
                             dec   _getint_screenx
                             GOXY  _getint_screenx;$25
                             bra   :input
:notBack                     cmp   #"9"+1
                             bcs   :badChar                   ;bge > 9
                             cmp   #"0"
                             bcc   :badChar                   ;
                             sec
                             sbc   #"0"
                             bra   :storeInput
:badChar                     jmp   :input
:storeInput
                             pha
                             jsr   PRHEX
                             pla
                             ldy   _getint_current
                             sta   _getint_buffer,y
                             inc   _getint_screenx
                             iny
                             cpy   #_getint_internalmax
                             bge   :internalmax
                             cpy   _getint_maxlen
                             bge   :passedmax
                             sty   _getint_current
                             bra   :input
:internalmax
:passedmax
                             lda   _getint_resultptr
                             sta   $0
                             lda   _getint_resultptr+1
                             sta   $1
                             ldy   #0                         ;clear any existing data (otherwise OR mask error with odd input lengths)
                             tya
                             sta   ($0),y
                             iny
                             sta   ($0),y
                             ldx   #0
                             ldy   #0
                             lda   _getint_maxlen
                             lsr
                             bcs   :oddmax
:copyBuffer                  lda   _getint_buffer,x
                             asl                              ; move to upper nibble
                             asl
                             asl
                             asl
                             sta   ($0),y                     ; store
                             inx
:oddmax                      lda   _getint_buffer,x
                             ora   ($0),y
                             sta   ($0),y
                             iny
                             inx
                             cpx   _getint_maxlen
                             bcc   :copyBuffer                ;done copying to buffer like "0123" (still decimal)

                             ldy   #$1                        ;@todo this is all wonky and a bit hackish
                             lda   ($0),y
                             tax
                             dey
                             lda   ($0),y
                             tay
                             jsr   BCDtoBIN
                             tya
                             ldy   #1
                             sta   ($0),y
                             dey
                             txa
                             sta   ($0),y
                             rts



_getint_internalmax          equ   4
_getint_resultptr            da    0000
_getint_maxlen               db    1
_getint_current              db    0
_getint_buffer               ds    _getint_internalmax
_getint_screenx              db    0




* x/y = high/low
BINtoBCD                     stx   BIN
                             sty   BIN+1
                             jsr   BINBCD16
                             ldx   BCD
                             ldy   BCD+1
                             rts
BCDtoBIN
                             stx   BCD
                             sty   BCD+1
                             jsr   BCDBIN16
                             ldx   BIN
                             ldy   BIN+1
                             rts

BIN                          dw    $0000
BCD                          ds    3

BINBCD16                     SED                              ; Switch to decimal mode
                             LDA   #0                         ; Ensure the result is clear
                             STA   BCD+0
                             STA   BCD+1
                             STA   BCD+2
                             LDX   #16                        ; The number of source bits

:CNVBIT                      ASL   BIN+0                      ; Shift out one bit
                             ROL   BIN+1
                             LDA   BCD+0                      ; And add into result
                             ADC   BCD+0
                             STA   BCD+0
                             LDA   BCD+1                      ; propagating any carry
                             ADC   BCD+1
                             STA   BCD+1
                             LDA   BCD+2                      ; ... thru whole result
                             ADC   BCD+2
                             STA   BCD+2
                             DEX                              ; And repeat for next bit
                             BNE   :CNVBIT
                             CLD                              ; Back to binary
                             rts


* 16-bit mode!!!
BCDBIN16
                             clc
                             xce
                             rep   #$30
                             stz   BIN
                             lda   BCD
                             and   #$000F                     ;get 1's
                             sta   BIN
                             lda   BCD
                             and   #$00F0                     ;get 10's
                             lsr
                             lsr
                             lsr
                             lsr
                             jsr   TIMES10
                             clc
                             adc   BIN                        ;add 10's back to BIN
                             sta   BIN
                             lda   BCD
                             and   #$0f00                     ;get 100's
                             xba
                             jsr   TIMES10
                             jsr   TIMES10
                             clc
                             adc   BIN
                             sta   BIN
                             lda   BCD
                             and   #$f000                     ;get 1000's
                             xba
                             lsr
                             lsr
                             lsr
                             lsr
                             jsr   TIMES10
                             jsr   TIMES10
                             jsr   TIMES10
                             clc
                             adc   BIN
                             sta   BIN
                             sep   #$30
                             rts

                             mx    %00
* 16-bit mode!!!
TIMES10
                             sta   :tensadd+1
                             ldx   #9                         ;9 loops since because initial value already in M
:tensloop                    clc
:tensadd                     adc   #$0000                     ;placeholder, gets overwritten above
                             dex
                             bne   :tensloop
                             rts


                             mx    %11
BINBCDVARDUMP
                             lda   BIN+1
                             jsr   PRBYTE
                             lda   BIN
                             jsr   PRBYTE
                             lda   #" "
                             jsr   COUT
                             lda   BCD+2
                             jsr   PRBYTE
                             lda   BCD+1
                             jsr   PRBYTE
                             lda   BCD
                             jsr   PRBYTE
                             jsr   RDKEY
                             rts



























































