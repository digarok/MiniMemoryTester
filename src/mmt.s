****************************************
* MiniMemoryTester                     *
*                                      *
*  Dagen Brock <dagenbrock@gmail.com>  *
*  2015-09-16                          *
****************************************

                           org          $2000                         ; start at $2000 (all ProDOS8 system files)
                           typ          $ff                           ; set P8 type ($ff = "SYS") for output file
                           dsk          mtsystem                      ; tell compiler what name for output file
                           put          applerom

Init
                           clc
                           xce                                        ;enable full 65816
                           LDA          #$A0                          ;USE A BLANK SPACE TO
                           JSR          $C300                         ;TURN ON THE VIDEO FIRMWARE

                           lda          $C034                         ; save border color
                           sta          BorderColor

                           jsr          DetectRam
                           lda          BankExpansionLowest
                           sta          StartBank
                           lda          BankExpansionHighest
                           sta          EndBank

                           lda          #MainMenuDefs
                           ldx          #>MainMenuDefs
                           jsr          Menu_InitMenu

*
* Main Menu loop begin2
*
Main
:menuLoop                  jsr          DrawMenuBackground
                           jsr          DrawRomMessage
                           jsr          DrawRamMessages

                           jsr          LogWelcomeMessage
                           jsr          LogRamMessages

:menuDrawOptionsLoop       jsr          MenuUpdateConfig              ;always update this before draw in case of change
                           lda          #MainMenuDefs
                           ldy          #>MainMenuDefs
                           jsr          Menu_DrawOptions
:menuNoDrawLoop            jsr          MenuCheckKeyColor
                           bcc          :menuNoDrawLoop               ;hmm?
:keyHit                    cmp          #KEY_ENTER                    ;8D
                           bne          :check1
:enter                     jsr          Menu_HandleSelection
                           bra          :menuDrawOptionsLoop          ;because an option might have changed

:check1                    cmp          #KEY_UPARROW                  ;8B
                           beq          :prevItem
                           cmp          #KEY_LTARROW                  ;88
                           beq          :prevItem
                           cmp          #KEY_DNARROW                  ;8A
                           beq          :nextItem
                           cmp          #KEY_RTARROW                  ;95
                           beq          :nextItem
:unknownKey                bra          :menuNoDrawLoop
:prevItem                  jsr          Menu_PrevItem
                           jsr          Menu_UndrawSelectedAll        ;hack for blinky cursor
                           stz          _ticker
                           bra          :menuNoDrawLoop
:nextItem                  jsr          Menu_NextItem
                           jsr          Menu_UndrawSelectedAll        ;hack for blinky cursor
                           stz          _ticker
                           bra          :menuNoDrawLoop
*
* Main Menu loop end ^^^
*




DrawMenuBackground         jsr          HOME
                           lda          #MainMenuStrs
                           ldy          #>MainMenuStrs
                           ldx          #00                           ; horiz pos
                           jsr          PrintStringsX
                           rts

* Prints "Apple IIgs ROM 0x"
DrawRomMessage
                           PRINTXY      #54;#05;Mesg_Rom
                           lda          GSROM
                           jsr          PRBYTE
                           rts

* Prints "Built-In RAM  xxxK"
*        "Expansion RAM yyyyK"
DrawRamMessages
                           lda          GSROM
                           cmp          #3
                           bne          :rom0or1
:rom3                      PRINTXY      #54;#06;Mesg_InternalRam1024
                           bra          :drawExpansionMessage
:rom0or1                   PRINTXY      #54;#06;Mesg_InternalRam256
:drawExpansionMessage      PRINTXY      #54;#07;Mesg_ExpansionRam
                           ldx          #BankExpansionRamKB
                           ldy          #>BankExpansionRamKB
                           jsr          PrintInt
                           lda          #"K"
                           jsr          COUT
                           rts

LogWelcomeMessage          jsr          WinConsole
                           LOG          Mesg_Welcome
                           jsr          WinFull
                           rts

LogRamMessages             jsr          WinConsole
                           LOG          Mesg_DetectedBanks

                           lda          BankExpansionLowest
                           jsr          PRBYTE
                           lda          #Mesg_ToBank
                           ldy          #>Mesg_ToBank
                           jsr          PrintString
                           lda          BankExpansionHighest
                           jsr          PRBYTE
                           jsr          WinFull
                           rts
LogTestDone                jsr          WinConsole
                           LOG          Mesg_Done
                           jsr          WinFull
                           rts





*
*    #######                                      ###    ###    ###
*       #    ######  ####  ##### ###### #####     ###    ###    ###
*       #    #      #        #   #      #    #    ###    ###    ###
*       #    #####   ####    #   #####  #    #     #      #      #
*       #    #           #   #   #      #####
*       #    #      #    #   #   #      #   #     ###    ###    ###
*       #    ######  ####    #   ###### #    #    ###    ###    ###
*
*
TestInit
                           PRINTXY      #$34;#$E;_clearstring
                           jsr          WinConsole
                           LOG          Mesg_Starting
                           jsr          WinFull
                           sei                                        ; disable interrupts
                           stz          _testErrors
                           stz          _testIteration
                           inc          _testIteration                ;actually, set to 1.  let test passes be indicated in natural numbers.  see, i'm not such a bad guy.
                           stz          _testIteration+1
                           stz          _testState
                           stz          _walkState

TestMasterLoop             clc
                           xce
                           rep          #$10                          ;long x/y

                           ldy          StartAddr
                           ldx          EndAddr
                           stz          CurBank

                           jsr          TestPrintIteration
                           jsr          TestPrintErrors               ;just to get it drawn
:NextBank
                           jsr          TestSetState                  ;sets read/write/both
                           jsr          TestForceUpdateStatus         ;print last tested address before we advance banks
                           jsr          TestGetNextBank               ;sets initial bank when CurBank = 0
                           jsr          TestPatchBanks                ;patches code for whatever CurBank is set to
                           jsr          TestPastFinalBank
                           bcs          :NextIteration

                           jsr          TestPrintState
                           jsr          TestGetStartAddress

                           jsr          TestForceUpdateStatus         ;print last tested address before we advance banks
                           stz _updateTick
                           stz _updateTick+1


:TestLoop                                                             ;THIS IS IT!
                           lda          $C000
                           bmi          TestKeyHandler
KeyHandled
                           jsr          TestMemoryLocation
                           jsr          TestUpdateStatus
                           jsr          TestAdvanceLocation
                           bcc          :TestLoop
                           bcs          :NextBank



:NextIteration             inc          _testIteration                ;see if we've done enough tests
                           lda          TestIterations
                           beq          :infiniteIterations           ;0=infinite
                           cmp          _testIteration
                           bcc          :testComplete
:infiniteIterations        lda          TestTwoPass
                           beq          :notwopass

                           stz          _testState                    ;hack to reset for two pass, will switch it back to write when it sees zero.  i hope. (update: WORKS! BOOM!)

:notwopass                 jmp          TestMasterLoop
TestAbort                  jsr          TestForceUpdateStatus         ;print last test address
:testComplete              sep          #$10
                           jsr          LogTestDone
                           rts
Mesg_Done                  asc          "DONE WITH TEST",$8D,00


TestKeyHandler             sta          $C010
                           jsr          ToLower
                           cmp          #"q"
                           beq          TestAbort
                           cmp          #KEY_ESC
                           beq          TestAbort
                           cmp          #"p"
                           beq          :pause
                           jmp          KeyHandled
:pause
:nokey                     lda          $C000
                           bpl          :nokey
                           sta          $C010
                           jmp          KeyHandled




* This should just flip-flop, no matter what, on TwoPass mode... otherwise W/R (BOTH in one pass)
                           mx           %10
TestSetState               lda          TestTwoPass                   ;read pass then write pass?
                           bne          :twopass
                           lda          #TESTSTATE_BOTH               ;r&w
                           sta          _testState
                           rts
:twopass                   lda          _testState
                           beq          :setWrite                     ;0 check for initial value
                           cmp          #TESTSTATE_READ
                           beq          :setWrite
                           lda          #TESTSTATE_READ
                           sta          _testState
                           rts
:setWrite                  lda          #TESTSTATE_WRITE              ;otherwise, start with write pass
                           sta          _testState
                           rts


* Prints whether "Reading", "Writing", or "W/R" (Both)
TestPrintState             PushAll
                           sep          #$10
                           lda          _testState
:check1                    cmp          #TESTSTATE_WRITE
                           bne          :check2
                           PRINTXY      #53;#12;Mesg_Writing
                           bra          :done
:check2                    cmp          #TESTSTATE_READ
                           bne          :check3
                           PRINTXY      #53;#12;Mesg_Reading
                           bra          :done
:check3                    cmp          #TESTSTATE_BOTH
                           bne          :done
                           PRINTXY      #53;#12;Mesg_WR
:done                      clc
                           xce
                           rep          #$10
                           PopAll
                           rts

* Prints current test pass string
TestPrintIteration         PushAll
                           sep          #$10
                           PRINTXY      #53;#10;Mesg_TestPass
                           ldx          #_testIteration
                           ldy          #>_testIteration
                           jsr          PrintInt
                           clc
                           xce
                           rep          #$10
                           PopAll
                           rts

* Prints current test error counts string
TestPrintErrors            PushAll
                           sep          #$10
                           PRINTXY      #53;#11;Mesg_Errors
                           ldx          #_testErrors
                           ldy          #>_testErrors
                           jsr          PrintInt
                           clc
                           xce
                           rep          #$10
                           PopAll
                           rts

* Pauses on test error (when enabled, otherwise immediately returns)
TestPauseError             lda          TestErrorPause                ;is this option enabled?
                           beq          :no
                           PushAll
                           sep          #$10
                           PRINTXY      #55;#14;Mesg_TestError1       ;draw messages
                           PRINTXY      #55;#15;Mesg_TestError2

:pause                     lda          $C000                         ;wait for key
                           bpl          :pause
                           sta          $C010

                           PRINTXY      #55;#14;Mesg_Blank            ;undraw messages
                           PRINTXY      #55;#15;Mesg_Blank
                           clc
                           xce
                           rep          #$10
                           PopAll
:no                        rts

* Print a console error detailing the spot in memory where the error occured
TestLogError               PushAll
                           php
                           sta          _stash+12                     ;8 or 16 bit? YES!
                           sty          _stash+10                     ;ReadRepeat
                           stx          _stash+2                      ;address - legacy
                           rep          #$30                          ;need longA
                           lda          TestReadRepeat
                           inc                                        ;n++
                           sec
                           sbc          _stash+10                     ;-Y
                           sta          _stash+10                     ;ReadRepeat+1-(ReadRepeat-TestPass)=TestPass
                           sep          #$30
                           inc          _testErrors
                           bne          :noRoll
                           inc          _testErrors+1
:noRoll                    jsr          WinConsole
                           LOG          Mesg_F1                       ;Error Count
                           ldx          #_testErrors
                           ldy          #>_testErrors
                           jsr          PrintInt

                           PRINTSTRING  Mesg_F2                       ;Test Pass
                           ldx          #_testIteration
                           ldy          #>_testIteration
                           jsr          PrintInt

                           PRINTSTRING  Mesg_F3                       ;Read Pass
                           ldx          #_stash+10
                           ldy          #>_stash+10
                           jsr          PrintInt

                           PRINTSTRING  Mesg_F4                       ;Address
                           lda          CurBank
                           jsr          PRBYTE
                           lda          #"/"
                           jsr          COUT
                           lda          _stash+3
                           ldx          _stash+2
                           jsr          PRNTAX


                           PRINTSTRING  Mesg_F5                       ;Wrote
                           lda          TestSize16Bit
                           bne          :16bitwrote
:8bitwrote                 lda          HexPattern
                           jsr          PRBYTE
                           bra          :printread
:16bitwrote                lda          HexPattern
                           ldx          HexPattern+1
                           jsr          PRNTAX
:printread                 PRINTSTRING  Mesg_F6                       ;Read
                           lda          TestSize16Bit
                           bne          :16bitread
:8bitread                  lda          _stash+12
                           jsr          PRBYTE
                           bra          :nextline
:16bitread                 lda          _stash+12
                           ldx          _stash+12+1
                           jsr          PRNTAX


:nextline                  bra          :skipit
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
:skipit
                           jsr          WinFull
                           clc
                           xce
                           rep          $10                           ;take me out? but make sure to mx%
                           plp                                        ;M can be 0 or 1 depending on test size
                           PopAll
                           rts

*Mesg_Error0	asc "Error: Bad Read Pass 0000  Location: 00/1234"
*Mesg_Error0	asc "Wrote: $00 %12345678    Read: $00 %12345678"


TestRollBack
                           lda          TestDirection
                           eor          #$01
                           sta          TestDirection
                           jsr          TestAdvanceLocation
                           lda          TestDirection
                           eor          #$01
                           sta          TestDirection
                           rts

TestForceUpdateStatus      PushAll
                           stx          _stash
                           bra          :print
TestUpdateStatus           ldy          _updateTick
                           cpy          #_updateInterval
                           bcc          :noprint

                           PushAll
                           stx          _stash                        ; save real X
                           ldy          #$0000
                           sty          _updateTick


:print                     sep          #$10                          ;in case?  there was a sec xce combo here
                           GOXY         #66;#12
                           lda          CurBank
                           jsr          PRBYTE
                           lda          #"/"
                           jsr          COUT
                           lda          _stash+1
                           ldx          _stash
                           jsr          PRNTAX
                           clc
                           xce
                           rep          #$10
                           PopAll
:noprint                  ldy          _updateTick
                           iny
                           sty          _updateTick
    rts

_updateTick                dw           #0
_updateInterval            =            #$0200                        ;327 works well










TestMemoryLocation
                           lda          _testState
                           cmp          #TESTSTATE_BOTH
                           bne          TestMemoryLocationTwoPass

                           lda          TestSize16Bit
                           bne          :test16
:test8                     lda          TestType
                           cmp          #TT_BITPATTERN
                           bne          :checkrand
                           jmp          Test_8BitPatternWR
:checkrand                 cmp          #TT_RANDOM
                           bne          :checkwalk0
                           jmp          Test_8RandomWR
:checkwalk0                cmp          #TT_BITWALK0
                           bne          :checkwalk1
                           jmp          Test_8BitWalk0WR
:checkwalk1                cmp          #TT_BITWALK1
                           bne          :UNHANDLED
                           jmp          Test_8BitWalk1WR

:test16                    rep          #$30                          ;full 16-bit for long M

                           lda          TestType
                           and          #$00ff
                           cmp          #TT_BITPATTERN
                           bne          :check16rand
                           jmp          Test_16BitPatternWR
:check16rand               cmp          #TT_RANDOM
                           bne          :check16walk0
                           jmp          Test_16RandomWR
:check16walk0              cmp          #TT_BITWALK0
                           bne          :check16walk1
                           jmp          Test_16BitWalk0WR
:check16walk1              cmp          #TT_BITWALK1
                           bne          :UNHANDLED
                           jmp          Test_16BitWalk1WR
:UNHANDLED                 sep          #$30
                           rep          #$10

                           rts






***********************
*************************  TWO PASS !!
****************************
********************************
************************************
*****************************************


TestMemoryLocationTwoPass
                           lda          TestSize16Bit
                           bne          :test16

:test8                     lda          TestType                      ;8-bit tests
                           cmp          #TT_BITPATTERN
                           bne          :checkrand
                           jmp          Test_8BitPatternTP
:checkrand                 cmp          #TT_RANDOM
                           bne          :checkwalk0
                           jmp          Test_8RandomTP
:checkwalk0                cmp          #TT_BITWALK0
                           bne          :checkwalk1
                           jmp          Test_8BitWalk0TP
:checkwalk1                cmp          #TT_BITWALK1
                           bne          :UNHANDLED
                           jmp          Test_8BitWalk1TP


:test16                    rep          #$30                          ;full 16-bit for long M
                           lda          TestType
                           and          #$00ff
                           cmp          #TT_BITPATTERN
                           bne          :check16rand
                           jmp          Test_16BitPatternTP
:check16rand               cmp          #TT_RANDOM
                           bne          :check16walk0
                           jmp          Test_16RandomTP
:check16walk0              cmp          #TT_BITWALK0
                           bne          :check16walk1
                           jmp          Test_16BitWalk0TP
:check16walk1              cmp          #TT_BITWALK1
                           bne          :UNHANDLED
                           jmp          Test_16BitWalk1TP

:UNHANDLED                 sep          #$30
                           rep          #$10

                           rts

* TWO PASS TESTS
                           mx           %10

Test_16RandomTP
Test_16BitWalk1TP
Test_16BitWalk0TP
                           rts

_walkState                 db           0                             ;use to track in two pass mode
Test_8BitWalk0TP           lda          _walkState
                           asl
                           phx                                        ;TRICKY!  THEY ALL NEED TO PULL X WHEN THEY REACH THEIR JMP!
                           tax
                           jmp          (_walkTbl8B0,x)

_walkTbl8B0                da           Walk8B0_0,Walk8B0_1,Walk8B0_2,Walk8B0_3,Walk8B0_4,Walk8B0_5,Walk8B0_6,Walk8B0_7

Walk8B0_0                  plx
                           lda          #%01111111
                           sta          HexPattern
                           jmp          Test_8BitPatternTP
Walk8B0_1                  plx
                           lda          #%10111111
                           sta          HexPattern
                           jmp          Test_8BitPatternTP
Walk8B0_2                  plx
                           lda          #%11011111
                           sta          HexPattern
                           jmp          Test_8BitPatternTP
Walk8B0_3                  plx
                           lda          #%11101111
                           sta          HexPattern
                           jmp          Test_8BitPatternTP
Walk8B0_4                  plx
                           lda          #%11110111
                           sta          HexPattern
                           jmp          Test_8BitPatternTP
Walk8B0_5                  plx
                           lda          #%11111011
                           sta          HexPattern
                           jmp          Test_8BitPatternTP
Walk8B0_6                  plx
                           lda          #%11111101
                           sta          HexPattern
                           jmp          Test_8BitPatternTP
Walk8B0_7                  plx
                           lda          #%11111110
                           sta          HexPattern
                           jmp          Test_8BitPatternTP



Test_8BitWalk1TP           lda          _walkState
                           asl
                           phx                                        ;TRICKY!  THEY ALL NEED TO PULL X WHEN THEY REACH THEIR JMP!
                           tax
                           jmp          (_walkTbl8B1,x)

_walkTbl8B1                da           Walk8B1_0,Walk8B1_1,Walk8B1_2,Walk8B1_3,Walk8B1_4,Walk8B1_5,Walk8B1_6,Walk8B1_7

Walk8B1_0                  plx
                           lda          #%10000000
                           sta          HexPattern
                           jmp          Test_8BitPatternTP
Walk8B1_1                  plx
                           lda          #%01000000
                           sta          HexPattern
                           jmp          Test_8BitPatternTP
Walk8B1_2                  plx
                           lda          #%00100000
                           sta          HexPattern
                           jmp          Test_8BitPatternTP
Walk8B1_3                  plx
                           lda          #%00010000
                           sta          HexPattern
                           jmp          Test_8BitPatternTP
Walk8B1_4                  plx
                           lda          #%00001000
                           sta          HexPattern
                           jmp          Test_8BitPatternTP
Walk8B1_5                  plx
                           lda          #%00000100
                           sta          HexPattern
                           jmp          Test_8BitPatternTP
Walk8B1_6                  plx
                           lda          #%00000010
                           sta          HexPattern
                           jmp          Test_8BitPatternTP
Walk8B1_7                  plx
                           lda          #%00000001
                           sta          HexPattern
                           jmp          Test_8BitPatternTP


Test_8RandomTP             jsr          GetRandByte                   ;should match with seeds?
                           sta          HexPattern
                           jmp          Test_8BitPatternTP



TwoPassSeed                dw           #$0000                        ;we store the pass at the beginning of each write round, and restore it for the read round


Test_8BitPatternTP         lda          _testState
                           cmp          #TESTSTATE_READ
                           beq          :read
:write                     ldy          TestWriteRepeat
_writeloop2                lda          HexPattern
                           stal         $020000,x
BANKPATCH10                =            *-1
                           dey
                           bne          _writeloop2

                           jsr          CORRUPTOR

                           rts

:read
                           ldy          TestReadRepeat
_readloop3                 ldal         $020000,x
BANKPATCH11                =            *-1
                           cmp          HexPattern
                           bne          :readerror
                           dey
                           bne          _readloop3
                           rts
:readerror                 jsr          TestLogError
                           jsr          TestPrintErrors
                           jsr          TestPauseError
                           rts

                          mx %00
* 16-bit two-pass tests
Test_16BitPatternTP
                        lda          _testState
                           cmp          #TESTSTATE_READ
                           beq          :read16
:write16                     ldy          TestWriteRepeat
_writeloop3                lda          HexPattern
                           stal         $020000,x
BANKPATCH12                =            *-1
                           dey
                           bne          _writeloop3

                           PushAll
                           sep #$20
                           jsr CORRUPTOR
                           clc
                           xce
                           rep #$30
                           PopAll

                           sep #$20
                           rts

:read16
                           ldy          TestReadRepeat
_readloop4                 ldal         $020000,x
BANKPATCH13                =            *-1
                           cmp          HexPattern
                           bne          :readerror
                           dey
                           bne          _readloop4
                           sep $20
                           rts
:readerror                 jsr          TestLogError
                           jsr          TestPrintErrors
                           jsr          TestPauseError
                           rts









                           mx           %10                           ;still shortM longX
* 8-bit W/R TESTS
Test_8BitWalk0WR
                           lda          #%01111111
                           sta          HexPattern
                           jsr          Test_8BitPatternWR
                           lda          #%10111111
                           sta          HexPattern
                           jsr          Test_8BitPatternWR
                           lda          #%11011111
                           sta          HexPattern
                           jsr          Test_8BitPatternWR
                           lda          #%11101111
                           sta          HexPattern
                           jsr          Test_8BitPatternWR
                           lda          #%11110111
                           sta          HexPattern
                           jsr          Test_8BitPatternWR
                           lda          #%11111011
                           sta          HexPattern
                           jsr          Test_8BitPatternWR
                           lda          #%11111101
                           sta          HexPattern
                           jsr          Test_8BitPatternWR
                           lda          #%11111110
                           sta          HexPattern
                           jmp          Test_8BitPatternWR

Test_8BitWalk1WR
                           lda          #%10000000
                           sta          HexPattern
                           jsr          Test_8BitPatternWR
                           lda          #%01000000
                           sta          HexPattern
                           jsr          Test_8BitPatternWR
                           lda          #%00100000
                           sta          HexPattern
                           jsr          Test_8BitPatternWR
                           lda          #%00010000
                           sta          HexPattern
                           jsr          Test_8BitPatternWR
                           lda          #%00001000
                           sta          HexPattern
                           jsr          Test_8BitPatternWR
                           lda          #%00000100
                           sta          HexPattern
                           jsr          Test_8BitPatternWR
                           lda          #%00000010
                           sta          HexPattern
                           jsr          Test_8BitPatternWR
                           lda          #%00000001
                           sta          HexPattern
                           jmp          Test_8BitPatternWR

Test_8RandomWR
                           jsr          GetRandByte
                           sta          HexPattern
                           jmp          Test_8BitPatternWR


Test_8BitPatternWR
                           ldy          TestWriteRepeat
_writeloop                 lda          HexPattern
                           stal         $020000,x
BANKPATCH01                =            *-1
                           lda          TestAdjacentWrite
                           beq          _noAdjacentWrite
                           stal         $02FFFF,x                     ;-1
BANKPATCH02                =            *-1
                           stal         $020001,x                     ;+1
BANKPATCH03                =            *-1

_noAdjacentWrite           dey
                           bne          _writeloop

                           jsr          CORRUPTOR

                           ldy          TestReadRepeat
_readloop2                 ldal         $020000,x
BANKPATCH04                =            *-1
                           cmp          HexPattern
                           bne          :readerror
                           dey
                           bne          _readloop2
                           rts
:readerror                 jsr          TestLogError
                           jsr          TestPrintErrors
                           jsr          TestPauseError
                           rts


* 16-bit W/R TESTS
                           mx           %00
Test_16BitWalk0WR
                           lda          #%0111111111111111
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%1011111111111111
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%1101111111111111
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%1110111111111111
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%1111011111111111
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%1111101111111111
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%1111110111111111
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%1111111011111111
                           sta          HexPattern
                           jsr          Test_16Bit

                           lda          #%1111111101111111
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%1111111110111111
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%1111111111011111
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%1111111111101111
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%1111111111110111
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%1111111111111011
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%1111111111111101
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%1111111111111110
                           sta          HexPattern
                           jsr          Test_16Bit
                           sep          #$20
                           rts
                           mx           %00

Test_16BitWalk1WR
                           lda          #%1000000000000000
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%0100000000000000
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%0010000000000000
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%0001000000000000
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%0000100000000000
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%0000010000000000
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%0000001000000000
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%0000000100000000
                           sta          HexPattern
                           jsr          Test_16Bit


                           lda          #%0000000010000000
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%0000000001000000
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%0000000000100000
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%0000000000010000
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%0000000000001000
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%0000000000000100
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%0000000000000010
                           sta          HexPattern
                           jsr          Test_16Bit
                           lda          #%0000000000000001
                           sta          HexPattern
                           jsr          Test_16Bit
                           sep          #$20
                           rts
                           mx           %00


Test_16RandomWR            jsr          GetRandByte16
                           sta          HexPattern
                           jsr          Test_16Bit
                           sep          #$20
                           rts
                           mx           %00


Test_16BitPatternWR        jsr          Test_16Bit
                           sep          #$20
                           rts
                           mx           %00

* the real test function for 16-bit
Test_16Bit                 ldy          TestWriteRepeat
_writeloop16               lda          HexPattern
                           stal         $020000,x
BANKPATCH05                =            *-1
                           lda          TestAdjacentWrite
                           beq          _noAdjacentWrite16
_adjacentWrite16           stal         $02FFFE,x                     ;-1
BANKPATCH06                =            *-1
                           stal         $020002,x                     ;+1
BANKPATCH07                =            *-1

_noAdjacentWrite16         dey
                           bne          _writeloop16
                           sep          #$20                          ;?????????????????
                           jsr          CORRUPTOR
                           clc
                           xce
                           rep          #$30                          ;????????????????????

                           ldy          TestRefreshPause
                           beq          :nopause
:outer                     dey

                           bne          :outer
:nopause

                           ldy          TestReadRepeat
_readloop16                ldal         $020000,x
BANKPATCH08                =            *-1
                           cmp          HexPattern
                           bne          _readerror16
                           dey
                           bne          _readloop16
                           rts

_readerror16               sep          #$20
                           jsr          TestLogError
                           jsr          TestPrintErrors
                           jsr          TestPauseError
                           clc
                           xce
                           rep          #$30
                           rts


                           mx           %10

TestAdvanceLocation        lda          TestDirection
                           bne          :dn

:up                        lda          TestSize16Bit
                           beq          :up8
:up16                      inx
                           beq          :hitBankBoundry
:up8                       inx
                           beq          :hitBankBoundry               ;rollover
                           cpx          EndAddr                       ;sets carry if we are past/done
                           bcs          :done
                           rts

:dn                        lda          TestSize16Bit
                           beq          :dn8
:dn16                      cpx          #0
                           beq          :hitBankBoundry
                           dex                                        ;
                           cpx          #0
                           beq          :hitBankBoundryTest           ;we still need to test in this case.  side effect of odd start/ends
                           bra          :testStartAddr
:dn8                       cpx          #0
                           beq          :hitBankBoundry
                           dex
:testStartAddr             cpx          StartAddr
                           bcc          :done
:hitBankBoundryTest        clc
                           rts
:done
:hitBankBoundry            sec
                           rts


TestGetStartAddress        lda          TestDirection
                           bne          :dn
:up                        ldx          StartAddr
                           rts
:dn                        ldx          EndAddr
:addressSet                rts

TestPastFinalBank          lda          TestDirection
                           bne          :descending
:ascending                 lda          EndBank
                           cmp          CurBank                       ;is EndBank < CurBank ?
                           bcc          :yes                          ;past final bank
                           bcs          :no
:descending                lda          CurBank
                           cmp          StartBank                     ;is CurBank < StartBank ?
                           bcc          :yes
                           bcs          :no

:yes                       sec
                           rts
:no                        clc
                           rts


TestTwoPassRestoreSeed
                           lda          TwoPassSeed+1                 ;if we are on a read pass, restore our seed
                           sta          _seed16a
                           lda          TwoPassSeed
                           sta          _seed16b
                           sta          _seed
                           rts


TestTwoPassMakeSeed                                                   ;jsr          GetRandByte                   ;update our two-pass seed (even if we aren't in random mode.  too lazy to check.)
                           sta          TwoPassSeed                   ;
                           sta          _seed16b
                           sta          _seed

                                                                      ;jsr          GetRandByte                   ;
                           sta          TwoPassSeed+1                 ;
                           sta          _seed16a

                           rts


* TWO PASS has lots of exceptions where it doesn't advance the bank right away
TestGetNextBank            lda          CurBank
                           bne          :notInitialBank               ;can't be bank 00 so we must be starting a new test
                           jmp          SetInitialBank                ;will RTS back - THIS IS THE SAME FOR ALL TESTS
:notInitialBank

                           lda          TestTwoPass                   ;see if we are doing two-passes of the bank
                           bne          :TwoPass                      ;nope, no additional logic needed
                           jmp          SetNextBank                   ;regular way to advance the bank

:TwoPass                   jsr          TwoPassBankLogics
                           bcs          SetNextBank
                           rts


* Just sets the CurBank to either the StartAddr or EndAddr, depending on direction
SetInitialBank             lda          TestDirection
                           bne          :descending
:ascending                 lda          StartBank
                           bra          :storeInitialBank
:descending                lda          EndBank
:storeInitialBank          sta          CurBank
                           rts

* INCs or DECs CurBank, depending on direction.  No value checking at all.
SetNextBank                lda          TestDirection
                           bne          :descending
                           inc          CurBank
                           rts
:descending                dec          CurBank
                           rts


* This is really TestGetNextBank for TwoPass!!!
* Set CARRY on return to advance bank
TwoPassBankLogics
                           lda          _testState
                           cmp          #TESTSTATE_READ               ;don't change bank on read pass of two-pass.  (we read during this pass)
                           bne          :checkWrite
                           lda          TestType
:checkReadPattern          cmp          #TT_BITPATTERN
                           bne          :checkReadRandom
                           clc
                           rts
:checkReadRandom           cmp          #TT_RANDOM
                           bne          :checkReadBitwalk0
                           jsr          TestTwoPassRestoreSeed        ;for RANDOM, restore our write seed
:checkReadBitwalk0         cmp          #TT_BITWALK0
                           bne          :checkReadBitwalk1
                                                                      ;     jmp          TestUpdateWalkState           ;for BITWALK0, update walkpass and SEC when loops
                           clc
                           rts
:checkReadBitwalk1         cmp          #TT_BITWALK1
                           bne          :unknown
                                                                      ;       jmp          TestUpdateWalkState           ;for BITWALK1, update walkpass and SEC when loops
                           clc
                           rts


:checkWrite                                                           ;we're in write mode.
                           lda          TestType
:checkWritePattern         cmp          #TT_BITPATTERN
                           bne          :checkWriteRandom
                           sec
                           rts

:checkWriteRandom          cmp          #TT_RANDOM
                           bne          :checkWriteBitwalk0
                           jsr          TestTwoPassMakeSeed           ;for RANDOM, make a write seed
:checkWriteBitwalk0        cmp          #TT_BITWALK0
                           bne          :checkWriteBitwalk1
                           jmp          TestUpdateWalkState           ;for BITWALK0, update walkpass and SEC when loops

:checkWriteBitwalk1        cmp          #TT_BITWALK1
                           bne          :unknown
                           jmp          TestUpdateWalkState           ;for BITWALK1, update walkpass and SEC when loops

:unknown                   sec                                        ;unknown - advance? should not occur.
                           rts


* sets carry when last test complete
TestUpdateWalkState
                           inc          _walkState                    ;walkstate++
                           lda          TestSize16Bit
                           bne          :walk16
:walk8
                           lda          _walkState
                           cmp          #8
                           beq          :resetWalkState
                           clc
                           rts
:walk16
                           lda          _walkState
                           cmp          #16
                           beq          :resetWalkState
                           clc
                           rts

:resetWalkState                                                       ;walkstate=0
                           stz          _walkState
                           sec
                           rts





TestPatchBanks             lda          CurBank
                           sta          BANKPATCH01
                           sta          BANKPATCH02
                           sta          BANKPATCH03
                           sta          BANKPATCH04
                           sta          BANKPATCHXX                   ;corruptor!
                           sta          BANKPATCH05
                           sta          BANKPATCH06
                           sta          BANKPATCH07
                           sta          BANKPATCH08

                           sta          BANKPATCH10                   ;two pass start here
                           sta          BANKPATCH11
                           sta BANKPATCH12
                           sta BANKPATCH13

                           rts


CORRUPTOR                  lda          $C000
                           bpl          _nokey
                           cmp          #"c"
                           bne          _nokey
                           jsr          GetRandTrash                  ;careful... this is 8-bit code.  make sure M=1
                                                                      ;lda          #$55
                           stal         $020000,x
BANKPATCHXX                =            *-1
_nokey                     nop
                           rts
                           mx           %11








_testIteration             ds           8
_testErrors                ds           8
_testState                 ds           2                             ;1=read 2=write 3=both (read & write)
TESTSTATE_READ             =            1
TESTSTATE_WRITE            =            2
TESTSTATE_BOTH             =            3
UpdateScanInterval         equ          #$1000

Mesg_Welcome               asc          "Welcome to Mini Memory Tester v0.3 by Dagen Brock",$8D,00
Mesg_InternalRam256        asc          "Built-In RAM  256K",00
Mesg_InternalRam1024       asc          "Built-In RAM  1024K",00
Mesg_ExpansionRam          asc          "Expansion RAM ",00
Mesg_Rom                   asc          "Apple IIgs ROM ",00
Mesg_UserManual            asc          "USE ARROW KEYS TO MOVE  -  USE ENTER TO SELECT/EDIT",00
Mesg_Starting              asc          $8D,"Starting Test",$8D,"Press P to pause, ESC to stop.",$8D,$8D,00
Mesg_Waiting               asc          "   Waiting: ",00
Mesg_Writing               asc          "   Writing: ",00
Mesg_Reading               asc          "   Reading: ",00
Mesg_WR                    asc          "Write&Read: ",00
Mesg_Errors                asc          "    Errors:  ",$00
Mesg_TestPass              asc          " Test Pass:  ",00
Mesg_Blank                 asc          "                 ",00
Mesg_DetectedBanks         asc          "Setting default start/end banks to detected memory expansion: $",00
Mesg_ToBank                asc          " to $",00
Mesg_TestError1            asc          "Error: Press any",00
Mesg_TestError2            asc          "key to continue.",00
* Error message strings
Mesg_E1                    asc          "Bad Read - Pass ",00
Mesg_E2                    asc          "   Location: ",00
Mesg_E3                    asc          "Wrote: $",00
Mesg_E4                    asc          " ",$1B,'SU',$18," Read: $",00
Mesg_Arrow                 asc          $1B,'SU',$18,00

Mesg_F1                    asc          "ERR #",00
Mesg_F2                    asc          "  Pass #",00
Mesg_F3                    asc          "  Rep #",00
Mesg_F4                    asc          "  Addr. $",00
Mesg_F5                    asc          "  Wrote $",00
Mesg_F6                    asc          " ",$1B,'U',$18," Read $",00


                           mx           %10                           ;i think?
* called with short M,  long X
PrintTestError

                           sep          $30
                           inc          _testErrors
                           bne          :noRoll
                           inc          _testErrors+1
:noRoll                    PRINTXY      #55;#11;Mesg_Errors
                           ldx          _testErrors
                           lda          _testErrors+1
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
                           jsr          WinFull
                           clc
                           xce
                           rep          $10
                           rts

*Mesg_Error0	asc "Error: Bad Read Pass 0000  Location: 00/1234"
*Mesg_Error0	asc "Wrote: $00 %12345678    Read: $00 %12345678"




                           mx           %11
PRBIN                      pha
                           phx
                           ldx          #8
:loop                      asl
                           pha
                           bcc          :zero
:one                       lda          #"1"
                           jsr          COUT
                           bra          :ok
:zero                      lda          #"0"
                           jsr          COUT
:ok                        pla
                           dex
                           bne          :loop
                           plx
                           pla
                           rts

Pauser
                           PRINTXY      #55;#13;Mesg_Waiting
                           ldy          #60
                           ldx          TestRefreshPause
                           beq          :donepause
                           jsr          PrintTimerVal                 ; inaugural print before waiting 1 sec
:secondloop
:wait                      ldal         $e1c019
                           bpl          :wait
:wait2                     ldal         $e1c019
                           bmi          :wait2
                           dey
                           bne          :secondloop
                           dex
                           beq          :donepause
                           jsr          PrintTimerVal
                           ldy          #60
                           bra          :secondloop
:donepause
                           PRINTXY      #55;#13;Mesg_Blank
                           rts
PrintTimerVal
                           phx
                           phy
                           txa
                           GOXY         #65;#13
                           ply
                           plx
                           txa
                           jsr          PRBYTE
                           rts

* possible EOR values
*$1d (29)
*$2b (43)
*$2d (45)
*$4d (77)
*$5f (95)
*$63 (99)
*$65 (101)
*$69 (105)
*$71 (113)
*$87 (135)
*$8d (141)
*$a9 (169)
*$c3 (195)
*$cf (207)
*$e7 (231)
*$f5 (245)


* This is ridiculously poorly implemented.  Don't care.
                           mx           $00
GetRandByte16              PushAll
                           ShortMX
                           lda          _seed16a
                           beq          :doEor
                           asl
                           bcc          :noEor
:doEor                     eor          #$a9
:noEor                     sta          _seed16a

                           lda          _seed16b
                           beq          :doEorB
                           asl
                           bcc          :noEorB
:doEorB                    eor          #$5f
:noEorB                    sta          _seed16b
                           Full16
                           PopAll
                           lda          _seed16a
                           rts
_seed16a                   db           03
_seed16b                   db           40





                           mx           %11

GetRandByte                                                           ; USE ONLY WITH CORRUPTOR
                           lda          _seed
                           beq          :doEor
                           asl
                           bcc          :noEor
:doEor                     eor          #$2b
:noEor                     sta          _seed
                           rts
_seed                      db           0
GetRandTrash                                                          ; USE ONLY WITH CORRUPTOR
                           lda          _randomTrashByte
                           beq          :doEor
                           asl
                           bcc          :noEor
:doEor                     eor          #$1d
:noEor                     sta          _randomTrashByte
                           rts
_randomTrashByte           db           0
















*
*       ####  ###### ##### ##### # #    #  ####   ####
*      #      #        #     #   # ##   # #    # #
*       ####  #####    #     #   # # #  # #       ####
*           # #        #     #   # #  # # #  ###      #
*      #    # #        #     #   # #   ## #    # #    #
*       ####  ######   #     #   # #    #  ####   ####

*@todo better defaults
* 00 - Byte : Selected Value
* 01 - Byte : Number of values
* 02... - Words : Table of Addresses of possible values
TestTypeTbl
TestType                   db           00                            ; actual CONST val
                           db           04                            ; number of possible values
                           da           TestType_BitPattern,TestType_BitWalk1,TestType_BitWalk0,TestType_Random,00,00
TestType_BitPattern        asc          "bit pattern",$00
TestType_BitWalk1          asc          " bit walk 1",$00
TestType_BitWalk0          asc          " bit walk 0",$00
TestType_Random            asc          "  random   ",$00
TT_BITPATTERN              =            0
TT_BITWALK1                =            1
TT_BITWALK0                =            2
TT_RANDOM                  =            3

TestDirectionTbl
TestDirection              db           0
                           db           2
                           da           _testDirectionUp,_testDirectionDown,00,00
_testDirectionUp           asc          "up",$00
_testDirectionDown         asc          "dn",$00

TestSizeTbl
TestSize16Bit              db           01                            ;0=no ... 8bit,    1=yes ... 16 bit
                           db           02
                           da           _TestSize_0,_TestSize_1
_TestSize_0                asc          " 8-bit",$00
_TestSize_1                asc          "16-bit",$00

MenuStr_BeginTestJSR       da           TestInit                      ; MUST PRECEDE MENU STRING!  Yes, it's magicly inferred. (-2)
MenuStr_BeginTest          asc          " BEGIN TEST "
MenuStr_BeginTestL         equ          #*-MenuStr_BeginTest
MenuStr_BeginTestE         db           00

StartBank                  db           #$06
EndBank                    db           #$1F
CurBank                    db           #0
StartAddr                  dw           #$0000
EndAddr                    dw           #$FFFF
HexPattern                 dw           #$0000

TestTwoPass                dw           #0                            ; bool is byte, but might change in future? :P
TestAdjacentWrite          dw           #0                            ; bool is byte, but might change in future? :P
TestRefreshPause           dw           #$00                          ; int
TestReadRepeat             dw           #$01                          ; int
TestWriteRepeat            dw           #$01                          ; int
TestIterations             dw           #$00                          ; int
TestErrorPause             dw           #0                            ;bool



*
*           #    # ###### #    # #    #
*           ##  ## #      ##   # #    #
*           # ## # #####  # #  # #    #
*           #    # #      #  # # #    #
*           #    # #      #   ## #    #
*           #    # ###### #    #  ####

MainMenuDefs
:StartBank                 hex          19,05                         ; x,y
                           db           Menu_TypeHex                  ; 1=hex input
                           db           01                            ; memory size (bytes)
                           da           StartBank                     ; variable storage
:EndBank                   hex          22,05                         ; x,y
                           db           Menu_TypeHex                  ; 1=hex input
                           db           01                            ; memory size (bytes)
                           da           EndBank                       ; variable storage
:StartAddr                 hex          19,06                         ; x,y
                           db           Menu_TypeHexByteOrder         ; 1=hex input
                           db           02                            ; memory size (bytes)
                           da           StartAddr                     ; variable storage
:EndAddr                   hex          20,06                         ; x,y
                           db           Menu_TypeHexByteOrder         ; 1=hex input
                           db           02                            ; memory size (bytes)
                           da           EndAddr                       ; variable storage
:TestType                  hex          19,07                         ; x,y
                           db           Menu_TypeList                 ; 3=list input
                           db           11                            ; max len size (bytes), 3=option list
                           da           TestTypeTbl                   ; params definition & storage
:TestSize                  hex          28,07                         ; x,y
                           db           Menu_TypeList                 ; 3=list input
                           db           6                             ; max len size (bytes), 3=option list
                           da           TestSizeTbl                   ; params definition & storage
:HexPattern                hex          19,08                         ; x,y
                           db           Menu_TypeHex                  ; 3=list input
_hexpatternsize            db           02                            ; max len size (bytes), 3=option list <- can change when 8 bit??
                           da           HexPattern                    ; params definition & storage
:BinPattern                hex          19,09                         ; x,y
                           db           Menu_TypeBin                  ; 5?=bin
_binpatternsize            db           02                            ; max len size (bytes), 3=option list <- can change when 8 bit??
                           da           HexPattern                    ; params definition & storage <- uses same space as above!! just different representation
:Direction                 hex          12,0B
                           db           Menu_TypeList
                           db           2
                           da           TestDirectionTbl
:TestErrorPause            hex          28,0B                         ; x,y
                           db           Menu_TypeBool                 ; 1=hex input
                           db           2                             ; could be 8-bit or 16-bit bool
                           da           TestErrorPause                ; variable storage
:AdjacentWrite             hex          12,0C                         ; x,y
                           db           Menu_TypeBool                 ; 1=hex input
                           db           01                            ; memory size (bytes)
                           da           TestAdjacentWrite             ; variable storage
:TwoPass                   hex          28,0C
                           db           Menu_TypeBool
                           db           2                             ; could be 8-bit or 16-bit bool
                           da           TestTwoPass

:ReadRepeat                hex          12,0D                         ; x,y
                           db           Menu_TypeInt                  ; 1=hex input
                           db           03                            ; display/entry width. ints are 16-bit internally
                           da           TestReadRepeat                ; variable storage
:WriteRepeat               hex          28,0D                         ; x,y
                           db           Menu_TypeInt                  ; 1=hex input
                           db           03                            ; display/entry width. ints are 16-bit internally
                           da           TestWriteRepeat               ; variable storage
:TestIterations            hex          12,0E                         ; x,y
                           db           Menu_TypeInt                  ; 1=hex input
                           db           03                            ; display/entry width. ints are 16-bit internally
                           da           TestIterations                ; variable storage
:TestRefreshPause          hex          28,0E                         ; x,y
                           db           Menu_TypeInt                  ; 1=hex input
                           db           03                            ; display/entry width. ints are 16-bit internally
                           da           TestRefreshPause              ; variable storage
:BeginTest                 hex          3A,0E                         ; x,y
                           db           Menu_TypeAction               ; 2=action
                           db           MenuStr_BeginTestL            ; menu string length
                           da           MenuStr_BeginTest             ; string storage
MainMenuLen                equ          *-MainMenuDefs
MainMenuItems              equ          MainMenuLen/6
MainMenuEnd                dw           0000
Menu_ItemSelected          db           0

* special helper functions to update some input sizes when
* the user switches between 8 and 16 bit testing modes
* ... also disable AdjacentWrite if TwoPass
MenuUpdateConfig           lda          TestSize16Bit
                           bne          :is16bit
:is8bit                    jsr          MenuFixMax8
                           jmp          MenuSet8Bit
:is16bit                   jsr          MenuFixMax16
                           jmp          MenuSet16Bit
MenuSet16Bit               lda          #2
                           bra          MenuSetBits
MenuSet8Bit                jsr          MenuClearPatterns             ;clear leftover chars because strings are shorter now
                           lda          #1
MenuSetBits                sta          _hexpatternsize
                           sta          _binpatternsize

:checkTwoPass              lda          TestTwoPass                   ;now check TwoPass/AdjacentWrite conflict
                           cmp          _lastTwoPass                  ;i wish this was simpler code
                           beq          :checkAdjacentWrite           ;some computer science dude could probably help me out here
                           sta          _lastTwoPass
                           stz          TestAdjacentWrite
                           stz          _lastAdjacentWrite
                           bra          :done
:checkAdjacentWrite        lda          TestAdjacentWrite
                           cmp          _lastAdjacentWrite
                           beq          :done
                           sta          _lastAdjacentWrite

                           stz          TestTwoPass
                           stz          _lastTwoPass
:done                      rts
_lastTwoPass               db           0
_lastAdjacentWrite         db           0

MenuFixMax16               Full16
                           lda          EndAddr
                           cmp          #$FFFF
                           bne          :noneed
                           dec          EndAddr
:noneed                    ShortMX
                           rts

MenuFixMax8                Full16
                           lda          EndAddr
                           cmp          #$FFFE
                           bne          :noneed
                           inc          EndAddr
:noneed                    ShortMX
                           rts

* hack to allow for smaller portion of screen to update
MenuClearPatterns          PRINTXY      #$17;#$8;_clearstring
                           PRINTXY      #$17;#$9;_clearstring
                           rts
_clearstring               asc          "                         ",$00

MainMenuStrs
                           asc          " ______________________________________________________________________________",$8D,$00
                           asc          $1B,'ZV_@ZVWVWVWV_',"Mini Memory Tester v0.3",'ZVWVWVWVWVWVWVWVWVWVW_',"UltimateMicro",'ZWVWVWVW_',$18,$00
                           asc          $1B,'ZLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL_',$18,00
                           asc          $1B,'ZZ \GGGGGGGGGGGGG_',"Test  Settings",'ZGGGGGGGGGGGGG\ _'," ",'Z \GGGGGGGG_',"Info",'ZGGGGGGGG\ _'," ",'_',$18,00
                           asc          $1B,'ZZ',"                                              ",'_'," ",'Z',"                          ",'_'," ",'_',$18,00
                           asc          $1B,'ZZ',"  Start/End Bank    :       /                 ",'_'," ",'Z',"                          ",'_'," ",'_',$18,00
                           asc          $1B,'ZZ',"  Start/End Address :       /                 ",'_'," ",'Z',"                          ",'_'," ",'_',$18,00
                           asc          $1B,'ZZ',"  Test Type         :                         ",'_'," ",'Z',"                          ",'_'," ",'_',$18,00
                           asc          $1B,'ZZ',"       Hex Pattern  :                         ",'_'," ",'Z',"                          ",'_'," ",'_',$18,00
                           asc          $1B,'ZZ',"       Bin Pattern  :                         ",'_'," ",'Z',"                          ",'_'," ",'_',$18,00
                           asc          $1B,'ZZ',"                                              ",'_'," ",'Z',"                          ",'_'," ",'_',$18,00
                           asc          $1B,'ZZ',"  Direction            Wait on Error          ",'_'," ",'Z',"                          ",'_'," ",'_',$18,00
                           asc          $1B,'ZZ',"  Adjacent Wr.         Two-Pass W/R           ",'_'," ",'Z',"                          ",'_'," ",'_',$18,00
                           asc          $1B,'ZZ',"  Read Repeat          Write Repeat           ",'_'," ",'Z',"                          ",'_'," ",'_',$18,00
                           asc          $1B,'ZZ',"  Iterations           Refresh Pause          ",'_'," ",'Z',"     ([ BEGIN TEST ])     ",'_'," ",'_',$18,00
                           asc          $1B,'ZZ',"                                              ",'_'," ",'Z',"                          ",'_'," ",'_',$18,00
                           asc          $1B,'ZLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL_',$18,00
                           asc          $1B,'Z',"                                                                              ",'_',$18,00
                           asc          $1B,'Z',"                                                                              ",'_',$18,00
                           asc          $1B,'Z',"                                                                              ",'_',$18,00
                           asc          $1B,'Z',"                                                                              ",'_',$18,00
                           asc          $1B,'Z',"                                                                              ",'_',$18,00
                           asc          $1B,'Z',"                                                                             _",'_',$18,00
                           asc          $1B,'Z',"_____________________________________________________________________________",'_',$18,00
                           hex          00,00






* Creates a 256 byte map of each bank, "BankRam"
* The map shows whether it's Built-in RAM, ROM, Expansion RAM, etc.
DetectRam
                           lda          #BankRAMFastBuiltIn           ;these are universal to all IIgs
                           sta          BankMap+$00                   ;bank 00
                           sta          BankMap+$01                   ;bank 01
                           lda          #BankRAMSlowBuiltIn           ;
                           sta          BankMap+$e0                   ;bank e0
                           sta          BankMap+$e1                   ;bank e1
                           lda          #BankROMUsed
                           sta          BankMap+$FE                   ;bank FE
                           sta          BankMap+$FF                   ;bank FF

                           lda          GSROM
                           cmp          #3                            ;check for ROM3 IIgs
                           bne          :rom0or1
:rom3                      lda          #BankRAMFastBuiltIn
                           ldx          #$02                          ;bank 02
:builtinram                sta          BankMap,x                     ;bank 02
                           inx
                           cpx          #$10                          ;stop after bank 0F
                           bcc          :builtinram
                           lda          #BankROMUsed                  ;ROM 3 is 256KB, so 4 banks (2 additional)
                           sta          BankMap+$FC                   ;
                           sta          BankMap+$FD                   ;
                           ldx          #$10                          ;ROM3 starts scan at bank 10
                           bra          :detectloop

:rom0or1                                                              ;no additional mappings
                           lda          #$FE                          ;ROM1 end bank FE
                           sta          :endbankscan+1                ;but change our max scan bank
                           ldx          #$02                          ;ROM0/1 starts scan at bank 02

:detectloop                txa                                        ;we'll store the bank number
                           sta          :writer+3                     ;overwrite bank address
                           sta          :reader+3
                           sta          :compare+1
:writer                    stal         $000000                       ;should overwrite first byte
:reader                    ldal         $000000
:compare                   cmp          #$00
                           bne          :notused
                           inc          BankExpansionRam              ;TotalMB++
                           lda          #BankRAMFastExpansion         ;store mapping
                           sta          BankMap,x
:continue                  inx
                           cpx          #$E0                          ;skip banks $E0-$EF
                           bcc          :endbankscan                  ; <E0
                           cpx          #$F0
                           bcs          :endbankscan                  ; >= F0    (>EF)
                           ldx          #$F0                          ;skip to bank F0
                           bra          :detectloop
:endbankscan               cpx          #$FC                          ;ROM3 end bank (default)
                           bcc          :detectloop                   ;blt

                                                                      ;let's find low/high to simplify things
                           ldx          #$ff
:lowloop                   lda          BankMap,x
                           cmp          #BankRAMFastExpansion
                           beq          :isRam
                           dex
                           cpx          #$ff
                           bne          :lowloop
                           bra          :checkhigh
:isRam                     stx          BankExpansionLowest
                           dex
                           bra          :lowloop

:checkhigh                 ldx          #$00
:highloop                  lda          BankMap,x
                           cmp          #BankRAMFastExpansion
                           beq          :isRam2
                           inx
                           bne          :highloop
                           bra          :done
:isRam2                    stx          BankExpansionHighest
                           inx
                           bra          :highloop

:done                      bra          :findKB

:notused                   lda          #BankNoRAM
                           sta          BankMap,x
                           bra          :continue

:findKB
                           lda          BankExpansionRam              ;number of banks
                           clc
                           xce
                           rep          #$30
                           mx           %00
                           and          #$00FF                        ;clear artifacts? can't remember state of B
                           asl                                        ;*2
                           asl                                        ;*4
                           asl                                        ;*8
                           asl                                        ;*16
                           asl                                        ;*32
                           asl                                        ;*64
                           sta          BankExpansionRamKB

                           lda          GSROM                         ;now check (hardcode really) build-in ram
                           cmp          #3
                           bne          :notrom3
:rom3                      lda          #1024
                           sta          BankBuiltInRamKB
                           rts
:notrom3                   lda          #256
                           sta          BankBuiltInRamKB
                           sep          #$30

                           rts



* Takes address in X/Y and prints out Int stored there
PrintInt
                           stx          :loc+1
                           inx
                           stx          :loc2+1
                           sty          :loc+2
                           sty          :loc2+2

:loc                       ldx          $2000                         ;overwrite
:loc2                      ldy          $2000                         ;overwrite
                           jsr          BINtoBCD
                           phx
                           tya
                           jsr          PRBYTE
                           pla
                           jsr          PRBYTE
                           rts




Quit                       jsr          MLI                           ; first actual command, call ProDOS vector
                           dfb          $65                           ; with "quit" request ($65)
                           da           QuitParm
                           bcs          Error
                           brk          $00                           ; shouldn't ever  here!

QuitParm                   dfb          4                             ; number of parameters
                           dfb          0                             ; standard quit type
                           da           $0000                         ; not needed when using standard quit
                           dfb          0                             ; not used
                           da           $0000                         ; not used

Error                      brk          $00                           ; shouldn't be here either

                           put          misc
                           put          strings.s
                           put          menu.s


                                                                      ;
BankROMUsed                =            1
BankROMReserved            =            2
BankRAMSlowBuiltIn         =            3
BankRAMFastBuiltIn         =            4
BankRAMFastExpansion       =            5
BankNoRAM                  =            0



BorderColor                db           0

BankExpansionRamKB         ds           2
BankBuiltInRamKB           ds           2
BankExpansionRam           ds           1
BankExpansionLowest        ds           1
BankExpansionHighest       ds           1
                           ds           \
BankMap                    ds           256                           ;page-align maps just to make them easier to see
_stash                     ds           256
                           ds           \
