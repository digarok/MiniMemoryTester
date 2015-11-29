**************************************************
* Apple Standard Memory Locations
**************************************************

* APPLESOFT
CH           equ   $24
CV           equ   $25
HCOLOR       equ   $E4         ;HCOLOR value
SETHCOL      equ   $F6EC       ;Set active HCOLOR to value of X (0 ... 7)
HPLOT        equ   $F457       ;Plots a colored dor at the position
                               ; given by A (vertical), Y (horizontal high), 
                               ; and X (horizontal low)
HLIN         equ   $F53A       ;Draws a line from the last plotted dot to the
                               ; position given by Y (vertical), X (horizontal high),
                               ; and A (horizontal low)
CLRLORES     equ   $F832
CLRHIRES     equ   $F3F2
CLRHIRESC    equ   $F3F6       ;clears to last color plotted



CLR80VID     equ   $C00C
SET80VID     equ   $C00D
CLR80COL     equ   $C001
SET80COL     equ   $C001
TXTCLR       equ   $C050
TXTSET       equ   $C051
MIXCLR       equ   $C052
MIXSET       equ   $C053
TXTPAGE1     equ   $C054
TXTPAGE2     equ   $C055
LORES        equ   $C056
HIRES        equ   $C057

KEY          equ   $C000
C80STOREOFF  equ   $C000
C80STOREON   equ   $C001
STROBE       equ   $C010
SPEAKER      equ   $C030
VBL          equ   $C02E
RDVBLBAR     equ   $C019       ;not VBL (VBL signal low

RAMWRTAUX    equ   $C005
RAMWRTMAIN   equ   $C004
CLRAN3       equ   $C05E       ;Clear annunciator-3 output  (DLR on)	 (Mislabeled in IIgs Firmare Ref?)
SETAN3       equ   $C05F       ;Set annunciator-3 output


COUT         equ   $FDED       ; Calls the output routine whose address is stored in CSW,
                               ;  normally COUTI
CLREOL       equ   $FC9C       ; Clears to end of line from current cursor position
CLEOLZ       equ   $FC9E       ; Clear to end ofline using contents of Y register as cursor
                               ;  position
CLREOP       equ   $FC42       ; Clears to bottom of window
CLRSCR       equ   $F832       ; Clears the low-resolution screen
CLRTOP       equ   $F836       ; Clears the top 40 lines of the low-resolution screen
COUTI        equ   $FDF0       ; Displays a character on the screen
CROUT        equ   $FD8E       ; Generates a carriage return
CROUT1       equ   $FD8B       ; Clears to end ofline and then generates a carriage return
GETLN        equ   $FD6A       ; Displays the prompt character; accepts a string of characters
                               ;  by means of RDKEY
HLINE        equ   $F819       ; Draws a horizontal line of blocks
HOME         equ   $FC58       ; Clears the window and puts the cursor in the upper left
                               ;  corner of the window
KEYIN        equ   $FD1B       ; With 80-column fumware inactive, displays checkerboard
                               ;  cursor; accepts characters from keyboard
PLOT         equ   $F800       ; Plots a single low-resolution block on the screen
PRBL2        equ   $F94A       ; Sends 1 to 256 blank spaces to the output device
PRBYTE       equ   $FDDA       ; Prints a hexadecimal byte
PRHEX        equ   $FDE3       ; Prints 4 bits as a hexadecimal number

PRNTAX       equ   $F941       ; Prints the contents of A and X in hexadecimal format
RDKEY        equ   $FD0C       ; Displays blinking cursor; goes to standard input
                               ;  routine, nonnally KEYIN or BASICIN
SCRN         equ   $F871       ; Reads color of a low-resolution block
SETCOL       equ   $F864       ; Sets the color for plotting in low-resolution block
VTAB         equ   $FC22       ; Sets the cursor vertical position (from CV)
VTABZ        equ   $FC24       ; Sets the cursor vertical position (0)
VLINE        equ   $F828       ; Draws a vertical line of low-resolution blocks

GSROM        equ   $FB59       ; should be int number of rom rev on Apple IIgs



* KEY EQUATES
KEY_UPARROW  =     $8B
KEY_DNARROW  =     $8A
KEY_RTARROW  =     $95
KEY_LTARROW  =     $88
KEY_ENTER    =     $8D
KEY_ESC      =     $9B
KEY_TAB      =     $89
KEY_DEL      =     $FF



*************************************************************
* NOT PART OF THE ROMS!  RATHER, THIS IS FROM PRODOS8 !!!   *
* Still, as it's part of the "standard" Apple II ecosystem, *
* I wanted to start making them available in this file.     *
*************************************************************
MLI          equ   $BF00



*************************************
* LORES / DOUBLE LORES / TEXT LINES *
*************************************
Lo01         equ   $400
Lo02         equ   $480
Lo03         equ   $500
Lo04         equ   $580
Lo05         equ   $600
Lo06         equ   $680
Lo07         equ   $700
Lo08         equ   $780
Lo09         equ   $428
Lo10         equ   $4a8
Lo11         equ   $528
Lo12         equ   $5a8
Lo13         equ   $628
Lo14         equ   $6a8
Lo15         equ   $728
Lo16         equ   $7a8
Lo17         equ   $450
Lo18         equ   $4d0
Lo19         equ   $550
Lo20         equ   $5d0
* the "plus four" lines
Lo21         equ   $650
Lo22         equ   $6d0
Lo23         equ   $750
Lo24         equ   $7d0

