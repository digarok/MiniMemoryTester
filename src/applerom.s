**************************************************
* Apple Standard Memory Locations
**************************************************
CH           EQU   $24
CV           EQU   $25

CLRLORES     equ   $F832
LORES        equ   $C050
TXTSET       equ   $C051
MIXCLR       equ   $C052
MIXSET       equ   $C053
TXTPAGE1     equ   $C054
TXTPAGE2     equ   $C055
KEY          equ   $C000
C80STOREOFF  equ   $C000
C80STOREON   equ   $C001
STROBE       equ   $C010
SPEAKER      equ   $C030
VBL          equ   $C02E
RDVBLBAR     equ   $C019       ;not VBL (VBL signal low

RAMWRTAUX    equ   $C005
RAMWRTMAIN   equ   $C004
SETAN3       equ   $C05E       ;Set annunciator-3 output to 0
SET80VID     equ   $C00D       ;enable 80-column display mode (WR-only)



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

