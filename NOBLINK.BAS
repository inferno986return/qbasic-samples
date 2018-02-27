'_|_|_|   NOBLINK.BAS
'_|_|_|   Demonstrates a method of turning off text blinking, and
'_|_|_|   enabling the use of high intensity backgrounds.
'_|_|_|   No warrantee or guarantee given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN  by Kurt Kuzba.  (8/24/96)
'$INCLUDE: 'qb.bi'
DIM Rin AS RegType
DIM Rout AS RegType
COLOR 2, 0: SCREEN 0: CLS : COLOR 17, 6
LOCATE 10, 20: PRINT " Hit any key to change to bright background. ";
LOCATE 11, 20: PRINT " Then hit any key to change back to normal.  ";
LOCATE 12, 20: PRINT " Please notice that the blink or brightness  ";
LOCATE 13, 20: PRINT " attribute is controlled in the FOREGROUND   ";
LOCATE 14, 20: PRINT " argument by adding 16 to the color number.  ";
WHILE INKEY$ = "": WEND
COLOR 2, 0
Rin.ax = &H1003
Rin.bx = 0
CALL INTERRUPT(16, Rin, Rout)
WHILE INKEY$ = "": WEND
Rin.ax = &H1003
Rin.bx = 1
CALL INTERRUPT(16, Rin, Rout)
'_|_|_|   end  NOBLINK.BAS

