'_|_|_|   TESTCOLR.BAS
'_|_|_|   This program will allow the user to test their palette
'_|_|_|   definitions, showing the proper color numbers for the
'_|_|_|   16 standard colors.
'_|_|_|   No guarantee or warrantee is given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (3/29/96)
SCREEN 13: SCREEN 0: WIDTH 80, 25: COLOR 15, 1: CLS
CL$ = "@ABCDETGxyz{|}~" + CHR$(127)
LOCATE 7, 3: COLOR 14, 4:
PRINT " Use cursor keys to select color to test. ": LOCATE , 3
PRINT " Press SPACE to test color, then any key  ": LOCATE , 3
PRINT " to continue. Press ESC to exit program.  "
FOR t% = 0 TO 15
LOCATE 1, t% * 4 + 3: COLOR t%, 0
   PRINT STRING$(4, CHR$(178));
   LOCATE 3, t% * 4 + 3: COLOR 15, 1
   PRINT ASC(MID$(CL$, t% + 1)) AND 63
NEXT
DO
   LOCATE 5, 3
   PRINT "color #"; LTRIM$(STR$(c%)); "  "
   k$ = "": WHILE k$ = "": k$ = INKEY$: WEND
   k% = ASC(k$): IF k% = 0 THEN k% = -ASC(MID$(k$, 2))
   SELECT CASE k%
      CASE -72: c% = (c% + 1) MOD 256
      CASE -75: c% = (c% + 240) MOD 256
      CASE -77: c% = (c% + 16) MOD 256
      CASE -80: c% = (c% + 255) MOD 256
      CASE 32: OUT &H3C7, c%
         t% = 0: IF c% = 0 THEN t% = 63
         r% = INP(&H3C9): g% = INP(&H3C9): b% = INP(&H3C9)
         OUT &H3C8, c%: OUT &H3C9, t%: OUT &H3C9, t%: OUT &H3C9, t%
         SOUND 1000, 5: WHILE INKEY$ = "": WEND: SOUND 2000, 1
         OUT &H3C8, c%: OUT &H3C9, r%: OUT &H3C9, g%: OUT &H3C9, b%
      CASE 27: END
   END SELECT
LOOP
'_|_|_|   end   TESTCOLR.BAS
