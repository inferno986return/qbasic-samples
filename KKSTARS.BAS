'_|_|_|   KKSTARS.BAS
'_|_|_|   This program demonstrates a method of emulating a
'_|_|_|   moving starfield in 10 planes in QuickBasic graphics.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba (12/16/96)
TYPE stardata
   C AS INTEGER
   D AS INTEGER
   X AS INTEGER
   Y AS INTEGER
END TYPE
RANDOMIZE TIMER
f% = 500 'C$ = COMMAND$: f% = VAL(C$) ' use command line if compiling
IF f% < 100 THEN
   PRINT "USAGE: KKSTARS n[number of stars 100-5000]"
   SYSTEM
END IF
IF f% > 4999 THEN f% = 4999
DIM star(f% + 1) AS stardata
SCREEN 12
COLOR 11: LOCATE 10, 33, 0: PRINT "Initializing..."
FOR n% = 0 TO f%
   star(n%).C = 0: star(n%).D = n% MOD 10
   star(n%).X = (n% * 3) MOD 480
   star(n%).Y = (n% * 4) MOD 640
NEXT
FOR n% = 0 TO 9000
   SWAP star(n% MOD f%).X, star(RND * f%).X
   SWAP star(n% MOD f%).Y, star(RND * f%).Y
NEXT
WHILE INKEY$ <> "": WEND
CLS
WHILE INKEY$ = ""
   FOR n% = 0 TO f%
      star(n%).C = star(n%).C + 1
      IF star(n%).C >= star(n%).D THEN
         Y% = star(n%).Y: X% = star(n%).X
         nY% = Y% - 1: nX% = X%
         IF nY% < 0 THEN
            nY% = 639: nX% = RND * 479
            star(n%).D = 1 + RND * 9
         END IF
         star(n%).Y = nY%: star(n%).X = nX%
         PSET (Y%, X%), 0: PSET (nY%, nX%), 15
         star(n%).C = 0
      END IF
   NEXT
WEND: SCREEN 0: SYSTEM
'_|_|_|   end   KKSTARS.BAS

