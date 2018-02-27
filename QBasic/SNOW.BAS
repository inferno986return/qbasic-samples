'_|_|_|   SNOW.BAS
'_|_|_|   This program demonstrates a method of emulating the
'_|_|_|   motion of falling snow in QuickBasic graphics.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba (12/15/96)
TYPE flakedata
   C AS INTEGER
   d AS INTEGER
   x AS INTEGER
   y AS INTEGER
END TYPE
RANDOMIZE TIMER
f% = 1000  'C$ = COMMAND$: f% = VAL(C$)
fore% = 15: change% = 0: rotate% = 0: back% = 33
IF INSTR(UCASE$(C$), "C") <> 0 THEN change% = -1
IF f% < 50 THEN
   PRINT " USAGE: snow n[flakes 50-5000] /C[confetti]": SYSTEM
END IF
IF f% > 4999 THEN f% = 4999
DIM flake(f% + 1) AS flakedata
FOR n% = 0 TO f%
   flake(n%).C = 0: flake(n%).d = n% MOD 5
   flake(n%).x = (n% * 3) MOD 200
   flake(n%).y = (n% * 4) MOD 320
NEXT
FOR n% = 0 TO 2000
   SWAP flake(n% MOD f%).x, flake(RND * f%).x
   SWAP flake(n% MOD f%).y, flake(RND * f%).y
NEXT
WHILE INKEY$ <> "": WEND
SCREEN 13
DEF SEG = &HA000
FOR void& = 0 TO 63999: POKE void&, 33: NEXT
WHILE INKEY$ = ""
   FOR n% = 0 TO f%
      flake(n%).C = flake(n%).C + 1
      IF flake(n%).C >= flake(n%).d THEN
         y% = flake(n%).y: x% = flake(n%).x
         IF x% = 199 THEN nx% = 0:  ELSE nx% = x% + 1
         IF nx% = 0 THEN
            ny% = RND * 319
         ELSE
            r% = RND
            ny% = (320 + y% + (r% > .6) - (r% < .4)) MOD 320
         END IF
         flake(n%).y = ny%: flake(n%).x = nx%
         PSET (y%, x%), back%: PSET (ny%, nx%), fore%
         flake(n%).C = 0
      END IF
   NEXT
   IF change% THEN
      rotate% = (rotate% + 1) MOD 2000
      IF rotate% = 0 THEN back% = fore%: fore% = RND * 15:
   END IF
WEND: SCREEN 0: WIDTH 80, 25: SYSTEM
'_|_|_|   end   SNOW.BAS

