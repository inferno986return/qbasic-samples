'_|_|_|   CONFETTI.BAS
'_|_|_|   This program demonstrates a method of emulating the
'_|_|_|   motion of falling confetti in QuickBasic graphics.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba (1/1/96)
RANDOMIZE (TIMER + INP(64))
f% = VAL(COMMAND$): IF f% < 50 THEN f% = 49
c% = 0: cl% = 15: IF f% > 200 THEN f% = 199
DIM flake%(200, 3, 2): DIM init%(320)
FOR num% = 0 TO 319: init%(num%) = num%: NEXT
FOR num% = 0 TO 1000: SWAP init%(num% MOD 320), init%(RND * 319): NEXT
FOR num% = 0 TO f%
   FOR dist% = 0 TO 2: flake%(num%, dist%, 1) = init%(num%): NEXT
NEXT
FOR num% = 0 TO 199: init%(num%) = num%: NEXT
FOR num% = 0 TO 1000: SWAP init%(num% MOD 200), init%(RND * 199): NEXT
FOR num% = 0 TO f%
   FOR dist% = 0 TO 2: flake%(num%, dist%, 0) = init%(num%): NEXT
NEXT: WHILE INKEY$ <> "": WEND: SCREEN 13
WHILE INKEY$ = ""
   FOR num% = 0 TO f%
      FOR dist% = 0 TO 2
         x% = flake%(num%, dist%, 0): y% = flake%(num%, dist%, 1)
         xn% = x% + dist% + 1 + ((RND * 99) MOD 2)
         IF xn% > 199 THEN
            xn% = RND * 3: yn% = RND * 319
         ELSE
            d% = RND * 999: yn% = y% + (d% > 800) - (d% < 199)
            yn% = ABS(yn%) + (yn% > 319)
         END IF
         PSET (y%, x%), cl%: PSET (yn%, xn%), 15
         flake%(num%, dist%, 0) = xn%: flake%(num%, dist%, 1) = yn%
      NEXT
   NEXT: c% = (c% + 1) MOD 1000
   IF c% = 0 THEN cl% = RND * 64: cl% = cl% - (cl% = 15)
WEND: SCREEN 0
'_|_|_|   end   CONFETTI.BAS

