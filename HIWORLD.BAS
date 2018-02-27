'_|_|_|   HIWORLD.BAS
'_|_|_|   A simple program to say hi to the world, with simple
'_|_|_|   music and flashing keyboard LED's.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (3/26/97)
DECLARE SUB hold (S!)
DIM N(5) AS INTEGER, D(5) AS SINGLE, I AS INTEGER
N(0) = 400: N(1) = 500: N(2) = 400: N(3) = 500: N(4) = 600
D(0) = .5: D(1) = .4: D(2) = .2: D(3) = .2: D(4) = .7: I = 0
CLS : PRINT "Hit SPACE to Exit program."
DO: K$ = INKEY$: SOUND N(I), 2: hold D(I)
I = (I + 1) MOD 6: COLOR 10 + I: LOCATE 11, 39: PRINT "HI"
LOOP WHILE K$ = "": SYSTEM

SUB hold (S!)
   IF S! > 14 THEN EXIT SUB
   DEF SEG = &H40: POKE &H17, PEEK(&H17) XOR 112
   b% = PEEK(&H6C) AND 255
   e% = (b% + FIX(S! * 18.20444)) AND 255
   WHILE (PEEK(&H6C) AND 255) <> e%: WEND: DEF SEG
END SUB

