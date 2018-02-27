DECLARE SUB MoveCirc ()
SCREEN 12: CLS : CIRCLE (320, 240), 10, 10: PAINT (320, 240), 10
LOCATE 1, 1: PRINT "Use cursor keys. ESC to exit."
DIM cr(200) AS INTEGER: GET (310, 230)-(330, 250), cr
x% = 310: y% = 230: nx% = 310: ny% = 230
DO
   k% = INP(96): toss$ = INKEY$
   SELECT CASE k%
      CASE 72: ny% = y% + (y% > 0): GOSUB MoveCirc
      CASE 75: nx% = x% + (x% > 0): GOSUB MoveCirc
      CASE 77: nx% = x% - (x% < 619): GOSUB MoveCirc
      CASE 80: ny% = y% - (y% < 459): GOSUB MoveCirc
   END SELECT
LOOP WHILE k% <> 1
WHILE INKEY$ <> "": WEND
SCREEN 0: END
MoveCirc:
   PUT (nx%, ny%), cr, XOR
   PUT (x%, y%), cr, XOR
   x% = nx%: y% = ny%: RETURN

