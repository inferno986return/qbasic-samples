'From: Alex Wellerstein          Conference: FidoQBasic   (139)
'Date: 06-07-96 09:36            Subject: Wow!                        
'Try this everybody, it's puurrrttyyy!
' ( btw i picked up the WAIT 986, 8 part from this echo...
'   makes the animation a bit smoother )...
DEFINT A-Z
SCREEN 12
x = 0: xx = -1: y = 0: yy = -1: speed% = 5
IF LEN(COMMAND$) THEN speed% = VAL(COMMAND$)
IF speed% < 1 THEN speed% = 5
DIM buf(4500) AS INTEGER
LINE (0, 0)-(125, 70), 15, B
LINE (10, 10)-(115, 60), , B
PAINT (2, 2)
LINE (20, 20)-(25, 50), , BF
LINE (40, 20)-(45, 50), , BF
LINE (60, 20)-(65, 50), , BF
LINE (80, 20)-(85, 50), , BF
LINE (100, 20)-(105, 50), , BF
LINE (20, 20)-(40, 50)
LINE (25, 20)-(45, 50)
LINE (100, 20)-(80, 50)
LINE (105, 20)-(85, 50)
PAINT (95, 30)
PAINT (30, 30)
GET (0, 0)-(125, 70), buf
DO WHILE INP(96) <> 1
   IF x > 509 THEN xx% = -1
   IF x < 5 THEN xx% = 1
   IF y > 404 THEN yy% = -1
   IF y < 5 THEN yy% = 1
   tx% = x + xx% * speed%: ty% = y + yy% * speed%
   WAIT &H3DA, 8: WAIT &H3DA, 8, 8
   PUT (tx%, ty%), buf, XOR
   PUT (x, y), buf, XOR
   x = tx%: y = ty%
LOOP: WHILE INKEY$ <> "": WEND: SCREEN 0

