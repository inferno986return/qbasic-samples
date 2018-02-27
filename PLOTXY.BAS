'_|_|_|   PLOTXY.BAS   PUBLIC DOMAIN   Kurt Kuzba   (6/17/1998)
'_|_|_|   Plot an array of points to scale in QBasic and QB45.
'_|_|_|   No warrantee or guarantee given or implied.
TYPE XYcoord
   x AS INTEGER
   y AS INTEGER
END TYPE
DECLARE SUB Plotxy (xy() AS XYcoord, C%, Scale)
DECLARE SUB Plotline (xy() AS XYcoord, C%, Scale)
SCREEN 13
CONST CARTX = 99
CONST CARTY = 159
DIM xy(1 TO 20) AS XYcoord
FOR z% = 1 TO 20: READ xy(z%).y: READ xy(z%).x: NEXT
DATA   0,0,   1,-1,   2,-1,   3,0,   4, 1,   4, 2,   4,3
DATA   3,4,   2, 5,   1, 6,   0,7,  -1, 6,  -2, 5,  -3,4
DATA  -4,3,  -4, 2,  -4, 1,  -3,0,  -2,-1,  -1,-1
'_|_|_|  sung to the tune of Jackson Brown's
'_|_|_|  "In the Shape of a Heart"
DO
   z% = z% MOD 200 + 1: Scale = Scale + .1
   IF (Scale > 10) OR (Scale < 1) THEN Scale = 1
   Plotxy xy(), z%, Scale
   Plotline xy(), z%, Scale
LOOP WHILE INKEY$ = ""
SCREEN 0: WIDTH 80, 25
 
SUB Plotline (xy() AS XYcoord, C%, Scale)
   af% = LBOUND(xy)
   zf% = UBOUND(xy)
   FOR z% = af% TO zf%
      y1% = CARTY + xy(z%).y * Scale
      x1% = CARTX + xy(z%).x * Scale
      IF z% = zf% THEN
         y2% = CARTY + xy(af%).y * Scale
         x2% = CARTX + xy(af%).x * Scale
      ELSE
         y2% = CARTY + xy(z% + 1).y * Scale
         x2% = CARTX + xy(z% + 1).x * Scale
      END IF
      LINE (y1%, x1%)-(y2%, x2%), C%
   NEXT
END SUB
 
SUB Plotxy (xy() AS XYcoord, C%, Scale)
   FOR z% = LBOUND(xy) TO UBOUND(xy)
      PSET (CARTY + xy(z%).y * Scale, CARTX + xy(z%).x * Scale), C%
   NEXT
END SUB
 
