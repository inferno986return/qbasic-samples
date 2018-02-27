'_|_|_|   INERTIA.BAS
'_|_|_|   A very basic demonstration of using gravity and inertia
'_|_|_|   on moving objects in a Quick Basic program.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (11/29/96)
SCREEN 12: COLOR 11
TYPE ball
   xV AS INTEGER: yV AS INTEGER: xT AS INTEGER: yT AS INTEGER
   xD AS INTEGER: yD AS INTEGER: x AS INTEGER: y AS INTEGER
END TYPE
DIM EX(1 TO 2) AS ball
EX(1).xT = 0: EX(1).yT = 0: EX(1).xV = 100: EX(1).yV = 500
EX(1).x = 15: EX(1).y = 15: EX(1).xD = 1: EX(1).yD = 1
EX(2).xT = 0: EX(2).yT = 0: EX(2).xV = 2000: EX(2).yV = 500
EX(2).x = 458: EX(2).y = 610: EX(2).xD = -1: EX(2).yD = -1
DO
   IF INKEY$ <> "" THEN EXIT DO
   x1% = EX(1).x: y1% = EX(1).y: x2% = EX(2).x: y2% = EX(2).y
   FOR t% = 1 TO 2
      x% = EX(t%).x: y% = EX(t%).y: v% = EX(t%).xV: d% = EX(t%).xD
      IF d% <> 0 THEN
         IF (v% <> 0) THEN
            EX(t%).xT = EX(t%).xT + v%
            IF d% > 0 THEN
               EX(t%).xV = v% - 4 * (v% < 2000)
            ELSE
               EX(t%).xV = v% + 5 * (v% > 0)
            END IF
         END IF
         IF v% <= 0 THEN
            IF x% > 467 THEN
               EX(t%).xD = 0
            ELSE
               EX(t%).xD = 1: EX(t%).xV = 1
            END IF
         END IF
         IF (x% < 11) OR (x% > 468) THEN
            EX(t%).xD = -(x% < 11) + (x% > 468)
         END IF
         IF EX(t%).xT > 1500 THEN
            EX(t%).x = x% + d%
         END IF
         EX(t%).xT = EX(t%).xT AND 2047
      END IF
      IF EX(t%).yD <> 0 THEN
         EX(t%).yT = EX(t%).yT + EX(t%).yV
         IF (y% < 11) OR (y% > 628) THEN
            EX(t%).yD = -(y% < 11) + (y% > 628)
            EX(t%).yV = EX(t%).yV - 1
         END IF
         IF EX(t%).yT > 200 THEN
            EX(t%).y = y% + EX(t%).yD
            IF EX(t%).xD = 0 THEN EX(t%).yV = EX(t%).yV - 1
         END IF
         EX(t%).yT = EX(t%).yT AND 511
         IF EX(t%).yV < 1 THEN EX(t%).yV = 0
         IF EX(t%).yV = 0 THEN EX(t%).yD = 0
      END IF
   NEXT
   FOR dla% = 1 TO 200: WAIT &H3DA, 1: WAIT &H3DA, 1, 0: NEXT
   CIRCLE (y1%, x1%), 5, 0: CIRCLE (EX(1).y, EX(1).x), 5, 14
   CIRCLE (y2%, x2%), 5, 0: CIRCLE (EX(2).y, EX(2).x), 5, 9
   motion% = (EX(1).xD <> 0) + (EX(1).yD <> 0)
   motion% = motion% + (EX(2).xD <> 0) + (EX(2).yD <> 0)
LOOP WHILE motion% <> 0
LOCATE 1, 1: PRINT "Hit any Key to Exit"
WHILE INKEY$ <> "": WEND: WHILE INKEY$ = "": WEND
SCREEN 0: COLOR 2, 0: CLS
'_|_|_|   end   INERTIA.BAS

