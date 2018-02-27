'From: Joshua Dickerson          Conference: FidoQBasic   (139)
'Date: 01-27-97 17:14              BBS Name: Exec-PC
'Subject: Mad Planets!!!                BBS ID: EXECPC
'Note: This may not be accurate..
'Its main purpose is to provide an illusion.
CLS
PRINT "QBASIC MAD PLANETS! WRITTEN BY JOSHUA DICKERSON."
PRINT "Assume this is a binary star system.."
INPUT "Enter the Number of planets ->", NumBodies
NumBodies = NumBodies + 1
TYPE BodyType
  Mass AS DOUBLE
  x AS DOUBLE
  y AS DOUBLE
  xV AS DOUBLE
  yV AS DOUBLE
  C AS INTEGER
END TYPE
DIM Body(NumBodies) AS BodyType
RANDOMIZE TIMER
SCREEN 12
  Body(0).x = 320
  Body(0).y = 240
  Body(0).Mass = 2872550111222#
  Body(0).xV = 2
  Body(0).yV = 0
  Body(0).C = 14
  CIRCLE (320, 240), 4, 14
  
  Body(1).x = 320
  Body(1).y = 200
  Body(1).Mass = 2872550111222#
  Body(1).xV = -2
  Body(1).yV = 0
  Body(1).C = 14
  CIRCLE (320, 200), 4, 14
FOR I = 2 TO NumBodies
  Body(I).x = 640 * RND
  Body(I).y = 320 * RND
  Body(I).Mass = RND * 900000 + 100000
  Body(I).xV = 1 - RND * 2
  Body(I).yV = 1 - RND * 2
  Body(I).C = 14 * RND + 1
  CIRCLE (Body(I).x, Body(I).y), 4, Body(I).C
NEXT
DO
  FOR I = 0 TO NumBodies
     FOR j = 0 TO NumBodies
        IF I <> j THEN
           xDist = Body(j).x - Body(I).x
           yDist = Body(j).y - Body(I).y
           Dist = xDist ^ 2 + yDist ^ 2
           G = Body(j).Mass
           M = Body(I).Mass
           gF = 6.672E-11 * (G * M / Dist)
           Dist = SQR(Dist)
           Body(I).xV = Body(I).xV + ((xDist / Dist) * gF / M)
           Body(I).yV = Body(I).yV + ((yDist / Dist) * gF / M)
        END IF
     NEXT
  NEXT
  WAIT &H3DA, 8
  FOR I = 0 TO NumBodies
     CIRCLE (Body(I).x, Body(I).y), 4, 0
     Body(I).x = Body(I).x + Body(I).xV
     Body(I).y = Body(I).y + Body(I).yV
     CIRCLE (Body(I).x, Body(I).y), 4, Body(I).C
  NEXT
LOOP UNTIL INKEY$ <> ""
END

