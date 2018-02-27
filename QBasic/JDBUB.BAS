'From: Joshua Dickerson          Conference: FidoQBasic   (139)
'Date: 12-24-96 12:00              BBS Name: Exec-PC
'Subject: BUBBLES.BAS                   BBS ID: EXECPC
DEFINT A-Z
'Written by Joshua Dickerson
'(C) 1996
CONST NumBubbles = 50
'       ^^^^
'Increase this to slow things down if nessesary.. I'm running on a 25 Mgz
'Ast Advantage! Explorer notebook. You most likely have something more
'powerful.
TYPE BubbleType
  x AS INTEGER
  Y AS INTEGER
  Speed AS SINGLE
  bSize AS INTEGER
  aSize AS SINGLE
END TYPE
DIM Bubbles(NumBubbles) AS BubbleType
SCREEN 12
FOR I = 0 TO NumBubbles
  Bubbles(I).x = 500 * RND + 80
  Bubbles(I).Y = 479
  Bubbles(I).Speed = 0
  Bubbles(I).bSize = RND * 25 + 1
  Bubbles(I).aSize = 0
NEXT
DO
  FOR I = 0 TO NumBubbles
     CIRCLE (Bubbles(I).x, Bubbles(I).Y), Bubbles(I).aSize!, 0
     Bubbles(I).Speed! = Bubbles(I).Speed! + .07 * Bubbles(I).bSize
     Bubbles(I).Y = Bubbles(I).Y - Bubbles(I).Speed!
     IF Bubbles(I).aSize! < Bubbles(I).bSize THEN Bubbles(I).aSize! = Bubbles(I).aSize + Bubbles(I).bSize * .05
     IF Bubbles(I).Y < -10 THEN
        Bubbles(I).x = 500 * RND + 80
        Bubbles(I).Y = 479
        Bubbles(I).Speed! = 0
        Bubbles(I).bSize = RND * 25 + 1
        Bubbles(I).aSize = 0
     ELSE
        CIRCLE (Bubbles(I).x, Bubbles(I).Y), Bubbles(I).aSize!, 9
        CIRCLE (Bubbles(I).x, Bubbles(I).Y), Bubbles(I).aSize!, 15, .1, .875
     END IF
  NEXT
LOOP UNTIL INKEY$ <> ""
END

