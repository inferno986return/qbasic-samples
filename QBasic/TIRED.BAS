'From: Mandie N.                 Conference: FidoQBasic   (139)
'Date: 05-28-96 22:43            Subject: tired.bas                  
'tired.bas
'the result of too many waking hours
DECLARE SUB CheckVar ()
COMMON SHARED xx, yy, cc, x, y, c
SCREEN 13: c = 16
COLOR 4: PRINT "TIRED -< the lesser one >- ": SLEEP 2: CLS
xx = RND * 2: yy = RND * 2: cc = 1
DO
FOR r = 16 TO 1 STEP -1
 CIRCLE (x, y), r, c
  PAINT (x, y), c
 CheckVar
 IF INKEY$ <> "" THEN END
NEXT
FOR r = 1 TO 16
 CIRCLE (x, y), r, c
  PAINT (x, y), c
 CheckVar
 IF INKEY$ <> "" THEN END
NEXT
LOOP

SUB CheckVar
 x = x + xx: y = y + yy: c = c + cc
  IF x < 0 OR x > 320 THEN
   xx = -xx
  END IF
  IF y < 0 OR y > 200 THEN
   yy = -yy
  END IF
  IF c < 17 OR c > 30 THEN
   cc = -cc
  END IF
 SOUND 0, .2
END SUB

