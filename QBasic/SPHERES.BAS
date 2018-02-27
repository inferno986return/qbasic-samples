REM == SPHERES.BAS ==

DEFINT A-Z: DEFDBL C

CLS

SCREEN 13
Xmax = 320
Ymax = 200

VIEW (0, 0)-(Xmax - 1, Ymax - 1)
WINDOW (0, 0)-(Xmax, Ymax)

' generate shading palette
 attr = 1
 PALETTE
FOR Blue = 63 TO 1 STEP -1
 colornumber = 65536 * Blue
 PALETTE attr, colornumber
 attr = attr + 1
NEXT Blue

FOR green = 63 TO 1 STEP -1
 colornumber = 256 * green
 PALETTE attr, colornumber
 attr = attr + 1
NEXT green

FOR red = 63 TO 1 STEP -1
 colornumber = red
 PALETTE attr, colornumber
 attr = attr + 1
NEXT red

RANDOMIZE TIMER

DO

  Xp = INT(RND * Xmax)
  Yp = INT(RND * Ymax)
  r = INT(RND * 30 + 1)
  k = INT(RND * 64)
  n = INT(RND * 3) * 63
'  v = (63 + k) AND 63 - r   '<==large shading range
  v = 31 - r                 '<==small shading range

  FOR i = r TO 1 STEP -1
   attr = n + v + i
   CIRCLE (Xp, Yp), i, attr
   PAINT (Xp, Yp), attr
  NEXT i

  AnyKey$ = INKEY$          ' Any key pressed, then get it

LOOP UNTIL LEN(AnyKey$)     ' Loop till a key is pressed

SCREEN 0
WIDTH 80

END  ' The End

