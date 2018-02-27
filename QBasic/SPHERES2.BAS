'From: MIKE ROSS                 Conference: QUIK_BAS -Qui (298)
REM SPHERES2.BAS

DEFINT A-Z: DEFDBL C

CLS

SCREEN 13
Xmin = 0 + 10
Ymin = 0 + 10
Xmax = 320 - 80
Ymax = 200 - 10

VIEW (Xmin, Ymin)-(Xmax - 1, Ymax - 1)
WINDOW (Xmin, Ymin)-(Xmax, Ymax)

 PALETTE


' create shades for 8 colours

 attr = 1

FOR pal = 0 TO 7

 FOR shade = 31 TO 0 STEP -1

  SELECT CASE pal
   CASE 0: Blue = 31 + shade: green = shade: red = shade         'blue
   CASE 1: Blue = shade: green = 31 + shade: red = shade         'green
   CASE 2: Blue = shade: green = shade: red = 31 + shade         'red
   CASE 3: Blue = 15 + shade: green = 15 + shade: red = shade    'cyan
   CASE 4: Blue = shade: green = 15 + shade: red = 15 + shade
   CASE 5: Blue = 15 + shade: green = shade: red = 15 + shade
   CASE 6: Blue = 7 + shade: green = shade \ 2: red = 15 + shade 'grape
   CASE 7: Blue = shade \ 2: green = 5 + shade: red = 20 + shade 'gold
  END SELECT

  colornumber = 65536 * Blue + 256 * green + red

  PALETTE attr, colornumber
  attr = attr + 1

 NEXT shade

NEXT pal


 PALETTE 0, 0       ' 0 is black


RANDOMIZE TIMER

DO

  p1 = INT(RND * (Xmax - Xmin) + Xmin)
  p2 = INT(RND * (Ymax - Ymin) + Ymin)
  r = INT(RND * 23) + 8      'radius at least 8
  k = INT(RND * 32)
  n = INT(RND * 8) * 32
'  v = (31 + k) AND 31 - r   'large shading range
  v = 31 - r                 'small shading range

  FOR i = r TO 1 STEP -1
   attr = n + v + i
   CIRCLE (p1, p2), i, attr
   PAINT (p1, p2), attr
  NEXT i


  AnyKey$ = INKEY$          ' Any key pressed, then get it

LOOP UNTIL LEN(AnyKey$)     ' Loop till a key is pressed

SCREEN 0
WIDTH 80

END  ' The End
'Expert:  One who has made all the mistakes and remembers them.
'Origin: Juxtaposition BBS, Telnet:juxtaposition.dynip.com (1:167/133)    
                                     

