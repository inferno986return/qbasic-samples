'From: TONY CAVE                 Conference: FidoQBasic   (139)
'Date: 06-07-96 03:01               Subject: illusion                  
'ILLUSION.BAS by tony cave
'This creates the illusion of steel spheres.  The radius of
'each sphere depends on its x,y coordinates.  A large slow moving
'circle makes any sphere close to it shrink.  You'll
'see what I mean if  you let it run a while.
'I hope you like it.
'DEFINT A-Z
SCREEN 13
FOR p = 1 TO 31
  PALETTE p, p * 2 + 65536 * INT(p * 2) + 256 * INT(p * 2)
NEXT p
DIM rds(255)
FOR rad = 1 TO 32: rds(rad) = INT((SIN(rad / 4) + 1) * 16): NEXT rad
Lf = .1
DO
t = t + 1
IF t = 200 THEN t = 0: L = L + Lf: IF L >= 1 OR L <= 0 THEN Lf = -Lf
x = RND * 320: y = RND * 200: 'per = RND
per = ABS(L - (SQR((ABS(x - 160) ^ 2 + ABS(y - 100) ^ 2)) / 188.6796))
'per = 1 - (SQR((ABS(x - 160) ^ 2 + ABS(y - 100) ^ 2)) / 188.6796)
FOR rad = 1 TO 32
  CIRCLE (x, y), rad * per, 255 - rad                 'bb's
  CIRCLE (x - 1, y), rad * per, 255 - rad             'bb's
  CIRCLE (x, y + 1), rad * per, 32 - rad              'bb's
  CIRCLE (x - 1, y + 1), rad * per, 32 - rad          'bb's
  'CIRCLE (x, y + 1), rad * per, 31 - rds(rad)'32-rad 'donuts
  'CIRCLE (x, y), rad * per, 31 - rds(rad) '32-rad    'donuts
NEXT rad
SOUND 6000, .03: SOUND 20000, 1
'SOUND 3000, .03: SOUND 20000, 1
LOOP UNTIL INKEY$ <> ""
SCREEN 0: WIDTH 80, 25: SYSTEM

