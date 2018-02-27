'============================================================================
'     To: ALL                           Number: 60       Refer: 0     
'   From: DAVE MORROW               Conference: QUIK_BAS -Qui (298)
'   Date: 07-28-99 03:02              BBS Name: YE OLDE PAWN SHOPPE ]I[
'Subject: Fireworks                     BBS ID: YOPSIII
'----------------------------------------------------------------------------
'
' Here are some crappy fireworks I made. It gets a random point on the
' screen, then does a fading explosion.
'
SCREEN 12
DEFINT A-Z
RANDOMIZE TIMER

DO
CLS
PALETTE 4, 63
RANDOM.X = RND * 639
RANDOM.Y = RND * 240
GETRANDOMSIZE:
RANDOM.SIZE = RND * 50
IF RANDOM.SIZE < 20 THEN GOTO GETRANDOMSIZE

DELAY = 1200
 FOR GOING.UP = 479 TO RANDOM.Y STEP -1
  PSET (RANDOM.X, GOING.UP), 4
  FOR I = 1 TO DELAY: NEXT I
  PSET (RANDOM.X, GOING.UP), 0
 NEXT GOING.UP

PSET (RANDOM.X, RANDOM.Y), 4

SOUND 60, 1

FOR O = 0 TO 120
 DELAY = 500
 FOR FIREWORKS.EXPLOSION = 0 TO 10
  IF INKEY$ <> "" THEN SYSTEM
  BURST1 = (RND * RANDOM.SIZE) + O: BURST2 = (RND * RANDOM.SIZE) + O
  PSET (RANDOM.X - RND * BURST1, RANDOM.Y - RND * BURST2), 4
  FOR I = 0 TO DELAY: NEXT I
  PSET (RANDOM.X + RND * BURST1, RANDOM.Y + RND * BURST2), 4
  FOR I = 0 TO DELAY: NEXT I
  PSET (RANDOM.X + RND * BURST1, RANDOM.Y - RND * BURST2), 4
  FOR I = 0 TO DELAY: NEXT I
  PSET (RANDOM.X - RND * BURST1, RANDOM.Y + RND * BURST2), 4
  FOR I = 0 TO DELAY: NEXT I
 NEXT FIREWORKS.EXPLOSION
 PALETTE 4, 63 - (O / 2)
NEXT O

LOOP WHILE INKEY$ = ""


'--- Maximus 3.01
' * Origin: BitByters BBS, Jim Brown (Sysop) Rockland, ON, Can. (1:163/215)    
                                                      

