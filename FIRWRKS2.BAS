DECLARE SUB Delay ()
'============================================================================
'     To: ALL                           Number: 48       Refer: 0     
'   From: DAVE MORROW               Conference: QUIK_BAS -Qui (298)
'   Date: 08-06-99 03:11              BBS Name: YE OLDE PAWN SHOPPE ]I[
'Subject: Better Fireworks              BBS ID: YOPSIII
'----------------------------------------------------------------------------
'
'Here is some better fireworks. Play with the values for the
'delay variables so it works right for you.

SCREEN 13
DEFINT A-Z
RANDOMIZE TIMER
DO
  CLS
  GREEN = 0: RED = 0: BLUE = 0
   RNDCLR = (RND * 2)
    FOR PAL = 0 TO 63
   IF INKEY$ <> "" THEN SYSTEM
   IF RNDCLR = 0 THEN RED = PAL
   IF RNDCLR = 1 THEN BLUE = PAL
   IF RNDCLR = 2 THEN GREEN = PAL
   FIREWORKS.PALETTE& = (65536 * BLUE) + (256 * GREEN) + RED
   PALETTE PAL, FIREWORKS.PALETTE&
  NEXT PAL
  X = ((RND * 32767) MOD 300) + 10
  Y = ((RND * 32767) MOD 180) + 10
 FOR GOING.UP = 199 TO Y STEP -1
  IF INKEY$ <> "" THEN SYSTEM
  PSET (X, GOING.UP), 63
  Delay
  PSET (X, GOING.UP), 0
 NEXT GOING.UP
  PSET (X, Y), 63
 FOR PAL = 63 TO 0 STEP -1
  IF INKEY$ <> "" THEN SYSTEM
  PSET (X - (RND * (63 - PAL)), Y - (RND * (63 - PAL))), PAL
  PSET (X + (RND * (63 - PAL)), Y + (RND * (63 - PAL))), PAL
  PSET (X + (RND * (63 - PAL)), Y - (RND * (63 - PAL))), PAL
  PSET (X - (RND * (63 - PAL)), Y + (RND * (63 - PAL))), PAL
  Delay
 NEXT PAL
  FOR PAL = 63 TO 0 STEP -1
  PALETTE PAL, 0
 NEXT PAL
LOOP WHILE INKEY$ = ""
SYSTEM
'
'
'--- Maximus 3.01
' * Origin: BitByters BBS, Jim Brown (Sysop) Rockland, ON, Can. (1:163/215)    
                                 

SUB Delay : FOR I = 0 TO 250
  WAIT &H3DA, 1: WAIT &H3DA, 1, 1
NEXT: END SUB

