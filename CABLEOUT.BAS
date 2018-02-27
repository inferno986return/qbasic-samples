'============================================================================
'     To: ALL                           Number: 59       Refer: 0     
'   From: DAVE MORROW               Conference: QUIK_BAS -Qui (298)
'   Date: 07-28-99 03:04              BBS Name: YE OLDE PAWN SHOPPE ]I[
'Subject: Static                        BBS ID: YOPSIII
'----------------------------------------------------------------------------
'
 'This is what happens when your cable goes out!!!
 'It works really fast in FirstBasic
 'It takes about a minute and a half to draw the pages out in Qbasic
 'but then it is pretty fast.

SCREEN 7, , 0, 0
DEFINT A-Z
RANDOMIZE TIMER

'This draws the eight frames, one on each video page
FOR SCREEN.PAGE = 0 TO 7
 SCREEN 7, , SCREEN.PAGE, SCREEN.PAGE
 FOR Y = 0 TO 199
  FOR X = 0 TO 319
   RANDOM.STATIC = RND * 2
   IF RANDOM.STATIC = 0 THEN CLR = 7
   IF RANDOM.STATIC = 1 THEN CLR = 8
   IF RANDOM.STATIC = 2 THEN CLR = 15
   PSET (X, Y), CLR
  NEXT X
 NEXT Y
NEXT SCREEN.PAGE

'This part displays the pages in sequence until a key is pressed.
 DO
  VISUAL.PAGE = VISUAL.PAGE + 1
  IF VISUAL.PAGE > 7 THEN VISUAL.PAGE = 0
  SCREEN 7, , 0, VISUAL.PAGE
 LOOP WHILE INKEY$ = ""

'--- Maximus 3.01
' * Origin: BitByters BBS, Jim Brown (Sysop) Rockland, ON, Can. (1:163/215)    
                                                                                
                                         

