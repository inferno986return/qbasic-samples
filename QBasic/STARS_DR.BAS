'============================================================================
'     To: ALL                           Number: 152      Refer: 0     
'   From: DAVID ROTHSCHILD          Conference: QUIK_BAS -Qui (298)
'   Date: 08-19-97 15:20              BBS Name: YE OLDE PAWN SHOPPE ]I[
'Subject: Stars.bas 1/1                 BBS ID: YOPSIII
'----------------------------------------------------------------------------
'
'Hello,
'
'  Heres a program the basically draws stars randomly on the screen.  I
'put  in something I call (for lack of a better word) a "twinkling"
'effect.  Maybe you could put this underneath a title screen or maybe a
'screensaver, who knows?
'
'Stars.Bas  Public Domain by David Rothschild
'Feel free to do whatever  with this program!  I added comments to help you
'see what the program was doing.  Also, you can change the rate of "twinkle"
'in the "FOR L = 1 TO 10000: NEXT l" line.
'****************************************************************************
SCREEN 12: RANDOMIZE TIMER: DEFLNG A-Z   'Set it Up.
DO                                       'Start A Loop.
  A = A + 1                              'Counter Variable.
  PSET (RND * 640, RND * 480), 15        'Draw a bright star.
    IF A > 1000 THEN                     'If we did this 1000 times (equal
      FOR l = 1 TO 10000: NEXT l         'to 3000 stars) then pause.
    END IF                               '
  PSET (RND * 640, RND * 480), 7         'Draw a medium star.
    IF A > 1000 THEN                     'Pause (to increase or decrease the
      FOR l = 1 TO 10000: NEXT l         'number of stars add or subtract to
    END IF                               'the 1000 in each of the IFs).
  PSET (RND * 640, RND * 480), 8         'Draw A Dark star.
    IF A > 1000 THEN                     'Pause.
      FOR l = 1 TO 10000: NEXT l         '
    END IF                               '
  IF A > 1000 THEN                       'If we did this 1000 times (equal
    FOR I = 1 TO 100                     'to 3000 stars) then draw 100 black
      PSET (RND * 640, RND * 480), 0     'pixels for every 3 stars (helps keep
        FOR l = 1 TO 5000: NEXT l        'the amount of stars down but adds a
    NEXT I                               '"twinkling" effect.
  END IF                                 '
  A$ = INKEY$                            'Check for keypress.
LOOP WHILE A$ = ""                       'If a key was pressed then end.
'End Stars.Bas
'
'Have fun!
'
'David Rothschild
'
'--- DB 1.39/004485
' * Origin: The Diamond Bar BBS, San Dimas CA, 909-599-2088 (1:218/1001)       
                    

