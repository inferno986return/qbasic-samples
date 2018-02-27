'To: All                           Number: 138      Refer: 0134  
'From: Mathew Robertson          Conference: FidoQBasic   (139)
'Date: 11-11-96 16:52              BBS Name: Exec-PC
'Subject: Graphics.  Flame2.bas         BBS ID: EXECPC
'Here is a QB version of a graphics effect I have seen done in
'other languages.  It is not very fast it crawls on a 386 even
'at the reduced screen size.  May need to compile to get it to
'run fast enough on some systems.  Just let it run for abit and
'you will see why the file name is flame2.bas
DECLARE SUB rgbset (c%, r%, g%, B%)
SCREEN 13: CLS    'setup and clear screen
DEF SEG = &HA000   'set extra seg to start of video memory
'RANDOMIZE TIMER
esc$ = CHR$(27)  'pre calculate what the escape key is.
FOR x% = 1 TO 32     'set some nice fire colours
 CALL rgbset(x%, x% * 2 - 1, 0, 0)
 CALL rgbset(x% + 32, 63, x% * 2 - 1, 0)
 CALL rgbset(x% + 64, 63, 63, x% * 2 - 1)
 CALL rgbset(x% + 96, 63, 63, 63)
NEXT x%
DO  'start are main loop
'cycle through all the x coordinates
FOR x% = 100 TO 219 STEP 2 'can be changed to use full screen
'cycle through all the y coordinates for each x 
FOR y% = 100 TO 201 STEP 2 'can be changed to use fulle screen
'calculate the colour to use, from what is already on the screen.
c = (POINT(x%, y%) + POINT(x% + 2, y%) + POINT(x% - 2, y%) + POINT(x% + 2, y% + 2)) / 4
      
'use one colour lower then calculated.
IF c <> 0 THEN c = c - 1
      
'write the colour to video memory.
tmp& = ((y%) * 320& + x%)  'precalculate what is simular to all
POKE (tmp&), c             'the pixels being ploted
POKE (tmp& + 1), c
tmp& = tmp& - 320
POKE (tmp&), c
POKE (tmp& + 1), c
      
       
NEXT y%
'y% = y% - 2
'rnum = (RND * 2) + 1
'PSET (x%, y%), rnum * 160
     
     
NEXT x%
LOOP UNTIL INKEY$ = esc$     'check for the escape key.
DEF SEG     'reset the extra seg to the same as the data seg.
SCREEN 0    'set text mode
WIDTH 80
END
'--Official Quik--- QScan/PCB v1.19b / 01-0441
' * Origin: Pegasus Computing Moonstone,Ont 705-835-0098 (1:252/403)

SUB rgbset (c%, r%, g%, B%)  'sets colours used in palette
OUT &H3C8, c%
OUT &H3C9, r%
OUT &H3C9, g%
OUT &H3C9, B%
END SUB

