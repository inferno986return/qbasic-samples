'From: Trevor Sinclair           Conference: Quik_Bas (3)
'Date: 09-02-96 15:26            Subject: Mystify                    
'Hey people, here's my mystify code I though some of you could maybe use.
'Type a number on the commandline as a paramater to Mystify.exe and that
'will be the number of lines.  You can form a nice web by doing mystify 200.
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' MYSTIFY.BAS    - by Trevor Sinclair for the public domain.
' :::::::::::
' :::::::::::    - This is my QBasic version of Mystify, like the one
' :::::::::::      that comes with Windows.  If you change the screen
' :::::::::::      mode, be sure to change the maxx, maxy variables.
' :::::::::::    - This is anybody's source code now, so use it in your
' :::::::::::      programs and don't worry about giving me credit.
'
'                - You may wish to add a delay.  On my 386 this was a bit
'                  too fast.

'          ******* Thanks to Kurt Kuzba for the Delay sub *******

DECLARE SUB delay (hold!)

DEFINT A-Z                ' all untypes variables are int

OPTION BASE 1             ' Arrays start at 1
RANDOMIZE TIMER           ' randomize randomizer

SCREEN 9

TYPE dotType
  x AS INTEGER
  y AS INTEGER
END TYPE

numlines = VAL(COMMAND$)
IF numlines < 2 THEN numlines = 5

DIM Points(numlines, 4) AS dotType   ' the main array
DIM Incs(4) AS dotType        ' holds the dots' increments
head = 1
tail = 2

col = RND * 14 + 1            ' pick a color
colCounter = 0                ' counts up to certain amount then
                              '  changes color

maxx = 640                    ' change these to the maximum coordinates
maxy = 350                    ' for your screen mode

FOR i = 1 TO 4                ' generate initial increments
  DO
    xinc = RND * 10 - 5
    yinc = RND * 10 - 5
  LOOP UNTIL xinc <> 0 AND yinc <> 0
  Incs(i).x = xinc
  Incs(i).y = yinc
NEXT i

FOR i = 1 TO 4                ' starting spots for each point in the thingy
  Points(head, i).x = RND * maxx
  Points(head, i).y = RND * maxy
NEXT i

DO                                        ' main loop

  colCounter = colCounter + 1             ' increment colCounter
  IF (colCounter > 50) THEN              ' change current color when
    col = RND * 14 + 1                    '  colCounter reaches 200
    colCounter = 0
  END IF

  FOR i = 1 TO 3
    x1 = Points(tail, i).x: x2 = Points(tail, i + 1).x     ' erase most last
    y1 = Points(tail, i).y: y2 = Points(tail, i + 1).y     '  thingy drawn
    LINE (x1, y1)-(x2, y2), 0
  NEXT i
  x1 = Points(tail, 4).x: x2 = Points(tail, 1).x
  y1 = Points(tail, 4).y: y2 = Points(tail, 1).y
  LINE (x1, y1)-(x2, y2), 0

  oldHead = head                     ' change the pointers to the tail
  head = tail                        '  and head elements.  This simulates
  tail = tail + 1                    '  a stack.
  IF tail > numlines THEN tail = 1          ' make sure stack doesn't overflow
  
  FOR i = 1 TO 4                     ' make new shape
    Points(head, i).x = Points(oldHead, i).x + Incs(i).x
    Points(head, i).y = Points(oldHead, i).y + Incs(i).y
  NEXT i

  FOR i = 1 TO 4                     ' check that the thingy isn't off the
    temp = RND * 10 + 1              '  screen, invert increments if it is.
    IF (Points(head, i).x > maxx - 12) THEN Incs(i).x = -temp
    IF (Points(head, i).x < 12) THEN Incs(i).x = temp
    IF (Points(head, i).y > maxy - 12) THEN Incs(i).y = -temp
    IF (Points(head, i).y < 12) THEN Incs(i).y = temp
  NEXT i

  FOR i = 1 TO 3                      ' now draw the current thingy
    x1 = Points(head, i).x: x2 = Points(head, i + 1).x
    y1 = Points(head, i).y: y2 = Points(head, i + 1).y
    LINE (x1, y1)-(x2, y2), col
  NEXT i
  x1 = Points(head, 4).x: x2 = Points(head, 1).x
  y1 = Points(head, 4).y: y2 = Points(head, 1).y
  LINE (x1, y1)-(x2, y2), col
   
  delay .01                           ' delay program operation.
                                      '  Thank you Kurt Kuzba for this SUB
                                      '  I was wondering how one did that..
LOOP WHILE INKEY$ = ""   ' stop when keyboard has been pressed

SCREEN 0                 ' return to text screen mode

' Okay basically what this does is hold the data for all the 4 corners of
' the quadrilateral, and saves the data for the last five dots.  By simulating
' a stack, with wrap-around pointers to the head and tail, it uses the same
' five elements over and over again.  That is the best I can explain it.
'
' Made by Trevor Sinclair, September 1st, 1996.
'* Origin: <FIDONET> Late Nite DIVERSIONS * Sarnia Ontario
'* 519-332-0241 * (1:246/104)

SUB delay (hold!)   ' Thanks to Kurt Kuzba for this function
' pause program in .05 second intervals.

DEF SEG = 0
IF hold! < (3600 / 65536) THEN hold! = (3600 / 65536)
WHILE hold! > 14.0625
   hold% = PEEK(&H46C)
   WHILE PEEK(&H46C) = hold%: WEND
   WHILE PEEK(&H46C) <> hold%: WEND
   hold! = hold! - 14.0625
WEND
hold% = hold! * 18.20444444#
hold% = (PEEK(&H46C) + hold%) AND 255
   WHILE PEEK(&H46C) <> hold%: WEND

END SUB

