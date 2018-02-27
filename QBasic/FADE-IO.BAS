'_|_|_|   FADE-IO.BAS
'_|_|_|   This program used the PALETTE statement to fade in and
'_|_|_|   out a welcoming message. Greater complexity is possible.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (1/21/96)
SCREEN 12: : COLOR 1
'_|_|_|   We are only changing color 1, normally BLUE, in this demo
PALETTE 1, 0: LOCATE 7
LOCATE , 15: PRINT "²²²   ²²²  ²²²²²²²  ²²²       ²²²       ²²²²²²²²²"
LOCATE , 15: PRINT "²²²°  ²²²° ²²²²²²²° ²²²°      ²²²°      ²²²²²²²²²°"
LOCATE , 15: PRINT "²²²°  ²²²° ²²²°°°°° ²²²°      ²²²°      ²²²°°°²²²°"
LOCATE , 15: PRINT "²²²°  ²²²° ²²²°     ²²²°      ²²²°      ²²²°  ²²²°"
LOCATE , 15: PRINT "²²²²²²²²²° ²²²²²²   ²²²°      ²²²°      ²²²°  ²²²°"
LOCATE , 15: PRINT "²²²²²²²²²° ²²²²²²°  ²²²°      ²²²°      ²²²°  ²²²°"
LOCATE , 15: PRINT "²²²°°°²²²° ²²²°°°°  ²²²°      ²²²°      ²²²°  ²²²°"
LOCATE , 15: PRINT "²²²°  ²²²° ²²²°     ²²²°      ²²²°      ²²²°  ²²²°"
LOCATE , 15: PRINT "²²²°  ²²²° ²²²°     ²²²°      ²²²°      ²²²°  ²²²°"
LOCATE , 15: PRINT "²²²°  ²²²° ²²²²²²²  ²²²²²²²²  ²²²²²²²²  ²²²²²²²²²°"
LOCATE , 15: PRINT "²²²°  ²²²° ²²²²²²²° ²²²²²²²²° ²²²²²²²²° ²²²²²²²²²°"
LOCATE , 15: PRINT " °°°   °°°  °°°°°°°  °°°°°°°°  °°°°°°°°  °°°°°°°°°"
COLOR 2: PRINT "Hit a key"
FOR i& = 1 TO 126
   WAIT (&H3DA), 8: WAIT (&H3DA), 8, 8
   PALETTE 1, 65536 * (i& \ 3) + 256 * (i& \ 2) + (i& \ 4)
   '_|_|_|   COLOR-MIX [ BLUE ]    [ GREEN ]      [ RED ]
NEXT
SOUND 416, 4: SOUND 524, 7: SOUND 468, 4: SOUND 627, 9
FOR i& = 125 TO 0 STEP -1
   WAIT (&H3DA), 8: WAIT (&H3DA), 8, 8
   WAIT (&H3DA), 8: WAIT (&H3DA), 8, 8
   PALETTE 1, 65536 * (i& \ 3) + 256 * (i& \ 2) + (i& \ 4)
NEXT
WHILE INKEY$ = "": WEND
PALETTE   '_|_|_|   Reset to the default colors
SCREEN 0
'_|_|_|   end   FADE-IO.BAS


