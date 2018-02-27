'============================================================================
'     To: ALL                           Number: 103      Refer: 0    
'   From: MATTHEW PULLEY            Conference: QUIK_BAS -Qui (298)
'   Date: 01-08-98 05:23              BBS Name: YE OLDE PAWN SHOPPE ]I[
'Subject: BLOSSOM.BAS                   BBS ID: YOPSIII
'----------------------------------------------------------------------------
'
'Hey All!
'
'        I'm almost ashamed to post this, seeing as how simple it is compared
'to the stuff I usually post. (Yes, I used to usually post stuff...) Anyway,
'it's a pretty screen saver thingamajiggy. Aaron, the time is now 5:15 am, so I
'missed the zone by two hours, but oh well. You know, I think someone will
'eventually discover the secret of the universe, and the answer will be found
'at 3am. On the dot.
'
'As always, compile for better speed.
'Good ol' CHR$(27) exits at any time.
'
'-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
'
'Blossom.BAS
'Written by: Matthew Pulley,
'author of better things
'than this...

DEFINT A-Z
RANDOMIZE TIMER
SCREEN 13

FOR t = 1 TO 63
 OUT &H3C8, t
  OUT &H3C9, t \ 3
  OUT &H3C9, t \ 2
  OUT &H3C9, t
NEXT

DO

 x = RND * 160
 y = RND * 100
 Radius% = RND * 75
 Bend% = RND * 2
 Colr% = RND * 63
 
 FOR t = 1 TO Colr%
  CIRCLE (x, y), t + Radius%, t, , , Bend%
  CIRCLE (x + 1, y), t + Radius%, t, , , Bend%
  CIRCLE (-x + 319, y), t + Radius%, t, , , Bend%
  CIRCLE ((-x + 319) + 1, y), t + Radius%, t, , , Bend%
  CIRCLE (x, -y + 200), t + Radius%, t, , , Bend%
  CIRCLE (x + 1, -y + 200), t + Radius%, t, , , Bend%
  CIRCLE (-x + 319, -y + 200), t + Radius%, t, , , Bend%
  CIRCLE ((-x + 319) + 1, -y + 200), t + Radius%, t, , , Bend%
  IF INKEY$ = CHR$(27) THEN EXIT DO
 NEXT

LOOP
SCREEN 0: WIDTH 80, 25
'-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
'
'- M@
'
'... Mary had a little lamb, so why shouldn't I have a cow?
'--- GAPNet Enhanced
' * Origin: Spells, Scrolls & Wards At The Roundtable (1:264/416.0)           
'                                                   

