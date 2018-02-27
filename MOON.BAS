'============================================================================
'     To: All                           Number: 146      Refer: 0029  
'   From: Earl Montgomery           Conference: FidoQBasic   (139)
'   Date: 11-01-95 15:42              BBS Name: Exec-PC
'Subject: Moon.Bas                      BBS ID: EXECPC
'----------------------------------------------------------------------------

 DEFINT A-Z: DIM a(100): DIM b(200): RANDOMIZE TIMER: SCREEN 12: CLS
 LOCATE 2, 2: PRINT " The space colony is located in the green square. It has completed"
 PRINT "  the assigned mission and is about to depart the planet when a  meteor shower"
 PRINT "  rains down on the planet. If the colony can survive 300 meteor strikes then"
 PRINT "  it can depart to colonize other planets."
 LOCATE 10, 25: PRINT "Press any key to continue.": SLEEP (200)
tryagain:
 CLS : LOCATE 2, 2: INPUT "Meteor showers slow, medium or fast <S,M,F>"; q$
 q$ = UCASE$(q$)
 IF q$ = "S" THEN n = 12000: GOTO makemeteor
 IF q$ = "M" THEN n = 600: GOTO makemeteor
 IF q$ = "F" THEN n = 100: GOTO makemeteor
 BEEP: GOTO tryagain
makemeteor:
 REM Make and GET meteor
 CIRCLE (10, 10), 2, 15: PAINT (11, 11), 15, 15
 GET (4, 4)-(16, 16), a: CLS
 REM Make and GET spaceship
 COLOR 15: DRAW "bm10,10;U4R1F2R3F1R2D1L8"
 PAINT (11, 9), 15, 15: GET (8, 4)-(21, 12), b: CLS
 PALETTE 8, 0: COLOR 8
 REM Draw planet and stars
 CIRCLE (300, 200), 200: PAINT (302, 202), 15, 8
 FOR x = 0 TO 400
 hh = RND * 600: vv = RND * 460: tt = RND * 10
 IF tt = 5 THEN CIRCLE (hh, vv), 1, 15: PAINT (hh, vv), 15, 15
 PSET (hh, vv), 15
 NEXT
 REM locate and draw city
putcity:
 ch = RND * 600: cv = RND * 400
 IF POINT(ch, cv) = 15 THEN GOSUB checkcity
 IF flag$ = "clear" THEN GOTO locatecraters ELSE GOTO putcity
locatecraters:
 mh = 620: mv = 2
 h = RND * 490: v = RND * 390: c = 7: size = RND * 6
 IF POINT(h, v) = 2 THEN flag$ = "hit": GOTO makecraters
 IF POINT(h, v) = 15 THEN GOSUB checkcraters
 GOTO locatecraters
makecraters:
 hadj = mh \ h
mainloop:
 IF mh > h THEN mh = mh - hadj
 IF mh < h THEN mh = h
 mv = v
 IF mh = h THEN GOTO cratergraphics
 PUT (mh, mv), a, XOR: FOR delay = 0 TO n: NEXT: PUT (mh, mv), a, XOR
 GOTO mainloop
cratergraphics:
 IF flag$ = "hit" THEN GOTO explosion
 rc = 4: FOR x = 1 TO size
 CIRCLE (h, v), x, rc: FOR delay = 0 TO 28000: NEXT: NEXT
 CIRCLE (h, v), size, c: PAINT (h, v), c, c
 IF size > 3 THEN CIRCLE (h, v), 2, 8: PAINT (h, v), 4, 8
 SOUND 50, 1.2
 COLOR 14: mc = mc + 1
 IF mc = 301 THEN GOTO flyaway
 LOCATE 28, 2: PRINT "Meteor count "; mc
 RETURN
checkcity:
 IF POINT(ch + 15, cv) = 15 THEN GOTO furthercheck:  ELSE RETURN
furthercheck:
 IF POINT(ch, cv + 15) = 15 THEN GOTO doit:  ELSE RETURN
doit:
 LINE (ch, cv)-(ch + 12, cv + 12), 2, BF: flag$ = "clear"
 RETURN
checkcraters:
 IF POINT(h + 10, v) = 15 THEN GOTO check1:  ELSE RETURN
check1:
 IF POINT(h - 10, v) = 15 THEN GOTO check2:  ELSE RETURN
check2:
 IF POINT(h, v + 10) = 15 THEN GOTO check3:  ELSE RETURN
check3:
 IF POINT(h, v - 10) = 15 THEN GOTO makecraters:  ELSE RETURN
flyaway:
 LOCATE 28, 2: PRINT "The colony has survived and sets off to colonize new planets. "
 FOR x = ch - 20 TO 610: PUT (x + 10, cv), b, XOR
 FOR delay = 0 TO 32000: NEXT: PUT (x + 10, cv), b, XOR: NEXT
 SLEEP (5)
 GOTO endofprogram
explosion:
 CIRCLE (ch + 7, cv + 7), 16, c: PAINT (ch + 2, cv + 1), c, c
 CIRCLE (ch + 7, cv + 7), 8, 4: PAINT (ch + 2, cv + 2), 4, 4
 FOR x = 37 TO 150: SOUND x, .1: SOUND 2 * x, .1: SOUND 3 * x, .1: NEXT
 LOCATE 28, 2: PRINT "The Space Colony has been destroyed! Meteor count"; mc
 SLEEP (10)
endofprogram:
 SCREEN 0: CLS : END
'--- Maximus 2.02
' * Origin: Verbose Ink * 214-437-0914 * V34/VFC/V32t/H16/V32b/FAX
'1:124/5125)
                   

