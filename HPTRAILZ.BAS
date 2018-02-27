DECLARE SUB HappyTrails (l%)
'_|_|_|   HPTRAILZ.BAS
'_|_|_|   Happy Trailz. This program demonstrates one method
'_|_|_|   of creating a series of self-overwriting lines to
'_|_|_|   keep the CPU happy when there is nothing else to do.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (2/10/96)
TYPE LNZ
   x1 AS INTEGER: x2 AS INTEGER: y1 AS INTEGER: y2 AS INTEGER
END TYPE
l$ = COMMAND$   '_|_|_|   REM this out if using QBasic
IF VAL(l$) < 1 THEN l$ = "4"
l% = VAL(l$)
IF l% > 20 THEN l% = 20
IF l% < 2 THEN l% = 2
HappyTrails l%
'_|_|_|   end   HPTRAILZ.BAS

SUB HappyTrails (l%)
   DIM la(1 TO l%, 11) AS LNZ, C(6) AS INTEGER, K(6) AS INTEGER
   DIM dx1(1 TO l%) AS INTEGER, dx2(1 TO l%) AS INTEGER
   DIM dy1(1 TO l%) AS INTEGER, dy2(1 TO l%) AS INTEGER
   C(0) = 15: C(1) = 11: C(2) = 3: C(3) = 9: C(4) = 1: C(5) = 0
   K(0) = 14: K(1) = 13: K(2) = 12: K(3) = 4: K(4) = 5: K(5) = 0
   RANDOMIZE (TIMER + INP(64))
   FOR ln% = 1 TO l%
      dx1(ln%) = 9: dx2(ln%) = 9: dy1(ln%) = 9: dy2(ln%) = 9
      FOR fade% = 0 TO 10
         la(ln%, fade%).x1 = 320 + fade% * 2
         la(ln%, fade%).y1 = 240 + fade% * 2
         la(ln%, fade%).x2 = 320 - fade% * 2
         la(ln%, fade%).y2 = 240 - fade% * 2
      NEXT
   NEXT: SCREEN 12
   Ky$ = INKEY$
   WHILE Ky$ = ""
      FOR ln% = 1 TO l%
      WHILE (INP(&H3DA) AND 8) = 0: WEND
      WHILE (INP(&H3DA) AND 8) <> 0: WEND
      Ky$ = INKEY$: IF Ky$ <> "" THEN EXIT FOR
      FOR fade% = 10 TO 1 STEP -1
         x1% = la(ln%, fade%).x1: y1% = la(ln%, fade%).y1
         x2% = la(ln%, fade%).x2: y2% = la(ln%, fade%).y2
         la(ln%, fade%).x1 = la(ln%, fade% - 1).x1
         la(ln%, fade%).y1 = la(ln%, fade% - 1).y1
         la(ln%, fade%).x2 = la(ln%, fade% - 1).x2
         la(ln%, fade%).y2 = la(ln%, fade% - 1).y2
         IF (ln% AND 1) = 0 THEN
            LINE (x1%, y1%)-(x2%, y2%), C(fade% \ 2)
         ELSE
            LINE (x1%, y1%)-(x2%, y2%), K(fade% \ 2)
         END IF
      NEXT
      x1% = la(ln%, 0).x1 + dx1(ln%): y1% = la(ln%, 0).y1 + dy1(ln%)
      x2% = la(ln%, 0).x2 + dx2(ln%): y2% = la(ln%, 0).y2 + dy2(ln%)
      IF x1% > 639 THEN dx1(ln%) = -(RND * 7 + 9): x1% = 639
      IF x1% < 0 THEN dx1(ln%) = RND * 7 + 9: x1% = 0
      IF x2% > 639 THEN dx2(ln%) = -(RND * 7 + 9): x2% = 639
      IF x2% < 0 THEN dx2(ln%) = RND * 7 + 9: x2% = 0
      IF y1% > 479 THEN dy1(ln%) = -(RND * 7 + 9): y1% = 479
      IF y1% < 0 THEN dy1(ln%) = RND * 7 + 9: y1% = 0
      IF y2% > 479 THEN dy2(ln%) = -(RND * 7 + 9): y2% = 479
      IF y2% < 0 THEN dy2(ln%) = RND * 7 + 9: y2% = 0
      la(ln%, 0).x1 = x1%: la(ln%, 0).y1 = y1%
      la(ln%, 0).x2 = x2%: la(ln%, 0).y2 = y2%
      IF (ln% AND 1) = 0 THEN
         LINE (x1%, y1%)-(x2%, y2%), C(0)
      ELSE
         LINE (x1%, y1%)-(x2%, y2%), K(0)
      END IF
      NEXT
   WEND
   SCREEN 0
END SUB

