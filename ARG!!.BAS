'_|_|_|   ARG!!.BAS
'_|_|_|   A game based an original game posted on FIDO.
'_|_|_|   From: Andrew Jones     ...   Echo: FidoQBasic
'_|_|_|   Date: 04-14-96 14:33   ...   Subj: arg.bas
'_|_|_|   No warrantees or guarantees are given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (4/28/96)
DECLARE FUNCTION bounc% ()
TYPE dotdata: ox AS INTEGER: xd AS INTEGER: x AS INTEGER
   oy AS INTEGER: yd AS INTEGER: y AS INTEGER: END TYPE
DIM dot(21) AS dotdata, me(15) AS INTEGER, they(15) AS INTEGER
RANDOMIZE (TIMER * 1000): SCREEN 13
CIRCLE (160, 100), 2, 33: PAINT (160, 100), 33
GET (158, 98)-(162, 102), they
CIRCLE (160, 100), 2, 65: PAINT (160, 100), 65
GET (158, 98)-(162, 102), me
dot(0).x = 158: dot(0).y = 98: dot(0).xd = 0: dot(0).yd = 0
level% = 1000
DO
   CLS : b$ = STRING$(6, CHR$(177)): S$ = SPACE$(34)
   LOCATE 6, 1: P$ = STRING$(43, CHR$(177)) + S$ + b$
   P$ = P$ + " Avoid all the Blue Meanies while " + b$ + S$ + b$
   P$ = P$ + " the red line crosses the screen! " + b$ + S$ + b$
   P$ = P$ + "   Use the Cursor keys to move.   " + b$ + S$ + b$
   P$ = P$ + "  The ESCAPE key quits the game.  " + b$ + S$ + b$
   P$ = P$ + "    The P key pauses the game.    " + b$ + S$ + b$
   P$ = P$ + "   Possible scores are 0 - 100.   " + b$ + S$ + b$
   P$ = P$ + "   Press any key to begin game.   " + b$ + S$ + b$
   P$ = P$ + STRING$(37, CHR$(177))
   WHILE INKEY$ <> "": WEND: c% = 64
   WHILE INKEY$ = ""
      LOCATE 4, 1: COLOR c%: PRINT P$
      c% = (c% + 1) MOD 103: IF c% = 0 THEN c% = 64
   WEND
   CLS
   FOR t% = 1 TO 20
      DO: P% = RND * 300 + 14: LOOP WHILE (P% > 100) AND (P% < 214)
      dot(t%).x = P%: dot(t%).xd = bounc%
      DO: P% = RND * 180 + 14: LOOP WHILE (P% > 70) AND (P% < 144)
      dot(t%).yd = bounc%: dot(t%).y = P%
      PUT (dot(t%).x, dot(t%).y), they, XOR
   NEXT
   dot(0).x = 155: dot(0).y = 95: PUT (dot(0).x, dot(0).y), me, XOR
   Quit% = 0: Resets& = 1: Score& = 0: Total& = 0
   DO: K$ = "": K$ = UCASE$(INKEY$)
      IF K$ = CHR$(27) THEN Total& = Total& + Score&: EXIT DO
      IF K$ = "P" THEN
         WHILE INKEY$ <> "": WEND: WHILE INKEY$ = "": WEND
      END IF
      WHILE (INP(&H3DA) AND 8) = 0: WEND
      WHILE (INP(&H3DA) AND 8) <> 0: WEND
      FOR egg% = 1 TO 10 + (Score& \ 300)
         IF ABS(dot(egg%).x - dot(0).x) < 3 THEN
            IF ABS(dot(egg%).y - dot(0).y) < 3 THEN
               Resets& = Resets& + 1
               Total& = Total& + Score&
               Score& = 0: SOUND 80, 3
               FOR t% = 196 TO 199
                  LINE (0, t%)-(319, t%), 0
               NEXT
               SOUND 65, 5
            END IF
         END IF
         dot(egg%).ox = dot(egg%).x: dot(egg%).oy = dot(egg%).y
         dot(egg%).x = dot(egg%).x + dot(egg%).xd
         dot(egg%).y = dot(egg%).y + dot(egg%).yd
         IF dot(egg%).x < 0 THEN
            dot(egg%).x = 0: dot(egg%).xd = ABS(bounc%)
            SOUND 999, .03
         END IF
         IF dot(egg%).x > 314 THEN
            dot(egg%).x = 314: dot(egg%).xd = -(ABS(bounc%))
            SOUND 999, .03
         END IF
         IF dot(egg%).y < 0 THEN
            dot(egg%).y = 0: dot(egg%).yd = ABS(bounc%)
            SOUND 999, .03
         END IF
         IF dot(egg%).y > 190 THEN
            dot(egg%).y = 190: dot(egg%).yd = -(ABS(bounc%))
            SOUND 999, .03
         END IF
         PUT (dot(egg%).ox, dot(egg%).oy), they, XOR
         PUT (dot(egg%).x, dot(egg%).y), they, XOR
      NEXT
      x% = 0: y% = 0
      SELECT CASE INP(96)
         CASE 72: IF dot(0).y > 15 THEN y% = -2
         CASE 75: IF dot(0).x > 15 THEN x% = -2
         CASE 77: IF dot(0).x < 300 THEN x% = 2
         CASE 80: IF dot(0).y < 175 THEN y% = 2
      END SELECT
      K$ = "": K$ = UCASE$(INKEY$)
      IF K$ = CHR$(27) THEN Total& = Total& + Score&: EXIT DO
      IF K$ = "P" THEN
         WHILE INKEY$ <> "": WEND: WHILE INKEY$ = "": WEND
      END IF
      WHILE (INP(&H3DA) AND 8) = 0: WEND
      WHILE (INP(&H3DA) AND 8) <> 0: WEND
      IF (x% <> 0) OR (y% <> 0) THEN
         PUT (dot(0).x, dot(0).y), me, XOR
         dot(0).x = dot(0).x + x%: dot(0).y = dot(0).y + y%
         PUT (dot(0).x, dot(0).y), me, XOR
      END IF
      IF Score& MOD 30 = 0 THEN
         LINE (Score& \ 10, 196)-(Score& \ 10, 199), 4
      END IF
      Score& = Score& + 1
      IF Score& > 3199 THEN Total& = Total& + Score&: EXIT DO
   LOOP
   IF Score& > 3199 THEN
      SOUND 500, 2: COLOR RND * 20 + 50: SOUND 700, 5: LOCATE 11, 16
      SOUND 600, 2: PRINT "YOU WON !!": SOUND 800, 7
      WHILE INKEY$ <> "": WEND
      DO
         COLOR RND * 20 + 50: LOCATE 11, 16: PRINT "YOU WON !!"
      LOOP WHILE INKEY$ = ""
      level% = level% * .8
   ELSE level% = 1000
   END IF
   bl$ = CHR$(221): br$ = CHR$(222): COLOR RND * 50 + 40
   LOCATE 7, 15: PRINT CHR$(220); STRING$(10, CHR$(223)); CHR$(220)
   LOCATE 8, 15: PRINT bl$; "GAME OVER "; br$
   P$ = LEFT$(STR$(FIX((Total& \ Resets&) \ 32)) + "   ", 4)
   LOCATE 9, 15: PRINT bl$; "SCORE:"; P$; br$
   LOCATE 10, 15: PRINT bl$; "PLAY AGAIN"; br$
   LOCATE 11, 15: PRINT bl$; "  (Y/N)?  "; br$
   LOCATE 12, 15: PRINT CHR$(223); STRING$(10, CHR$(220)); CHR$(223)
   Que$ = ". YN" + CHR$(13)
   DO
      Query$ = UCASE$(INKEY$): DEF SEG = &HA000
      L& = (RND * 100 + 110) + (320 * FIX(RND * 48 + 48))
      IF PEEK(L&) <> 0 THEN POKE L&, RND * 20 + 75
   LOOP WHILE INSTR(Que$, Query$) < 2
   IF INSTR(Que$, Query$) > 3 THEN Quit% = -1
LOOP WHILE NOT Quit%
SCREEN 0: WIDTH 80, 25
'_|_|_|   end   ARG!!.BAS

FUNCTION bounc%
   SHARED Score&, level%
   b% = (RND MOD (2 + (Score& \ level%))) + 1
   IF (INP(64) AND 1) = 0 THEN b% = -b%
   bounc% = b%
END FUNCTION

