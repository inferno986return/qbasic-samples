'_|_|_|   A_WORM.BAS
'_|_|_|   This program creates a 'worm' that eats all the characters
'_|_|_|   on your screen. Hitting a key restores them and exits.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (5/10/96)
DECLARE SUB worm ()
TYPE bodySegments
   x AS INTEGER
   y AS INTEGER
END TYPE
worm
SUB worm
   DIM buf(4000) AS STRING * 1, body(11) AS bodySegments
   DEF SEG = &HB800: RANDOMIZE (TIMER * 1000)
   xd% = 0: yd% = 0: chx% = 0: chy% = 0: wc% = 0
   FOR t% = 0 TO 3999: buf(t%) = CHR$(PEEK(t%)): NEXT
   segs$ = CHR$(219) + CHR$(178) + CHR$(177) + CHR$(176) + "  "
   FOR t% = 0 TO 10: body(t%).x = 12: body(t%).y = 39: NEXT
   WHILE INKEY$ = ""
      IF wc% = 0 THEN COLOR RND * 14 + 1, 0: clim% = RND * 50 + 50
      wc% = (wc% + 1) MOD clim%
      FOR t% = 9 TO 0 STEP -1
         LOCATE body(t%).x, body(t%).y
         WHILE (INP(&H3DA) AND 8) = 0: WEND
         WHILE (INP(&H3DA) AND 8) <> 0: WEND
         PRINT STRING$(2, MID$(segs$, t% \ 2 + 1, 1));
         body(t% + 1).x = body(t%).x
         body(t% + 1).y = body(t%).y
      NEXT
      IF chx% = 0 THEN
         xd% = ((RND * 999) MOD 3) - 1
         xlim% = RND * 20 + 10
      END IF
      chx% = (chx% + 1) MOD xlim%
      IF chy% = 0 THEN
         yd% = 2 * (((RND * 999) MOD 3) - 1)
         ylim% = RND * 20 + 10
      END IF
      chy% = (chy% + 1) MOD ylim%
      IF (yd% = 0) AND (xd% = 0) THEN
         yd% = 2 - 4 * (INP(64) AND 1): xd% = 1 - 2 * (INP(64) AND 1)
      END IF
      IF (yd% < 0) AND (body(0).y < 3) THEN yd% = (RND * 999) AND 2
      IF (yd% > 0) AND (body(0).y > 77) THEN yd% = -((RND * 999) AND 2)
      IF (xd% < 0) AND (body(0).x = 1) THEN xd% = (RND * 999) AND 1
      IF (xd% > 0) AND (body(0).x = 25) THEN xd% = -((RND * 999) AND 1)
      body(0).y = body(0).y + yd%
      body(0).x = body(0).x + xd%
   WEND
   FOR t% = 0 TO 3999: POKE t%, ASC(buf(t%)): NEXT
END SUB
'_|_|_|   end   A_WORM.BAS
