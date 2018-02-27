'_|_|_|   CARDS.BAS
'_|_|_|   A program demonstrating creating, shuffling, and displaying
'_|_|_|   of a standard deck of 52 cards. The randomizing routine was
'_|_|_|   taken from examples posted by Bill White. ( of Miami :)
'_|_|_|   Where INTEGER types are used, the SWAP command may be used
'_|_|_|   in place of the clumsy string CHR$() method used here.
'_|_|_|   No warrantees or guarantees are given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (4/30/96)
DECLARE SUB card (x%, y%, c%)
DECLARE FUNCTION shuffle$ ()
RANDOMIZE (TIMER)
CLS : COLOR 11, 1: LOCATE 3, 21
PRINT "  RETURN to quit. Any key to reshuffle  "
DO
   deck$ = shuffle$
   FOR s% = 0 TO 3
      FOR t% = 1 TO 13
      card s% * 4 + 5, t% * 6 - 5, ASC(MID$(deck$, s% * 13 + t%))
      NEXT
   NEXT: WHILE INKEY$ <> "": WEND: i$ = ""
   WHILE i$ = "": i$ = INKEY$: WEND
LOOP WHILE i$ <> CHR$(13)
'_|_|_|   CARDS.BAS
 
SUB card (x%, y%, c%)
COLOR 1, 7: LOCATE x%, y%
PRINT CHR$(213); STRING$(4, CHR$(205)); CHR$(184): LOCATE , y%
PRINT CHR$(179); SPACE$(4); CHR$(179): LOCATE , y%
PRINT CHR$(179); SPACE$(4); CHR$(179): LOCATE , y%
PRINT CHR$(212); STRING$(4, CHR$(205)); CHR$(190): LOCATE , y%
COLOR -4 * (c% < 26): LOCATE x% + 1, y% + 2
PRINT MID$("A 2 3 4 5 6 7 8 9 10J Q K ", (c% MOD 13) * 2 + 1, 2)
LOCATE , y% + 3: PRINT CHR$(INT(c% / 13) + 3)
END SUB
 
FUNCTION shuffle$
   d$ = "": FOR t% = 1 TO 52: d$ = d$ + CHR$(t% - 1): NEXT
   FOR t% = 1 TO 500
      s% = (RND * 999) MOD 52 + 1: c% = t% MOD 52 + 1
      s$ = MID$(d$, c%, 1)
      MID$(d$, c%, 1) = MID$(d$, s%, 1)
      MID$(d$, s%, 1) = s$
   NEXT: shuffle$ = d$
END FUNCTION
 
