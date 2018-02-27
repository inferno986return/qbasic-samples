'_|_|_|   SCROLL13.BAS
'_|_|_|   This is a simple demonstration of a technique of causing
'_|_|_|   text to scroll smoothly in graphics mode 13h.
'_|_|_|   No warrantees or guarantees are given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (3/18/96)
DECLARE SUB smoothscroll (screenline%)
SCREEN 13
FOR t% = 1 TO 30
   y% = RND * 250 + 30: x% = RND * 150 + 25: c% = RND * 255
   FOR c% = 1 TO 10: CIRCLE (y%, x%), RND * 75 + 25, c%: NEXT
NEXT: SOUND 999, 3
text$ = " ... ... ...   This is a simple scrolling message. It"
text$ = text$ + " doesn't mean anything and really doesn't say"
text$ = text$ + " anything. It just scrolls across the screen "
text$ = text$ + "until somebody gets sick of it and hits a key"
text$ = text$ + ". I hope you like the way it works... ... ..."
COLOR 75: Kolor% = 0
WHILE INKEY$ = ""
   LOCATE 10, 1, 0: COLOR Kolor% + 36
   PRINT LEFT$(text$, 40);
   text$ = MID$(text$ + LEFT$(text$, 1), 2)
   smoothscroll 10
   Kolor% = (Kolor% + 1) AND 63
   IF MID$(text$, 37, 3) = "..." THEN SOUND 1111, .5
WEND
SCREEN 0: END
'_|_|_|   end   SCROLL13.BAS

SUB smoothscroll (screenline%)
   DIM buf(1278) AS INTEGER
   '_|_|_|   ((319 * 8 = 2552) / 2 = 1276) + (4 / 2 = 2) = 1278
   '_|_|_|   You need to add 4 bytes for the graphic dimension data
   sl% = (screenline% - 1) * 8
   FOR t% = 1 TO 8
   '_|_|_|   We want to move the full 8 pixels, but one at a time.
      GET (1, sl%)-(319, sl% + 7), buf
   '_|_|_|   Grab the line, minus the first pixel
      WAIT &H3DA, 8: WAIT &H3DA, 8, 8
   '_|_|_|   Wait for a retrace for smooth timing
      PUT (0, sl%), buf, PSET
   '_|_|_|   Put the line back, overwriting the first pixel
      DEF SEG = &HA000
      FOR s& = screenline% TO screenline% + 7
         POKE 320 * s& - 1, 0
   '_|_|_|   Black out the pixels duplicated in the shift to left.
   '_|_|_|   Use LONG here to avoid overflow error on lower screen.
      NEXT
   NEXT
   ERASE buf
   '_|_|_|   Might as well free up the memory when not in use
END SUB

