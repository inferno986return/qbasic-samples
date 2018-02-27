DECLARE SUB draw.bob (i%)
DECLARE SUB kolorset ()
RANDOMIZE (TIMER * 100)
SCREEN 13
XY& = 0
DO UNTIL INKEY$ <> ""
   IF XY& = 0 THEN i% = RND * 2 + 1: kolorset
   draw.bob i%: XY& = (XY& + 1) MOD 10000
LOOP
SCREEN 0: : WIDTH 80, 25

SUB draw.bob (i%)
   DIM buf(452) AS INTEGER
   x% = RND * 290: y% = RND * 170
   GET (x%, y%)-(x% + 29, y% + 29), buf
   DEF SEG = VARSEG(buf(0)): os& = VARPTR(buf(0)) + 4
   FOR t& = os& TO os& + 899: POKE t&, (PEEK(t&) + i%) AND 255: NEXT
   PUT (x%, y%), buf, PSET
   ERASE buf
END SUB

SUB kolorset
   DIM c(644) AS INTEGER
   GET (0, 0)-(319, 3), c: i% = RND * 255
   DEF SEG = VARSEG(c(0)): o& = VARPTR(c(0))
   FOR t% = 4 TO 1283: POKE o& + t%, i%: NEXT
   FOR t% = 0 TO 196 STEP 4: PUT (0, t%), c, PSET: NEXT
   ERASE c
END SUB

