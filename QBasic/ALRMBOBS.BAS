'_|_|_|   ALRMBOBS.BAS
'_|_|_|   This program builds on the shadebob concept to create
'_|_|_|   a faster display utilizing the graphics GET and PUT,
'_|_|_|   manipulating screen data directly in an array buffer.
'_|_|_|   No guarantees or warrantees are given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (3/10/96)
DECLARE SUB draw.bob (x%, y%, i%)
RANDOMIZE TIMER
SCREEN 13: CLS
DIM SHARED buf(805) AS INTEGER
DIM bob(448) AS INTEGER
FOR t% = 0 TO 447: bob(t%) = t%: NEXT
whatbob = 0
DO UNTIL INKEY$ <> ""
   IF whatbob = 0 THEN
      FOR t% = 0 TO 447
         SOUND 1000 + t%, .03
         SWAP bob(t%), bob(RND * 447)
         SOUND 1400 - t%, .03
      NEXT
      i% = RND * 15 + 1
   END IF
   draw.bob ((bob(whatbob) \ 16) * 10), ((bob(whatbob) MOD 16) * 10), i%
   whatbob = (whatbob + 1) MOD 448
LOOP
SCREEN 0: : WIDTH 80, 25
'_|_|_|   end  ALRMBOBS.BAS

SUB draw.bob (x%, y%, i%)
   GET (x%, y%)-(x% + 39, y% + 39), buf
   DEF SEG = VARSEG(buf(0)): os& = VARPTR(buf(0))
   FOR t% = 4 TO 1603
      POKE t% + os&, (PEEK(t% + os&) + i%) AND 255
   NEXT
   PUT (x%, y%), buf, PSET
END SUB

