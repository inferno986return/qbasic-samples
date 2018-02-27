'_|_|_|   FASTBOBS.BAS
'_|_|_|   This program builds on the shadebob concept, which
'_|_|_|   was demonstrated by Jean-Francois Gosselin, to create
'_|_|_|   a faster display utilizing the graphics GET and PUT,
'_|_|_|   manipulating screen data directly in an array buffer.
'_|_|_|   No guarantees or warrantees are given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (3/10/96)
DECLARE SUB draw.bob (x%, y%)
SCREEN 13: CLS  '320x200
DIM SHARED buf(1538) AS INTEGER
RANDOMIZE TIMER
DO UNTIL INKEY$ <> ""
   draw.bob INT(RND * 255), INT(RND * 152)
LOOP
SCREEN 0: : WIDTH 80, 25
'_|_|_|   end  FASTBOBS.BAS

SUB draw.bob (x%, y%)
   GET (x%, y%)-(x% + 63, y% + 47), buf
   DEF SEG = VARSEG(buf(0)): os& = VARPTR(buf(0))
   FOR t% = 4 TO 3075
      POKE t% + os&, (PEEK(t% + os&) + 1) AND 255
   NEXT
   PUT (x%, y%), buf, PSET
END SUB

