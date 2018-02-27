'_|_|_|   BEACHBAL.BAS
'_|_|_|   This program demonstrates very simple animation.
'_|_|_|   No warrantees or guarantees are given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (11/25/96)
SCREEN 12
DIM animation(193, 8) AS LONG
'creates 8 blocks of memory for sprites, each 32x24 + 4 bytes
'for the GET/PUT definition. Note that the 193 is the first
'parameter. Unlike C, QB arrays are contiguous first to last.
FOR A% = 0 TO 7
   FOR C% = 0 TO 7
      B! = C% * .785398: E! = (1 + C%) * .785398
      FOR R% = 1 TO 8
         CIRCLE (15, 11), R%, C% + A%, B!, E!
         CIRCLE (15, 11), R%, C% + A%, B!, E!
         CIRCLE (15, 11), R%, C% + A%, B!, E!
         CIRCLE (15, 11), R%, C% + A%, B!, E!
         CIRCLE (15, 11), R%, C% + A%, B!, E!
         CIRCLE (15, 11), R%, C% + A%, B!, E!
         CIRCLE (15, 11), R%, C% + A%, B!, E!
         CIRCLE (15, 11), R%, C% + A%, B!, E!
      NEXT
   NEXT
   GET (0, 0)-(31, 23), animation(0, A%)
   PUT (0, 0), animation(0, A%)
NEXT
'This bit creates our sprites and saves them in the array
'The PUT takes advantage of the default XOR mode to erase
x% = 287: y% = 215: xd% = 2: yd% = 2
DO
   C% = (C% + 1) MOD 32
   WAIT &H3DA, 8: WAIT &H3DA, 8, 8
   PUT (x%, y%), animation(0, C% \ 4), PSET
   IF x% > 597 THEN xd% = -(RND * 2 + 1)
   IF x% < 10 THEN xd% = (RND * 2 + 1)
   IF y% > 445 THEN yd% = -(RND * 2 + 1)
   IF y% < 10 THEN yd% = (RND * 2 + 1)
   x% = x% + xd%: y% = y% + yd%
LOOP WHILE INKEY$ = ""
'This places our sprites on the screen, using PSET, which
'allows the sprite block to erase the old data by overlapping.
SCREEN 0
'_|_|_|   end   BEACHBAL.BAS
 
