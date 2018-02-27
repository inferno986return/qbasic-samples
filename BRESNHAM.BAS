'_|_|_|   BRESNHAM.BAS
'_|_|_|   This program demonstrates the Bresenham Algorithms
'_|_|_|   for the drawing of lines and circles, using PSET.
'_|_|_|   Adapted from BRESNHAM.C in Bob Stout's SNIPPETS.
'_|_|_|   No warrantee or guarantee is implied or given.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba. (4/16/96)
DECLARE SUB BLine (x%, y%, x2%, y2%, c%)
DECLARE SUB BCircle (x%, y%, r%, c%)
SCREEN 13
HIGH% = 200   'The Bresenham routines will need to know the screen
WIDE% = 320   'dimensions, which are found in these SHARED variables
ndx% = 0
RANDOMIZE (TIMER * 100 + INP(64))
DIM xy(412) AS LONG
BCircle 159, 99, 65, 77
DEF SEG = &HA000
FOR t& = 0 TO 63999
   IF PEEK(t&) = 77 THEN xy(ndx%) = t&: ndx% = ndx% + 1
NEXT:
WHILE INKEY$ = ""
   BCircle 159, 99, RND * 129 + 70, RND * 255
   ndx% = (RND * 400 + 5)
   l1& = xy(ndx%)
   x1% = l1& MOD 320
   y1% = l1& \ 320
   l2& = xy(ndx% + 3)
   x2% = l2& MOD 320
   y2% = l2& \ 320
   BLine x1%, y1%, x2%, y2%, RND * 255
WEND
SOUND 999, 1
WHILE INKEY$ = "": WEND
SCREEN 0
WIDTH 80, 25
END
SUB BCircle (xc%, yc%, r%, c%)
'_|_|_|   Bresenham Circle Drawing Algorithm
'_|_|_|   Adapted from BRESNHAM.C in Bob Stout's SNIPPETS.
   SHARED WIDE%, HIGH%
   x% = 0: d% = 2 * (1 - r%): W% = 2 * WIDE% \ HIGH%
   WHILE r% >= 0
      PSET (xc% + x%, yc% + r%), c%
      PSET (xc% + x%, yc% - r%), c%
      PSET (xc% - x%, yc% + r%), c%
      PSET (xc% - x%, yc% - r%), c%
      IF (d% + r%) > 0 THEN r% = r% - 1: d% = d% - W% * r% - 1
      IF x% > d% THEN x% = x% + 1: d% = d% + 2 * x% + 1
   WEND
END SUB
SUB BLine (x%, y%, x2%, y2%, c%)
'_|_|_|   Bresenham Line Drawing Algorithm
'_|_|_|   Adapted from BRESNHAM.C in Bob Stout's SNIPPETS.
   i% = 0: steep% = 0: e% = 0
   IF (x2% - x%) > 0 THEN sx% = 1: ELSE sx% = -1
   dx% = ABS(x2% - x%)
   IF (y2% - y%) > 0 THEN sy% = 1:  ELSE sy% = -1
   dy% = ABS(y2% - y%)
   IF (dy% > dx%) THEN
      steep% = 1
      SWAP x%, y%
      SWAP dx%, dy%
      SWAP sx%, sy%
   END IF
   e% = 2 * dy% - dx%
   FOR i% = 0 TO dx% - 1
      IF steep% = 1 THEN PSET (y%, x%), c%:  ELSE PSET (x%, y%), c%
      WHILE e% >= 0
         y% = y% + sy%: e% = e% - 2 * dx%
      WEND
      x% = x% + sx%: e% = e% + 2 * dy%
   NEXT
   PSET (x2%, y2%), c%
END SUB
'_|_|_|   end   BRESNHAM.BAS
