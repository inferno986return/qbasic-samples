'_|_|_|   BSV&LD.BAS
'_|_|_|   Demonstration of BSAVE and BLOAD functions.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (6/6/96)
SCREEN 13: RANDOMIZE (TIMER * 1000)
FOR t% = 1 TO 500
   LINE (RND * 319, RND * 199)-(RND * 319, RND * 199), RND * 255, BF
NEXT
DEF SEG = &HA000: BSAVE "13hscr.dat", &H0, 64000
' define segment: save "file"    , offset, bytes
CLS : PRINT "hit a key": WHILE INKEY$ = "": WEND
BLOAD "13hscr.dat", 0
' load "file", offset  (segment data is saved in file)
LOCATE 12, 15: PRINT " hit a key ": WHILE INKEY$ = "": WEND
SCREEN 0: WIDTH 80, 25
'_|_|_|   end   BSV&LD.BAS

