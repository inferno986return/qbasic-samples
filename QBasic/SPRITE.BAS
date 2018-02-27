SCREEN 13
DIM BUF(602) AS INTEGER   '30x40 + 4 bytes for format data
BUF(0) = 320              'set block width (in PIXELS!)
BUF(1) = 30               'set block height
DEF SEG = VARSEG(BUF(0)): O& = VARPTR(BUF(0)) + 4
'set segment to directly manipulate BUF
DO
   Colour% = RND * 255
   FOR T& = O& TO O& + 1199: POKE T&, Colour%: NEXT
   'set BUF contents to another color at random
   X% = RND * 279: Y% = RND * 169
   IF (Colour% AND 1) <> 0 THEN
      PUT (X%, Y%), BUF, PSET
   ELSE
      PUT (X%, Y%), BUF, XOR
   END IF
   IF INKEY$ <> "" THEN EXIT DO
LOOP
SCREEN 0: WIDTH 80, 25: END

