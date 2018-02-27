DECLARE SUB makeblit (mask%(), spr%())
DECLARE SUB buildsprite ()
DECLARE SUB buildmask ()
RANDOMIZE TIMER: SCREEN 13: spr% = 34
DIM SprObj(spr%) AS INTEGER
DIM SprBlit(spr%) AS INTEGER
DIM SprBack(spr%) AS INTEGER
DIM SprX AS INTEGER, SprY AS INTEGER
DIM SXD AS INTEGER, SYD AS INTEGER
SXD = 1: SYD = 1: SprY = 20: SprX = 50
buildsprite
GET (1, 0)-(8, 7), SprObj
FOR x% = 1 TO 200
   CIRCLE (RND * 319, RND * 200), RND * 200, RND * 255, BF
NEXT
LINE (17, 47)-(303, 183), 4, B
LINE (18, 48)-(302, 182), 4, B
LINE (19, 49)-(301, 181), 4, B
LINE (20, 50)-(300, 180), 4, B
GET (SprY, SprX)-(SprY + 7, SprX + 7), SprBack
GET (SprY, SprX)-(SprY + 7, SprX + 7), SprBlit
makeblit SprBlit(), SprObj()
PUT (SprY, SprX), SprBlit, PSET
WHILE INKEY$ > "": WEND
DO
   y% = SprY + SYD
   IF y% > 292 THEN y% = 292: SYD = -(RND * 2 + 1)
   IF y% < 20 THEN y% = 20: SYD = RND * 2 + 1
   x% = SprX + SXD
   IF x% > 172 THEN x% = 172: SXD = -(RND * 2 + 1)
   IF x% < 50 THEN x% = 50: SXD = RND * 2 + 1
   WAIT &H3DA, 8: WAIT &H3DA, 8, 8
   PUT (SprY, SprX), SprBack, PSET
   GET (y%, x%)-(y% + 7, x% + 7), SprBack
   GET (y%, x%)-(y% + 7, x% + 7), SprBlit
   makeblit SprBlit(), SprObj()
   PUT (y%, x%), SprBlit, PSET
   SprY = y%
   SprX = x%
LOOP WHILE INKEY$ = ""
SCREEN 0: WIDTH 80, 25: SYSTEM


SUB buildsprite
   sprite$ = ""
   sprite$ = sprite$ + "**bwwb**"
   sprite$ = sprite$ + "*bwggwb*"
   sprite$ = sprite$ + "bwg**gwb"
   sprite$ = sprite$ + "wg****gw"
   sprite$ = sprite$ + "wg****gw"
   sprite$ = sprite$ + "bwg**gwb"
   sprite$ = sprite$ + "*bwggwb*"
   sprite$ = sprite$ + "**bwwb**"
   FOR x% = 0 TO 7
      FOR y% = 1 TO 8
         SELECT CASE MID$(sprite$, x% * 8 + y%, 1)
            CASE "*": PSET (y%, x%), 255
            CASE "w": PSET (y%, x%), 15
            CASE "g": PSET (y%, x%), 7
            CASE "b": PSET (y%, x%), 8
         END SELECT
      NEXT
   NEXT
END SUB

SUB makeblit (blit%(), spr%())
   Bls& = VARSEG(blit%(2))
   Blp& = VARPTR(blit%(2))
   Sps& = VARSEG(spr%(2))
   Spp& = VARPTR(spr%(2))
   FOR x& = 0 TO 63
      DEF SEG = Sps&
      pxl% = PEEK(Spp& + x&) AND 255
      IF pxl% < 255 THEN
         DEF SEG = Bls&
         POKE Blp& + x&, pxl%
      END IF
   NEXT
END SUB

