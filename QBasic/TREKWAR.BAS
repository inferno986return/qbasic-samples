'============================================================================
'     To: All                           Number: 11       Refer: 0009  
'   From: Denis Boyles              Conference: FidoQBasic   (139)
'   Date: 01-22-97 19:24              BBS Name: Exec-PC
'Subject: TrekWar experiment    1/      BBS ID: EXECPC
'----------------------------------------------------------------------------
'
'Hi All,
'Don't ask, :p but I was experimenting with a few things here and there and
'I came up with the following program. The USS Enterprise-D battles it out
'with a Klingon battle cruiser in space! Nothing super fancy here, just two
'little sprites that I drew for the ships. The sprites `fly' in wax on wax
'off motion using a random attack schedule. The attack schedule just decides
'which ship should fire at the other. A red dashed line is the Enterprise
'firing and a green dashed line is the Klingon.
'The program goes into a loop having the ships battle it out and keeps track
'of damage and wins. When you've had enough, just press the ESC key to quit
'the program.
'Enjoy!
'  Denis Boyles
'[Cut-'N'-Save-(TREKWAR.BAS)------------------------------------------------]
'TREKWAR.BAS - Public Domain by Denis Boyles
'            ! Microsoft QBASIC v1.1
'            ? a mad programming experiment involving the USS-Enterprise-D
'              and a Klingon battle cruiser battling it out in space.
DECLARE SUB ReadSprite ()
DECLARE SUB ReadBits ()
DECLARE SUB DrawSprite (x%, y%, c%, num%)
DECLARE SUB DrawStars ()
DECLARE SUB CalcCircle ()
DEFINT A-Z
CONST NumStars = 400
CONST FlyRad = 80
DIM SHARED sprite(2, 16), bits(16), cosx(360), siny(360)
DIM spritea(324), spriteb(324)
PRINT "Please Wait..."
RANDOMIZE TIMER
ReadSprite
ReadBits
CalcCircle
SCREEN 13
ct = 0: ed = 0: kd = 0: el = 0: kl = 0
DEF SEG = &H40
DrawSprite cosx(ct) + 220, siny(ct) + 100, 7, 0
GET (cosx(ct) + 218, siny(ct) + 98)-(cosx(ct) + 238, siny(ct) + 120), spritea
DrawSprite siny(ct) + 110, cosx(ct) + 100, 2, 1
GET (siny(ct) + 108, cosx(ct) + 98)-(siny(ct) + 128, cosx(ct) + 120), spriteb
CLS
DrawStars
LOCATE 1, 1
PRINT USING "Enterprise Damage: ###% Wins: #####"; ed; kl
PRINT USING "   Klingon Damage: ###% Wins: #####"; kd; el
DO
  WAIT &H3DA, 8
  PUT (cosx(ct) + 218, siny(ct) + 98), spritea, PSET
  PUT (siny(ct) + 108, cosx(ct) + 96), spriteb, PSET
  SELECT CASE INT(RND * PEEK(&H6C))
    CASE 0
      LINE (cosx(ct) + 230, siny(ct) + 110)-(siny(ct) + 120, cosx(ct) + 110), 12, , &HFFF
      WAIT &H3DA, 8: WAIT &H3DA, 8
      LINE (cosx(ct) + 230, siny(ct) + 110)-(siny(ct) + 120, cosx(ct) + 110), 0
      kd = kd + INT(RND * 5)
      LOCATE 2, 20
      PRINT USING "###%"; kd
    CASE 1
      LINE (siny(ct) + 120, cosx(ct) + 110)-(cosx(ct) + 230, siny(ct) + 110), 10, , &HFFF
      WAIT &H3DA, 8: WAIT &H3DA, 8
      LINE (siny(ct) + 120, cosx(ct) + 110)-(cosx(ct) + 230, siny(ct) + 110), 0
      ed = ed + INT(RND * 5)
      LOCATE 1, 20
      PRINT USING "###%"; ed
  END SELECT
  IF kd >= 100 THEN
    kd = 0
    kl = kl + 1
    LOCATE 2, 20
    PRINT USING "###%"; kd
    LOCATE 1, 31
    PRINT USING "#####"; kl
    FOR r = 0 TO 32
      WAIT &H3DA, 8
      CIRCLE (siny(ct) + 120, cosx(ct) + 110), r, 14, , , .5
    NEXT
    FOR r = 32 TO 0 STEP -1
      WAIT &H3DA, 8
      CIRCLE (siny(ct) + 120, cosx(ct) + 110), r, 0, , , .5
    NEXT
  END IF
  IF ed >= 100 THEN
    ed = 0
    el = el + 1
    LOCATE 1, 20
    PRINT USING "###%"; ed
    LOCATE 2, 31
    PRINT USING "#####"; el
    FOR r = 0 TO 32
      WAIT &H3DA, 8
      CIRCLE (cosx(ct) + 230, siny(ct) + 110), r, 14, , , .5
    NEXT
    FOR r = 32 TO 0 STEP -1
      WAIT &H3DA, 8
      CIRCLE (cosx(ct) + 230, siny(ct) + 110), r, 0, , , .5
    NEXT
  END IF
  IF ct = 359 THEN
    ct = 0
  ELSE
    ct = ct + 1
  END IF
LOOP WHILE INKEY$ <> CHR$(27)
SCREEN 0
WIDTH 80
'USS Enterprise, NCC-1701-D :) (b/w bitmap image)
DATA &H0000,&H07E0,&H0FF0,&H0FF0,&H1FF8,&H1FF8,&H1FF8,&H1FF8
DATA &H07E0,&H6186,&H6186,&H6186,&H7FFE,&H7FFE,&H63C6,&H0000
'Klingon Warbird
DATA &HFE7F,&HFC3F,&HF81F,&HF81F,&HFC3F,&HFE7F,&HFE7F,&HFE7F
DATA &H3C3C,&H300C,&H0000,&H0000,&H0000,&H0660,&H3FFC,&H3FFC
'16 bits lookup table
DATA &H0001,&H0002,&H0004,&H0008,&H0010,&H0020,&H0040,&H0080
DATA &H0100,&H0200,&H0400,&H0800,&H1000,&H2000,&H4000,&H8000

SUB CalcCircle
  FOR ct = 0 TO 359
    cosx(ct) = INT(COS(ct * (3.14 / 180)) * FlyRad)
    siny(ct) = INT(SIN(ct * (3.14 / 180)) * FlyRad)
  NEXT
END SUB

SUB DrawSprite (x, y, c, num)
  FOR yct = 0 TO 15
    FOR xct = 15 TO 0 STEP -1
      IF sprite(num, yct) AND bits(xct) THEN
        PSET (x + xct, y + yct), c
      END IF
    NEXT
  NEXT
END SUB

SUB DrawStars
  DIM StarColor(3): StarColor(0) = 7: StarColor(1) = 8: StarColor(2) = 15
  FOR ct = 1 TO NumStars
    PSET (INT(RND * 320), INT(RND * 200)), StarColor(INT(RND * 3))
  NEXT
END SUB

SUB ReadBits
  FOR ct = 0 TO 15
    READ bits(ct)
  NEXT
END SUB

SUB ReadSprite
  FOR num = 0 TO 1
    FOR ct = 0 TO 15
      READ sprite(num, ct)
      IF num = 1 THEN
        sprite(num, ct) = NOT sprite(num, ct)
      END IF
    NEXT
  NEXT
END SUB

