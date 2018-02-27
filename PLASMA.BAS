'============================================================================
'     To: Rob Scott                     Number: 6        Refer: 3208  
'   From: Richard Barrowclough      Conference: FidoQBasic   (139)
'   Date: 06-15-96 11:36              BBS Name: Exec-PC
'Subject: RE: Req. Plasma               BBS ID: EXECPC
'----------------------------------------------------------------------------

' >     I posted this a while ago.. but the BBS I posted it on had a few
' > problems..and I Don't know if it got out.. Does anybody have some
' > code for plasma in Basic
'I don't know if this will help, I captured from this echo quite some time
'ago, Original Autor is Ian Remmler, I don't know if he still participates in
'this echo.
'---------------
'By: IAN REMMLER
'---------------
DECLARE SUB Adjust (xa, ya, x, y, xb, yb)
DECLARE SUB SubDivide (x1, y1, x2, y2)
SCREEN 13
RANDOMIZE TIMER
GOSUB PalSet
PSET (0, 0), (RND * 255) + 1
PSET (319, 0), (RND * 255) + 1
PSET (319, 199), (RND * 255) + 1
PSET (0, 199), (RND * 255) + 1
CONST F = 2.5  'change this to vary roughness
CALL SubDivide(0, 0, 319, 199)                 '(this does full screen)
BEEP
DO: LOOP WHILE INKEY$ = ""
SCREEN 0: WIDTH 80, 25
END
PalSet:
  FOR a = 0 TO 63
    s = 0: r = 63: g = a: b = 0: GOSUB Set
    s = 64: r = 63 - a: g = 63: b = 0: GOSUB Set
    s = 128: r = 0: g = 63 - a: b = a: GOSUB Set
    s = 192: r = a: g = 0: b = 63: GOSUB Set
  NEXT
RETURN
Set:
  IF a + s = 0 THEN RETURN  'to keep the background black
  OUT &H3C8, a + s
  OUT &H3C9, r
  OUT &H3C9, g
  OUT &H3C9, b
RETURN
' -=Peace=-
'   -=Richard=-
'---      [ Digital Addiction ]
' * Origin: This tagline is SHAREWARE! Send $10 [604]581-7660 (1:153/945)
                                                                             

SUB Adjust (xa, ya, x, y, xb, yb)
  IF POINT(x, y) <> 0 THEN EXIT SUB
  d = ABS(xa - xb) + ABS(ya - yb)
  v = (POINT(xa, ya) + POINT(xb, yb)) / 2 + (RND - .5) * d * F
  IF v < 1 THEN v = 1
  IF v > 255 THEN v = 255
  PSET (x, y), v
END SUB

SUB SubDivide (x1, y1, x2, y2)
  IF (x2 - x1 < 1) AND (y2 - y1 < 1) THEN EXIT SUB
  x = (x1 + x2) / 2
  y = (y1 + y2) / 2
  CALL Adjust(x1, y1, x, y1, x2, y1)
  CALL Adjust(x2, y1, x2, y, x2, y2)
  CALL Adjust(x1, y2, x, y2, x2, y2)
  CALL Adjust(x1, y1, x1, y, x1, y2)
  IF POINT(x, y) = 0 THEN
    v = POINT(x1, y1) + POINT(x2, y1) + POINT(x2, y2)
    v = v + POINT(x1, y2) + POINT(x1, y) + POINT(x, y1)
    v = v + POINT(x2, y) + POINT(x, y2)
    v = v / 8
    PSET (x, y), v
  END IF
  CALL SubDivide(x1, y1, x, y)
  CALL SubDivide(x, y1, x2, y)
  CALL SubDivide(x, y, x2, y2)
  CALL SubDivide(x1, y, x, y2)
END SUB

