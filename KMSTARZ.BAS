'============================================================================
'     To: All                           Number: 8570     Refer: 0     
'   From: Kevin Moonlight           Conference: Quik_Bas (3)
'   Date: 08-15-96 23:46              BBS Name: The Programmer's Mark BBS
'Subject: 2d Starfld.bas [1/1]          BBS ID: progmark
'----------------------------------------------------------------------------

' 2d StarField, By Kevin Moonlight.
' FidoNet: 1:250/804                  
' Internet: Cyborg@io.to.org

DEFINT A-Z
DEFSNG B, S

CONST MaxStars = 75
CONST Delay = 300
CONST XLine = 480
CONST YLine = 640

SCREEN 12, 0, 0, 0   '*** 640x480x16col Mode - (VGA Mode)...

DIM Star1(1 TO 2), Star2(1 TO 2), Star3(1 TO 2), BlockOut(1 TO 2)
DIM Xx(MaxStars), Yy(MaxStars), Ox(MaxStars), Oy(MaxStars)

CLS

FOR C = 1 TO MaxStars
   Xx(C) = INT(RND * XLine - 1) + 1
   Yy(C) = INT(RND * YLine - 1) + 1
NEXT C

StartNow = 0

DO
   'IF StartNow = 1 THEN COLOR 0: FOR C = 1 TO MaxStars: PSET (OldY(C),OldX(C)): NEXT C
   WAIT &H3DA, 8: WAIT &H3DA, 8, 0
   FOR C = 1 TO MaxStars / 3
      COLOR 0: PSET (Oy(C), Ox(C))
      COLOR 8: PSET (Yy(C), Xx(C))
   NEXT C
  
   FOR C = MaxStars / 3 + 1 TO MaxStars / 2
      COLOR 7: PSET (Yy(C), Xx(C))
      COLOR 0: PSET (Oy(C), Ox(C))
   NEXT C
  
   FOR C = MaxStars / 2 + 1 TO MaxStars
      COLOR 0: PSET (Oy(C), Ox(C))
     COLOR 15: PSET (Yy(C), Xx(C))
   NEXT C

    FOR s = 1 TO Delay: ss = 100 / 2 * 3 / 4 * 5: NEXT s

   FOR C = 1 TO MaxStars
      Ox(C) = Xx(C)
      Oy(C) = Yy(C)
      IF C < 11 THEN Yy(C) = Yy(C) - 1
      IF C > 10 AND C < 21 THEN Yy(C) = Yy(C) - 2
      IF C > 20 THEN Yy(C) = Yy(C) - 3
      IF Yy(C) < 1 THEN Yy(C) = YLine - 1: Xx(C) = INT(RND * XLine - 1) + 1
    StartNow = 1
   NEXT C
LOOP WHILE INKEY$ = ""

'--- GEcho 1.00
' * Origin: .תש Infinitie Online ש (905)831-4596 שת. (1:250/804)

                                                                                
                               

