DECLARE SUB Serpinski ()
'   TRIFRACT.BAS
'QB45_ECHO   Posted by Scott Mitchell
'Source code for a fractal of Serpinski's (sp.) Triangle:
'Look closely at the triangles...
'they go on in a repeating pattern for FOREVER!!!
DEFINT A-Z
CLS
SCREEN 12
X = 100:   Y = 100
RANDOMIZE TIMER
DO UNTIL INKEY$ > ""
       CALL Serpinski
LOOP
'===================================================================

'=================================================================
SUB Serpinski
SHARED X, Y
       OX = X: OY = Y
       Ran = INT(RND * 3 + 1)
       SELECT CASE Ran
               CASE 1
                       X = (OX + 200) \ 2
                       Y = OY \ 2
                       Col = 4
               CASE 2
                       X = OX \ 2
                       Y = (Y + 400) \ 2
                       Col = 1
               CASE 3
                       X = (OX + 400) \ 2
                       Y = (OY + 400) \ 2
                       Col = 3
       END SELECT
       PSET (X, Y), Col
END SUB

