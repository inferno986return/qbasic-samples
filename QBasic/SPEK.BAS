'From: ANDREAS KATZIG            Conference: QUIK_BAS -Qui (298)
'Date: 04-15-97 17:37              BBS Name: YE OLDE PAWN SHOPPE ]I[
'--------------Start of graphic.bas--------------------------
DECLARE SUB other ()
DEFINT A-Z
CLS
PRINT "You can use the following Keys:   S/A  - Change Spektrum"
PRINT "                                  +/-  - Change thickness"
PRINT "                                    R  - Call Special Mode. End it with pressing; "
PRINT "                                         R two times!"
FOR a = 1 TO 8: PRINT : NEXT a
PRINT "                                     SPaCe   "
DO
x$ = INKEY$
LOOP WHILE NOT x$ = " "
SCREEN 13

s = 1
x = 1
z = 0
g = 0
spektrum = 30
abstand = 1
DO

a$ = INKEY$
IF UCASE$(a$) = "R" THEN CALL other
IF a$ = "-" THEN LET abstand = abstand - 1
IF a$ = "+" THEN LET abstand = abstand + 1
IF UCASE$(a$) = "S" THEN LET spektrum = spektrum + 1
IF UCASE$(a$) = "A" THEN LET spektrum = spektrum - 1


IF v = 0 THEN LET farbe = farbe + 1:
IF v = 1 THEN LET farbe = farbe - 1:
IF spektrum = 60 THEN LET spektrum = 30
IF spektrum = 1 THEN LET spektrum = 30
IF farbe = spektrum THEN LET v = 1           ' also Brightness
IF farbe = 0 THEN LET v = 0

IF pal = 0 THEN LET f = f + 1
IF pal = 1 THEN LET f = f - 1
IF f = 200 THEN LET pal = 1
IF f = 17 THEN LET pal = 0

IF g = 0 THEN LET z = z + 1: PALETTE f, 65536 * farbe + 256 * 0 + 0
IF g = 1 THEN LET z = z - 1: PALETTE f, 65536 * 0 + 256 * farbe + 0
IF z = 0 THEN LET g = 0
IF abstand < 2 THEN LET abstand = 1
IF abstand > 20 THEN LET abstand = 1
IF z = abstand THEN LET g = 1             'range between colors


IF v1 = 0 THEN LET x = x + 1
IF v1 = 1 THEN LET x = x - 1
IF x = 200 THEN LET v1 = 1
IF x = 1 THEN LET v1 = 0


CIRCLE (160, 100), x, f
CIRCLE (159, 100), x, f
CIRCLE (160, 101), x, f

'LOCATE 25, 1: PRINT SPACE$(49)       ' If you remove the "'" you         
                                      ' will get another cool effect.

IF UCASE$(a$) = "R" THEN CALL other
IF a$ = "-" THEN LET abstand = abstand - 1
IF a$ = "+" THEN LET abstand = abstand + 1
IF UCASE$(a$) = "S" THEN LET spektrum = spektrum + 1
IF UCASE$(a$) = "A" THEN LET spektrum = spektrum - 1


LOOP WHILE NOT a$ = CHR$(27)

'----------------------END of Graphic.bas----------------------------

SUB other

s = 1
x = 1
z = 0
g = 0

DO
a$ = INKEY$
IF UCASE$(a$) = "R" THEN EXIT SUB
IF c1 = 0 THEN LET counter = counter + 1
IF c1 = 1 THEN LET counter = counter - 1
IF counter = 500 THEN LET c1 = 1
IF counter = 0 THEN LET c1 = 0

IF c2 = 0 THEN LET ran = ran + 1
IF c2 = 1 THEN LET ran = ran - 1
IF ran = 63 THEN LET c2 = 1
IF ran = 1 THEN LET c2 = 0

IF v = 0 THEN LET farbe = farbe + 1:
IF v = 1 THEN LET farbe = farbe - 1:
IF farbe = 30 THEN LET v = 1
IF farbe = 0 THEN LET v = 0

IF pal = 0 THEN LET f = f + 1
IF pal = 1 THEN LET f = f - 1
IF f = 200 THEN LET pal = 1
IF f = 17 THEN LET pal = 0

IF counter < 150 AND g = 0 THEN LET z = z + 1: PALETTE f, 65536 * 0 + 256 * 0 + farbe
IF counter > 150 AND g = 0 THEN LET z = z + 1: PALETTE f, 65536 * ran + 256 * 0 + farbe
IF counter > 150 AND g = 1 THEN LET z = z - 1: PALETTE f, 65536 * 0 + 256 * farbe
IF counter < 150 AND g = 1 THEN LET z = z - 1: PALETTE f, 65536 * ran + 256 * farbe
IF z = 0 THEN LET g = 0
IF z = 1 THEN LET g = 1


IF v1 = 0 THEN LET x = x + 1
IF v1 = 1 THEN LET x = x - 1
IF x = 200 THEN LET v1 = 1
IF x = 1 THEN LET v1 = 0


CIRCLE (160, 100), x, f
CIRCLE (159, 100), x, f
CIRCLE (160, 101), x, f


IF UCASE$(a$) = "R" THEN EXIT SUB
LOOP WHILE NOT a$ = CHR$(27)


END SUB

