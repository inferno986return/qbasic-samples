'============================================================================
'     To: All                           Number: 215      Refer: 0023  
'   From: Earl Montgomery           Conference: FidoQBasic   (139)
'   Date: 02-17-96 19:37              BBS Name: Exec-PC
'Subject: SPACEEYE.BAS                  BBS ID: EXECPC
'----------------------------------------------------------------------------
'Here is some old GWBASIC code I converted to QB. I have no idea how
 'the math works. Posted for your pleasure or study.
ON ERROR GOTO endofprogram
T = 1
ST = .0174532925#
SCREEN 12
X = 150: Y = 110
FOR J = 1 TO 9
READ R, HC
COLOR HC: GOSUB math
IF T = 2 THEN ST = .125
IF T = 4 THEN ST = .448
IF T = 5 THEN ST = .7853
IF T = 6 THEN ST = 1.256
NEXT J
DATA 90,2,70,3,60,6,50,7,40,5,30,1,20,7,10,9,5,3
math:
FOR CT = 0 TO 6.28318531# STEP ST
G = G + 1: IF G > 50 THEN G = 0
CX = R * COS(CT): CY = 6 / 7 * R * SIN(CT)
LINE (X + R, Y)-(X + CX, Y - CY)
FOR delay = 0 TO 2000: NEXT
X1 = RND(320): Y1 = RND(192): PSET (X1, Y1)
NEXT CT
T = T + 1
IF T > 5 THEN T = 0
IF T = 0 THEN ST = .0174532925#
RETURN
endofprogram:
RESUME nap
nap:
SLEEP (100)
'--- Maximus 2.02
' * Origin: Verbose Ink * 214-437-0914 * USR V34+FAX (1:124/5125)
                                                                         

