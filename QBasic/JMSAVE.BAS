'============================================================================
'     To: Alex Wellerstein              Number: 49       Refer: 3251  
'   From: James McMurrin            Conference: FidoQBasic   (139)
'   Date: 06-14-96 08:43              BBS Name: Exec-PC
'Subject: This is AMAZING i think       BBS ID: EXECPC
'----------------------------------------------------------------------------

' > 'this is a little screensaver type thingy i made... when there are a lot
' > 'of lines, it looks real trippy... if it's real slow (especially in
' > 'QBasic v.1.1) make sure to adjust the delay variable to like 0 or
' > 'something...
'Try this one:
'WRITTEN BY JAMES MCMURRIN, 805 SOUTH HICKORY, HARRISON AR  72601
'PHONE 1-501-741-4467
'ADDITIONS AND ENHANCEMENTS ARE ENCOURAGED
US$ = COMMAND$
DIM CO(4, 1000), SET(13)
FOR L = 1 TO 5
READ SET(L)
NEXT L
DATA 0,12,100,.05,300
A = INSTR(US$, "T")
IF A > 0 THEN
 EN = INSTR(MID$(US$, A, LEN(US$) - A + 1), " ") + A - 1
 IF EN = A - 1 THEN EN = LEN(US$)
 S = VAL(MID$(US$, A + 1, LEN(US$) - A - (LEN(US$) - EN)))
 IF (S = 0) + (S = 1) THEN SET(1) = S
END IF
A = INSTR(US$, "SC")
IF A > 0 THEN
 EN = INSTR(MID$(US$, A, LEN(US$) - A + 1), " ") + A - 1
 IF EN = A - 1 THEN EN = LEN(US$)
 S = VAL(MID$(US$, A + 2, LEN(US$) - A - (LEN(US$) - EN)))
 SET(2) = S
END IF
A = INSTR(US$, "N")
IF A > 0 THEN
 EN = INSTR(MID$(US$, A, LEN(US$) - A + 1), " ") + A - 1
 IF EN = A - 1 THEN EN = LEN(US$)
 S = VAL(MID$(US$, A + 1, LEN(US$) - A - (LEN(US$) - EN)))
 IF (S >= 1) * (S <= 1000) THEN SET(3) = S
END IF
A = INSTR(US$, "SP")
IF A > 0 THEN
 EN = INSTR(MID$(US$, A, LEN(US$) - A + 1), " ") + A - 1
 IF EN = A - 1 THEN EN = LEN(US$)
 S = VAL(MID$(US$, A + 2, LEN(US$) - A - (LEN(US$) - EN)))
 SET(4) = S
END IF
A = INSTR(US$, "D")
IF A > 0 THEN
 EN = INSTR(MID$(US$, A, LEN(US$) - A + 1), " ") + A - 1
 IF EN = A - 1 THEN EN = LEN(US$)
 S = VAL(MID$(US$, A + 1, LEN(US$) - A - (LEN(US$) - EN)))
 SET(5) = S
END IF
A = INSTR(US$, "CO")
IF A > 0 THEN
  A = A + 1
  IF SET(1) = 0 THEN LA = 3 ELSE LA = 5
  FOR GC = 1 TO LA
  REM PRINT MID$(US$, A + 1, LEN(US$) - A)
  EN = INSTR(MID$(US$, A + 1, LEN(US$) - A), " ") + A - 1
  IF EN = A - 1 THEN EN = LEN(US$)
  REM PRINT MID$(US$, A + 1, LEN(US$) - A - (LEN(US$) - EN)); EN
  S = VAL(MID$(US$, A + 1, LEN(US$) - A - (LEN(US$) - EN)))
  SET(GC + 5) = S
  A = EN + 1
  NEXT GC
END IF
WHILE NT <> SET(2)
READ NT, SET(11), SET(12), SET(13)
WEND
DATA 1,320,200,3,2,640,200,1,3,720,348,1,4,640,400,1,7,320,200,15,13,320,200
DATA 255,8,640,200,15,9,640,350,15,10,640,350,3,11,640,480,1,12,640,480,15
10 CLS
L = 0
RANDOMIZE TIMER
IF (B$ <> "") + (SET(6) = 0) THEN
  SET(7) = RND: SET(8) = RND: SET(6) = RND * 10
  IF SET(1) = 1 THEN SET(9) = RND: SET(10) = RND
END IF
B$ = UCASE$(B$)
IF (B$ = "Q") + (B$ = CHR$(27)) THEN END
IF B$ = "G" THEN
        CLS
        PRINT SET(7); SET(8); SET(6);
        IF SET(1) = 1 THEN PRINT SET(9); SET(10)
        WHILE INKEY$ = ""
        WEND
END IF
IF B$ = "C" THEN
        CLS
        INPUT "CX1,CX2,DX"; SET(7), SET(8), SET(6)
        IF SET(1) = 1 THEN INPUT "CX3,CX4"; SET(9), SET(10)
        CLS
END IF
B$ = ""
CLS : KEY OFF
SCREEN SET(2)
WHILE B$ = ""
STAR = TIMER
FOR Q = 1 TO SET(3)
FOR D = 1 TO SET(5): NEXT D
IF SET(1) = 0 THEN
  D = INT(SET(11) / 2 * SIN(SET(7) * L + SET(6))) + SET(11) / 2
  C = INT(SET(12) / 2 * SIN(L)) + SET(12) / 2
  D1 = INT(SET(11) / 2 * SIN(SET(8) * L)) + SET(11) / 2
  C1 = INT(SET(12) / 2 * SIN(L + SET(6))) + SET(12) / 2
ELSE
  D = INT(SET(11) / 3 * SIN(SET(7) * L + SET(6))) + SET(11) / 2 + INT(SET(11) / 6 * SIN(SET(9) * L))
  C = INT(SET(12) / 3 * SIN(L)) + SET(12) / 2 + INT(SET(12) / 6 * SIN(SET(9) * L))'FOR NEXT LINE
  D1 = INT(SET(11) / 3 * SIN(SET(8) * L)) + SET(11) / 2 + INT(SET(11) / 6 * SIN(SET(10) * L))
  C1 = INT(SET(12) / 3 * SIN(L + SET(6))) + SET(12) / 2 + INT(SET(12) / 6 * SIN(SET(10) * (L + SET(6))))
END IF
LINE (D1, C1)-(D, C), (ABS(L) MOD SET(13)) + 1
LINE (CO(3, Q), CO(4, Q))-(CO(1, Q), CO(2, Q)), 0
CO(1, Q) = D: CO(2, Q) = C: CO(3, Q) = D1: CO(4, Q) = C1
B$ = INKEY$
IF B$ <> "" THEN EXIT FOR
L = L + SET(4)
NEXT Q
FINI = TIMER
REM PRINT (FINI - STAR) / SET(3);
WEND
GOTO 10
'--- FMail 1.0g
' * Origin: The Philosopher's Stone- Harrison, AR (1:391/3000.4)
  

