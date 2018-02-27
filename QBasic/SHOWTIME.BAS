DECLARE FUNCTION ShowTime$ (x%, y%, f%, b%)
COLOR , 0: CLS : LOCATE 5: COLOR 14, 6
LOCATE , 10: PRINT " 1> Menu selection 1 "
LOCATE , 10: PRINT " 2> Menu selection 2 "
LOCATE , 10: PRINT " 3> Menu selection 3 "
LOCATE , 10: PRINT " 4> Menu selection 4 "
LOCATE , 10: PRINT " 5> Menu selection 5 "
LOCATE , 10: PRINT " 6> Menu selection 6 "
LOCATE , 10: PRINT " 7> Menu selection 7 "
LOCATE , 10: PRINT " 8> Menu selection 8 "
LOCATE , 10: PRINT " 9> Menu selection 9 "
LOCATE , 10: PRINT " Make Your Choice => ";
DO
  k$ = ShowTime$(1, 1, 1, 15)
LOOP WHILE INSTR(" 123456789", k$) < 2
COLOR 14, 6: PRINT k$

FUNCTION ShowTime$ (x%, y%, f%, b%)
   ox% = CSRLIN: oy% = POS(0): LOCATE x%, y%: COLOR f%, b%
   PRINT " "; TIME$; " ": LOCATE ox%, oy%: ShowTime$ = INKEY$
END FUNCTION

