COLOR 2, 0
CLS
PRINT "Enter your question please... => ";
COLOR 1, 3
PRINT STRING$(12, " "); STRING$(11, CHR$(29));
INPUT "", mth$
COLOR 2, 0
IF mth$ <> "" THEN
   mth$ = LTRIM$(mth$)
   val1 = VAL(mth$)
   mth$ = MID$(mth$, LEN(STR$(val1)))
   mth$ = LTRIM$(mth$)
   op$ = LEFT$(mth$, 1)
   val2 = VAL(MID$(mth$, 2))
   op1 = val1
   op2 = val2
   val1 = -val1 * ((op$ = "+") OR (op$ = "-"))
   val2 = val2 * (op$ = "-") - val2 * (op$ = "+")
   val2 = val2 - (op1 * op2) * (op$ = "*") - (op1 / op2) * (op$ = "/")
   PRINT op1; " "; op$; op2; " ="; val1 + val2
END IF

