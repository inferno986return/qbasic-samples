'_|_|_|   PRIMES.BAS
'_|_|_|   This is a small program to compute up to the first 16,000
'_|_|_|   prime numbers and store them in an array of type LONG.
'_|_|_|   No warrantees or guarantees are implied or given.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (7/25/96)
'$DYNAMIC
INPUT "Number of primes to locate => ", MAX&
'_|_|_|   Change this to set the number of primes found
COLOR 15, 0: CLS
DIM Primes(16000) AS LONG
IF MAX& > 16000 THEN MAX& = 16000
IF MAX& < 100 THEN MAX& = 100
Primes(0) = 1: Primes(1) = 2: Primes(2) = 3: Test& = 3: Ndx& = 2
Start$ = "Start: " + TIME$: PRINT "Primes found =>";
WHILE (INKEY$ = "") AND (Ndx& < MAX&)
  Test& = Test& + 1: Limit& = SQR(Test&): Div& = 1
  P& = Test& AND 1
  WHILE (P& <> 0) AND (Primes(Div&) < Limit&)
    Div& = Div& + 1: P& = Test& MOD Primes(Div&)
  WEND
  IF P& <> 0 THEN
    Ndx& = Ndx& + 1: Primes(Ndx&) = Test&
    PRINT Ndx&; : LOCATE , 16
  END IF
WEND: Finis$ = "End  : " + TIME$: PRINT
PRINT Finis$: PRINT Start$: Ndx& = 0: COLOR 14
LOCATE 2, 30: PRINT "Use UP and DOWN arrow keys to scroll list."
LOCATE , 30: PRINT "Use LEFT and RIGHT arrows for fast scroll."
LOCATE , 30: PRINT "Hit the ESCAPE key to exit the program."
DO
  FOR pr& = 0 TO 20
    LOCATE pr& + 4, 1, 0: COLOR 9
    PRINT RIGHT$("    " + STR$(Ndx& + pr& + 1), 6); "  ";
    COLOR 10
    PRINT MID$(STR$(Primes(pr& + Ndx&)) + SPACE$(8), 2, 9);
  NEXT
  DO
    inc% = (inc% + 1) MOD 2000
    k% = INP(96): toss$ = INKEY$
    IF inc% = 0 THEN
      IF k% = 1 THEN END
      IF k% = 72 THEN Ndx& = Ndx& + (Ndx& > 0): EXIT DO
      IF k% = 80 THEN Ndx& = Ndx& - (Ndx& < (MAX& - 21)): EXIT DO
      IF k% = 75 THEN
        IF Ndx& > 20 THEN
          Ndx& = Ndx& - 21
        ELSE Ndx& = 0
        END IF: EXIT DO
      END IF
      IF k% = 77 THEN
        IF Ndx& < (MAX& - 42) THEN
          Ndx& = Ndx& + 21
        ELSE Ndx& = MAX& - 21
        END IF: EXIT DO
      END IF
    END IF
  LOOP
LOOP
'_|_|_|   end   PRIMES.BAS

