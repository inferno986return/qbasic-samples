DECLARE FUNCTION ToBin$ (n&, digits%)
DECLARE FUNCTION BinTo& (n$)
'_|_|_|   BIN-LONG.BAS
'_|_|_|   This program provides conversions to binary strings from
'_|_|_|   long integers and from long integers to binary strings.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (2/9/96)
INPUT "Enter a string length for your binary strings => ", L%
IF (L% > 16) OR (L% < 1) THEN END
DO
   INPUT "Enter a number to convert => ", I$
   IF VAL(I$) <> 0 THEN
      num$ = ToBin$(VAL(I$), L%)
      PRINT num$, BinTo&(num$)
   END IF
LOOP WHILE VAL(I$) <> 0
'_|_|_|   end   BIN-LONG.BAS

FUNCTION BinTo& (n$)
   RESULT& = 0: L% = LEN(n$)
   FOR T% = 1 TO L%
      V% = INSTR("01", MID$(n$, T%, 1))
      IF V% > 0 THEN RESULT& = RESULT& * 2 + V% - 1
   NEXT: BinTo& = RESULT&
END FUNCTION

FUNCTION ToBin$ (n&, digits%)
   RESULT$ = "": TEMP& = n&: DIM B$(2): B$(0) = "0": B$(1) = "1"
   WHILE n& > 0: RESULT$ = B$(n& AND 1) + RESULT$: n& = n& \ 2: WEND
   ToBin$ = RIGHT$("0000000000000000" + RESULT$, digits%)
END FUNCTION

