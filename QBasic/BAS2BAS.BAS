DECLARE FUNCTION Base2Base$ (N$, O%, N%)
'_|_|_|   BAS2BAS.BAS
'_|_|_|   Converts a number from any one base to another.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (12/25/96)
DIM Number AS STRING, Oldbase AS INTEGER, Newbase AS INTEGER
A$ = COMMAND$
space1% = INSTR(A$, " ")
IF space1% > 1 THEN
   Number = STR$(VAL(LEFT$(A$, space1%)))
   space2% = INSTR(space1% + 1, A$, " ")
   IF space2% > space1% THEN
      Oldbase = VAL(MID$(A$, space1%, space2% - space1%))
      Newbase = VAL(MID$(A$, space2%))
   END IF
END IF
IF (VAL(Number) < 1) OR (Oldbase < 2) OR (Newbase < 2) THEN
   PRINT "USAGE: base2bas n[number] n[base] n[newbase]": SYSTEM
END IF
PRINT Base2Base(Number, Oldbase, Newbase)
SYSTEM
FUNCTION Base2Base$ (N$, O%, N%)
   V& = 0: N$ = UCASE$(RTRIM$(LTRIM$(N$)))
   L% = LEN(N$): R$ = "": O& = O%: N& = N%
   Tokens$ = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   FOR t% = 1 TO L%
      V& = V& * O& + INSTR(Tokens$, MID$(N$, t%, 1)) - 1: NEXT
   DO: R$ = MID$(Tokens$, V& MOD N& + 1, 1) + R$: V& = V& \ N&
   LOOP WHILE V& > 0: Base2Base$ = R$
END FUNCTION
'_|_|_|   end   BAS2BAS.BAS
