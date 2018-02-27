DECLARE SUB RomanNumerals (Num$)
DECLARE SUB RomanReplace (Num$, Frm$, R$)
DECLARE FUNCTION Romans$ (n%)
'_|_|_|   ROMANS.BAS
'_|_|_|   This program will convert an integer value, or a string
'_|_|_|   containing an integer value, into a Roman Numeral string.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (2/8/96)
DO
   INPUT "Enter a number => ", n$: n% = VAL(n$): v$ = STR$(n%)
   IF n% <> 0 THEN RomanNumerals v$: PRINT v$, Romans$(n%)
LOOP WHILE n% > 0
'_|_|_|   end   ROMANS.BAS

SUB RomanNumerals (Num$)
   x% = VAL(Num$): Num$ = "": IF x% < 1 THEN EXIT SUB
   DIM Ones(10) AS STRING
   Ones(0) = "": Ones(1) = "I": Ones(2) = "II"
   Ones(3) = "III": Ones(4) = "IV": Ones(5) = "V"
   Ones(6) = "VI": Ones(7) = "VII": Ones(8) = "VIII": Ones(9) = "IX"
   Tens$ = "XLC": Hund$ = "CDM"
   Num$ = Ones(x% MOD 10)
   n$ = Ones((x% \ 10) MOD 10): RomanReplace Num$, n$, Tens$
   n$ = Ones((x% \ 100) MOD 10): RomanReplace Num$, n$, Hund$
   Num$ = STRING$(x% \ 1000, "M") + Num$
END SUB

SUB RomanReplace (Num$, Frm$, R$)
   FOR t% = 1 TO LEN(Frm$)
      MID$(Frm$, t%, 1) = MID$(R$, INSTR("IVX", MID$(Frm$, t%, 1)))
   NEXT: Num$ = Frm$ + Num$
END SUB

FUNCTION Romans$ (n%)
   t$ = STR$(n%): RomanNumerals t$: Romans$ = t$
END FUNCTION

