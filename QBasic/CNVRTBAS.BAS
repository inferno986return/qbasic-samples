'_|_|_|   CNVRTBAS.BAS
'_|_|_|   Demonstration of base conversion in QuickBasic.
'_|_|_|   No warrantees or guarantees are given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (6/2/96)
DECLARE FUNCTION base$ (N%, B%)
DECLARE FUNCTION numput% (prompt$, length%)
DO
   NoExit$ = ""
   num% = numput%("Enter Number to convert =>", 5): PRINT
   IF num% = 0 THEN END
   bas% = numput%("Enter a conversion base =>", 2): PRINT
   IF bas% = 0 THEN END
   PRINT base$(num%, bas%)
LOOP WHILE NoExit$ = ""
'_|_|_|   end   CNVRTBAS.BAS

FUNCTION base$ (N%, B%)
   num$ = "": dgs$ = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   IF B% < 2 THEN B% = 2
   IF B% > 36 THEN B% = 36
   WHILE N% > 0
      num$ = MID$(dgs$, (N% MOD B%) + 1, 1) + num$: N% = N% \ B%
   WEND: base$ = num$
END FUNCTION

FUNCTION numput% (prompt$, length%)
   result$ = SPACE$(length%)
   PRINT prompt$; " "; : x% = CSRLIN: y% = POS(0): csrpos% = 1
   DO
      LOCATE x%, y%, 0: PRINT result$; : LOCATE , y% + csrpos% - 1, 1
      k$ = "": WHILE k$ = "": k$ = INKEY$: WEND
      IF (k$ = CHR$(8)) AND (csrpos% > 1) THEN
         csrpos% = csrpos% - 1: MID$(result$, csrpos%, 1) = " "
      END IF
      IF (k$ >= "0") AND (k$ <= "9") THEN
         IF csrpos% <= length% THEN csrpos% = csrpos% + 1
         MID$(result$, csrpos% - 1) = k$
      END IF
   LOOP WHILE k$ <> CHR$(13)
   num& = VAL(result$): IF num& > 32767 THEN num& = 32767
   numput% = num&
END FUNCTION

