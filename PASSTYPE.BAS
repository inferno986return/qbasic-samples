'_|_|_|   PASSTYPE.BAS
'_|_|_|   This program illustrates passing an array of a defined
'_|_|_|   type to a sub and editing the fields therein.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (7/9/97)
TYPE stringpack
   nam AS STRING * 16
   add AS STRING * 16
END TYPE
DECLARE FUNCTION quit$ ()
DECLARE SUB clrline (l%, s$)
DECLARE SUB editfields (D() AS stringpack, n%)
DIM dat(1 TO 20) AS stringpack
CLS
DO
   COLOR 14, 0
   FOR r% = 1 TO 20
      LOCATE r%, 1: PRINT "Record #"; r%, dat(r%).nam, dat(r%).add;
   NEXT
   WHILE (r% < 1) OR (r% > 20)
      COLOR 10: clrline 22, "Record to Edit => ": INPUT "", r%
      IF (r% < 1) OR (r% > 20) THEN
         clrline 22, "Do you wish to exit the program (Y/N)? "
         IF quit$ = "Y" THEN EXIT DO
      END IF
   WEND
   editfields dat(), r%
LOOP WHILE r% <> 0
'_|_|_|   PASSTYPE.BAS

SUB clrline (l%, s$)
   LOCATE l%, 1: PRINT SPACE$(80); : LOCATE , 1: PRINT s$; : END SUB

SUB editfields (D() AS stringpack, n%)
   clrline 22, " Enter Name => ": INPUT "", D(n%).nam
   clrline 22, " Enter Address => ": INPUT "", D(n%).add
   clrline 22, "": END SUB

FUNCTION quit$
   DO: k$ = UCASE$(INKEY$): LOOP WHILE INSTR(" YN", k$) < 2
   quit$ = k$: END FUNCTION

