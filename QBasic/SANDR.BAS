'_|_|_|   SANDR.BAS
'_|_|_|   A simple search and replace for QBasic strings.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (12/24/96)
DECLARE SUB SandR (text$, OldT$, NewT$)
PRINT CHR$(13); "Begin Search and Replace test."
text$ = "Ellie's groovy boot reserves flavors"
PRINT text$
OldWord$ = "boot": NewWord$ = "boy": rl% = LEN(OldWord$)
SandR text$, "ellie's", "Every"
PRINT text$
SandR text$, "groovy", "Good"
PRINT text$
SandR text$, "boot", "Boy"
PRINT text$
SandR text$, "reserves", "Deserves"
PRINT text$
SandR text$, "flavors", "Favor"
PRINT text$
'_|_|_|   end   SANDR.BAS

SUB SandR (text$, OldT$, NewT$)
   rl% = LEN(OldT$): found% = INSTR(UCASE$(text$), UCASE$(OldT$))
   IF found% THEN
      part1$ = "": IF found% > 1 THEN part1$ = LEFT$(text$, found% - 1)
      part2$ = MID$(text$, found% + rl%)
      text$ = part1$ + NewT$ + part2$
   END IF
END SUB

