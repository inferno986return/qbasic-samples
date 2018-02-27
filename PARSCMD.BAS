'_|_|_|   PARSCMD.BAS
'_|_|_|   A program to separate the COMMAND$ into an array.
'_|_|_|   VAL(arg$(0)) holds number of arguments, if any.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (1/14/97)
DECLARE SUB ParseCMD ()
'$DYNAMIC
DIM SHARED arg$(0)
CLS : ParseCMD: : PRINT arg$(0)
FOR t% = 1 TO VAL(arg$(0)): PRINT arg$(t%): NEXT
SUB ParseCMD
   args% = 0: a$ = COMMAND$: IF a$ = "" THEN EXIT SUB
   DO
      sp% = INSTR(a$, " "): args% = args% + 1
      IF sp% <> 0 THEN a$ = LTRIM$(MID$(a$, sp%)):  ELSE a$ = ""
   LOOP WHILE a$ <> "": a$ = COMMAND$
   REDIM arg$(args% + 1): arg$(0) = STR$(args%): a% = 1
   DO: sp% = INSTR(a$, " ")
      IF sp% <> 0 THEN
         arg$(a%) = RTRIM$(LEFT$(a$, sp%))
         a$ = LTRIM$(MID$(a$, sp%)): a% = a% + 1
      ELSE arg$(a%) = a$: a$ = ""
      END IF
   LOOP WHILE a$ <> ""
END SUB
'_|_|_|   end   PARSCMD.BAS
