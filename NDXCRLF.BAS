ON ERROR GOTO botched: F$ = COMMAND$: COLOR 7, 0: CLS
DIM CrLf(1 TO 10000) AS LONG: Hit% = 0: C$ = CHR$(13) + CHR$(10)
IF F$ = "" THEN INPUT "File to scan"; F$
OPEN F$ FOR BINARY AS #1
WHILE NOT EOF(1)
   fl& = LOC(1)
   txt$ = INPUT$(512, #1)
   ndx% = 0
   WHILE INSTR(ndx% + 1, txt$, C$) <> 0
         ndx% = INSTR(ndx% + 1, txt$, C$)
         Hit% = Hit% + 1
         CrLf(Hit%) = fl& + ndx%
   WEND
WEND
CLOSE #1
PRINT Hit%; "CR/LF pairs found."
PRINT "Their locations were stored in the array CrLf()"
PRINT "Press V to view CrLf().  Any other key = EXIT."
C$ = "": WHILE C$ = "": C$ = INKEY$: WEND
IF UCASE$(C$) = "V" AND Hit% <> 0 THEN
   FOR ndx% = 1 TO Hit%: PRINT CrLf(ndx%); : NEXT
   WHILE INKEY$ = "": WEND
END IF
fini: COLOR 2, 0: FOR S% = 5 TO 35: SOUND S% * 200, .1: NEXT
   END
botched: COLOR 2, 0
   PRINT "file "; CHR$(34); F$; CHR$(34); " not found"
   PRINT "error"; ERR: RESUME fini

