ON ERROR GOTO botched: F$ = COMMAND$: COLOR 7, 0: CLS
IF F$ = "" THEN INPUT "File to display"; F$
OPEN F$ FOR BINARY AS #1: OPEN "CONS:" FOR OUTPUT AS #2
WHILE NOT EOF(1): txt$ = INPUT$(512, #1): PRINT #2, txt$; : WEND
CLOSE #1: CLOSE #2
fini: COLOR 2, 0: FOR S% = 5 TO 35: SOUND S% * 200, .1: NEXT
   R$ = "": WHILE R$ = "": R$ = INKEY$: WEND: END
botched: COLOR 2, 0
   PRINT "file "; CHR$(34); F$; CHR$(34); " not found"
   PRINT "error"; ERR: RESUME fini

