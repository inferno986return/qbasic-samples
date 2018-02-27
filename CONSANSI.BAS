''''' -=*=--=*=--=*=-  begin CONSANSI.BAS  -=*=--=*=--=*=-
DECLARE SUB ansi (a$)
ON ERROR GOTO botched
IF F$ = "" THEN INPUT "File to display"; F$
OPEN F$ FOR BINARY AS #1
OPEN "CONS:" FOR OUTPUT AS #2
WHILE NOT EOF(1)
   a$ = INPUT$(512, #1)
   PRINT #2, a$;
WEND: CLOSE #1: CLOSE 2
fini: COLOR 2, 0: FOR S% = 5 TO 35: SOUND S% * 200, .1: NEXT
   R$ = "": WHILE R$ = "": R$ = INKEY$: WEND: END
botched: COLOR 2, 0
   PRINT "file "; CHR$(34); F$; CHR$(34); " not found"
   PRINT "error"; ERR: RESUME fini
''''' -=*=--=*=--=*=-  end CONSANSI.BAS  -=*=--=*=--=*=-

