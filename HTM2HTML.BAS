ON ERROR GOTO OOPs
SHELL "dir /b *.htm > html.txt"
Ferror$ = "ok": html% = FREEFILE: OPEN "html.txt" FOR INPUT AS #html%
IF Ferror$ <> "ok" THEN PRINT "Unable to open HTML.TXT.": END
WHILE NOT EOF(html%)
   LINE INPUT #html%, htm$: h% = INSTR(UCASE$(htm$), ".H")
   ht$ = LEFT$(htm$, h%) + "html"
   PRINT "ren " + htm$ + " " + ht$
   SHELL "ren " + htm$ + " " + ht$
WEND
CLOSE html%
KILL "html.txt"
SYSTEM
OOPs: Ferror$ = STR$(ERR): RESUME NEXT

