'_|_|_|   SEETIME.BAS   PUBLIC DOMAIN   Kurt Kuzba   (11/14/1998)
'_|_|_|   Display the current time by reading the BIOS clock.
'_|_|_|   No warrantee or guarantee given or implied.
DEF SEG = &H40
CLS
COLOR 15, 1
WHILE INKEY$ <> "": WEND
WHILE INKEY$ = ""
   LOCATE 1, 1: t3% = PEEK(&H6E) AND 255
   t2& = PEEK(&H6D) AND 255: t1& = PEEK(&H6C) AND 255
   PRINT LEFT$(STR$(t1&) + "   ", 5); "1/18THS OF A SECOND "
   PRINT LEFT$(STR$(t2&) + "   ", 5); "1/256THS OF AN HOUR "
   PRINT LEFT$(STR$(t3%) + "   ", 5); "HOUR OF THE DAY     "
   t# = (t2& * 256 + t1&) / 18.21#
   t$ = "      " + STR$(t3% + 12 * (t3% > 12)) + ":"
   t$ = t$ + RIGHT$("0" + MID$(STR$((FIX(t# \ 60))), 2), 2) + ":"
   t$ = t$ + RIGHT$("0" + MID$(STR$(FIX(t# MOD 60)), 2), 2)
   bios$ = t$ + MID$("ampm", 1 - 2 * (t3% > 12), 2)
   PRINT bios$; "         "
'_|_|_|   The following routine turns 24-hour military to standard
   h% = VAL(bios$): t$ = MID$(bios$, INSTR(bios$, ":"))
   SELECT CASE h%
      CASE IS > 12: t$ = RIGHT$(STR$(h% - 12), 2) + t$
      CASE 1 TO 12: t$ = RIGHT$(STR$(h%), 2) + t$
      CASE 0: t$ = "12" + t$
   END SELECT
   PRINT "      "; t$; "         "
'_|_|_|   The following prints 24-hour military from TIME$
   t$ = TIME$: PRINT "      "; t$; "           "
'_|_|_|   The following routine turns 24-hour military to standard
   h% = VAL(t$): t$ = MID$(t$, INSTR(t$, ":"))
   SELECT CASE h%
      CASE IS > 12: t$ = RIGHT$(STR$(h% - 12), 2) + t$
      CASE 1 TO 12: t$ = RIGHT$(STR$(h%), 2) + t$
      CASE 0: t$ = "12" + t$
   END SELECT
   t$ = t$ + MID$("ampm", 1 - 2 * (h% > 12), 2)
   PRINT "      "; t$; "         "
WEND
'_|_|_|   SEETIME.BAS   end of source

