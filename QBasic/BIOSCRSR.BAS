'_|_|_|   BIOSCRSR.BAS   PUBLIC DOMAIN   Kurt Kuzba   3/1/1998
'_|_|_|   A simple example of cursor control under CONS:
'_|_|_|   No warrantee or guarantee given or implied.
DECLARE SUB GetCursor (H%, V%)
DECLARE SUB SetCursor (H%, V%)
ON ERROR GOTO OOps
con% = FREEFILE: FError$ = "ok": OPEN "CONS:" FOR OUTPUT AS con%
IF FError$ <> "ok" THEN PRINT "Failure to open CONSOLE": SYSTEM
PRINT #con%, CHR$(27) + "[2J";
SetCursor 0, 12
PRINT #con%, "testing";
GetCursor H%, V%
PRINT #con%, ". this should be mid-screen, far left"
SetCursor 0, 0
DO: k$ = INKEY$: LOOP WHILE k$ = ""
PRINT #con%, " H ="; H%; "and V ="; V%
PRINT #con%, "This is where we saved the cursor position."
SetCursor H%, V%
PRINT #con%, "...   And we can return whence we came!";
CLOSE con%
DO: k$ = INKEY$: LOOP WHILE k$ = ""
SYSTEM
OOps: FError$ = STR$(ERR): RESUME NEXT
'_|_|_|   end   BIOSCRSR.BAS

SUB GetCursor (H%, V%)
   DEF SEG = &H40: H% = PEEK(&H50): V% = PEEK(&H51): END SUB

SUB SetCursor (H%, V%)
   DEF SEG = &H40: POKE &H50, H%: POKE &H51, V%: END SUB

