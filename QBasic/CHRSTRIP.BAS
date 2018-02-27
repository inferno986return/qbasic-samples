CR$ = CHR$(13)
PRINT CR$: COLOR 15, 1:
PRINT "    This program is designed to remove non-text   ";
COLOR 10, 0: PRINT : COLOR 15, 1
PRINT "  characters from a file for further processing.  ";
COLOR 10, 0: PRINT CR$
INPUT "File to parse    =>", source$
INPUT "File to parse to =>", destination$
PRINT : IF (source$ = "") OR (destination$ = "") THEN SYSTEM
S% = FREEFILE
OPEN source$ FOR BINARY AS #S%
D% = FREEFILE
OPEN destination$ FOR APPEND AS #D%
WHILE NOT EOF(S%)
   a$ = INPUT$(512, S%): l% = LEN(a$)
   FOR t% = 1 TO l%: D$ = MID$(a$, t%, 1)
      SELECT CASE D$
         CASE " " TO CHR$(127): PRINT #D%, D$;
         CASE CR$, LF$: PRINT #D%, D$;
      END SELECT
   NEXT
WEND
CLOSE : SOUND 999, 5: SYSTEM

