'_|_|_|   HOTKEY.BAS
'_|_|_|   Program demonstrating the use of ON KEY for hotkeys.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (9/14/96)
KEY 15, CHR$(0) + CHR$(16)
   '_|_|_|   Oddly enough, this is the assignment for ALT/q
   '_|_|_|   Apparently, the actual keyboard scan number is used.
   '_|_|_|   To demonstrate this, change to 0;17 and use w.
   '_|_|_|   To use ESC, use 0;1
ON KEY(15) GOSUB ExitQuery
KEY(15) ON   '_|_|_|  keys(15) to key(25) are user keys
RANDOMIZE TIMER
DEF SEG = &HB800
COLOR 10, 0: CLS
PRINT "Hit any key to begin, and q to quit"
DO: LOOP WHILE INKEY$ = ""
DO
   xy% = (xy% + 49) MOD 2000
   ch% = RND * 255: cl% = RND * 14 + 1
   POKE xy% * 2, ch%: POKE xy% * 2 + 1, cl%
LOOP
ExitQuery:
   DIM temp(160) AS STRING * 1: DEF SEG = &HB800
   '_|_|_|   create buffer and save screen line
   FOR t% = 0 TO 160: temp(t%) = CHR$(PEEK(t%)): NEXT
   LOCATE 1, 1: PRINT "   Did you wish to quit (y/n)?"; SPACE$(50);
   DO: k% = INSTR(" YN", UCASE$(INKEY$)): LOOP WHILE k% < 2
   FOR t% = 0 TO 160: POKE t%, ASC(temp(t%)): NEXT: ERASE temp
   '_|_|_|   restore screen and erase buffer
   IF k% = 2 THEN END
   RETURN
'_|_|_|   end   HOTKEY.BAS

