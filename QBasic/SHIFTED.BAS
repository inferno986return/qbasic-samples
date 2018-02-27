'_|_|_|   SHIFTED.BAS
'_|_|_|   Reads and displays the BIOS shift status at 0040:0017
'_|_|_|   and the value in the keyboard hardware port.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (10/3/96)
DEF SEG = &H40
COLOR 2, 0
CLS
COLOR 15, 1
LOCATE 1, 1
PRINT SPACE$(47)
PRINT SPACE$(47)
PRINT "      right shift"; SPACE$(30)
PRINT "      left shift "; SPACE$(30)
PRINT "      control    "; SPACE$(30)
PRINT "      alt        "; SPACE$(30)
PRINT "      scroll lock"; SPACE$(30)
PRINT "      num lock   "; SPACE$(30)
PRINT "      caps lock  "; SPACE$(30)
PRINT "      insert     "; SPACE$(30)
PRINT SPACE$(47)
COLOR 0, 7
LOCATE 3, 25
PRINT " Test keys now. "
LOCATE , 25
PRINT " [ ESC = EXIT ] "
COLOR 15, 1
WHILE UCASE$(INKEY$) <> CHR$(27)
   LOCATE 2, 1, 0
   sh% = PEEK(&H17)
   PRINT LEFT$(STR$(sh%) + "      ", 6);
   PRINT "("; INP(96); ") Key Status   "
   IF sh% AND 1 THEN PRINT " ON ":  ELSE PRINT " OFF"
   IF sh% AND 2 THEN PRINT " ON ":  ELSE PRINT " OFF"
   IF sh% AND 4 THEN PRINT " ON ":  ELSE PRINT " OFF"
   IF sh% AND 8 THEN PRINT " ON ":  ELSE PRINT " OFF"
   IF sh% AND 16 THEN PRINT " ON ":  ELSE PRINT " OFF"
   IF sh% AND 32 THEN PRINT " ON ":  ELSE PRINT " OFF"
   IF sh% AND 64 THEN PRINT " ON ":  ELSE PRINT " OFF"
   IF sh% AND 128 THEN PRINT " ON ":  ELSE PRINT " OFF"
WEND
COLOR 2, 0
'_|_|_|   end   SHIFTED.BAS

