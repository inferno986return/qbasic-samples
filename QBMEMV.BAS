'_|_|_|   QBMEMV.BAS
'_|_|_|   This program allows the user to view the contents of
'_|_|_|   their system's RAM, up to 1 Megabyte.
'_|_|_|   No guarantees or warrantees are given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (2/21/96)
COLOR 4, 0: CLS : f$ = CHR$(178): DIM BMem(256) AS STRING * 1
PRINT : PRINT "  QUICKBASIC": PRINT : PRINT "   MEMVIEW": COLOR 10
FOR t% = 1 TO 6: LOCATE t%, 16: PRINT STRING$(49, f$): NEXT
COLOR 14, 5: LOCATE 2, 4, 0
LOCATE , 19: PRINT " Cursor    UP  +256 "
LOCATE , 19: PRINT " Cursor  DOWN  -256 "
LOCATE , 19: PRINT " Cursor RIGHT  +4 K "
LOCATE , 19: PRINT " Cursor  LEFT  -4 K ": LOCATE 2
LOCATE , 42: PRINT " Page     UP  +64 K "
LOCATE , 42: PRINT " Page   DOWN  -64 K "
LOCATE , 42: PRINT " RETURN to EXIT     "
LOCATE , 42: PRINT " ESCAPE to EXIT     ": COLOR 10, 2
FOR t% = 7 TO 25: LOCATE t%, 2: PRINT STRING$(77, f$); : NEXT
LOCATE 8, 4: COLOR 1, 3: PRINT "       ";
FOR t% = 0 TO 15: PRINT RIGHT$("0" + HEX$(t%), 2); " "; : NEXT
COLOR 15, 1: CurSeg& = &H0
FOR t% = 0 TO 15: LOCATE 9 + t%, 10: PRINT SPACE$(49); : NEXT
DO
   FOR t% = 0 TO 15
      COLOR 1, 3: LOCATE 9 + t%, 4
      PRINT RIGHT$("0000" + HEX$(CurSeg&), 4); HEX$(t%); ":";
      DEF SEG = CurSeg&: COLOR 15, 1: PRINT " ";
      FOR byte% = 0 TO 15
         PRINT RIGHT$("0" + HEX$(PEEK(t% * 16 + byte%)) + " ", 3);
      NEXT
   NEXT: COLOR 0, 7
   FOR t% = 9 TO 24: LOCATE t%, 61: PRINT SPACE$(16); : NEXT
   COLOR 15, 1
   DO
      DEF SEG = CurSeg&
      FOR t% = 0 TO 15: FOR byte% = 0 TO 15
         BMem(t% * 16 + byte%) = CHR$(PEEK(t% * 16 + byte%))
      NEXT: NEXT: DEF SEG = &HB800
      FOR t% = 0 TO 15: LOCATE t% + 9, 11: FOR byte% = 0 TO 15
         PRINT RIGHT$("0" + HEX$(ASC(BMem(t% * 16 + byte%))) + " ", 3);
         POKE 1400 + t% * 160 + byte% * 2, ASC(BMem(t% * 16 + byte%))
      NEXT: NEXT
      Key$ = INKEY$: IF Key$ = "" THEN Key$ = " "
      K% = ASC(Key$): IF K% = 0 THEN K% = -ASC(MID$(Key$, 2))
      SELECT CASE K%
         CASE -72: CurSeg& = CurSeg& - 16: Key$ = "LOOP"
            IF CurSeg& < 0 THEN CurSeg& = 65535
         CASE -80: CurSeg& = CurSeg& + 16: Key$ = "LOOP"
            IF CurSeg& > 65535 THEN CurSeg& = 0
         CASE -75: CurSeg& = CurSeg& - 256: Key$ = "LOOP"
            IF CurSeg& < 0 THEN CurSeg& = 65535
         CASE -77: CurSeg& = CurSeg& + 256: Key$ = "LOOP"
            IF CurSeg& > 65535 THEN CurSeg& = 0
         CASE -73: CurSeg& = CurSeg& - 4096: Key$ = "LOOP"
            IF CurSeg& < 0 THEN CurSeg& = 65535
         CASE -81: CurSeg& = CurSeg& + 4096: Key$ = "LOOP"
            IF CurSeg& > 65535 THEN CurSeg& = 0
         CASE 13, 27: out$ = "OUT": Key$ = "LOOP"
      END SELECT
   LOOP WHILE Key$ <> "LOOP"
LOOP WHILE out$ <> "OUT"
COLOR 14, 0: LOCATE 2, 67: PRINT "Thank You"
LOCATE 3, 67: PRINT "for using": LOCATE 4, 67: PRINT "QuickBasic"
LOCATE 5, 67: PRINT "MemView.": LOCATE 24, 1
'_|_|_|   end   QBMEMV.BAS
