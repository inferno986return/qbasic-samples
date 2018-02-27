'_|_|_|   WRDCNT2.BAS   PUBLIC DOMAIN   Kurt Kuzba   (8/22/2010)
'_|_|_|   A simple example of counting text strings within any
'_|_|_|   text document, and tracking occurrences.
DECLARE SUB Rmsg (x%, y%, msg$, k$)
ON ERROR GOTO OOps
RANDOMIZE TIMER
DIM SHARED FError AS STRING
DO
  SCREEN 0: WIDTH 80, 25: COLOR 10, 0: CLS
  LOCATE 1, 3: PRINT "Please enter name of source file   "
  COLOR 11, 0: LOCATE , 3: INPUT "=> ", Src$
  IF Src$ = "" THEN EXIT DO
  DIM Fsrc AS INTEGER, WrdLen(1000) AS LONG
  FOR c% = 1 TO 999: WrdLen(c%) = 0: NEXT
  Fsrc = FREEFILE
  IF Fsrc = 0 THEN
    PRINT " I/O Error: No file handles available ": EXIT DO
  END IF
  FError = "": OPEN Src$ FOR INPUT AS #Fsrc
  IF FError > "" THEN
    PRINT " DISK Error: Unable to open "; Src$; " ": EXIT DO
  END IF
  CLOSE Fsrc: OPEN Src$ FOR BINARY AS #Fsrc
  LOCATE 4, 4: PRINT "count hypenated?"
  LOCATE 6, 4: PRINT "   [Y/N]> ";
  DO
   k$ = INKEY$
   IF k$ = "" THEN k$ = "!"
   k% = INSTR("YyNn", k$)
  LOOP WHILE k% = 0
  PRINT k$
  Word% = 0: Words& = 0: Pstart# = TIMER
  COLOR 11, 1
  FOR c% = 3 TO 10: LOCATE c%, 3: PRINT STRING$(22, CHR$(176)); : NEXT
  COLOR 15, 1
  LOCATE 4, 4: PRINT "                    ";
  LOCATE 5, 4: PRINT "  Words  :          ";
  LOCATE 6, 4: PRINT "  % Done :          ";
  LOCATE 7, 4: PRINT "  Min:    Sec:      ";
  LOCATE 8, 4: PRINT "                    ";
  LOCATE 9, 4: PRINT "                    ";
  Token$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  Token$ = Token$ + LCASE$(Token$) + "'@_"
  IF k% < 3 THEN Token$ = Token$ + "-"
  wrd% = 0
  DO WHILE NOT EOF(Fsrc)
    Text$ = INPUT$(64, Fsrc): T% = LEN(Text$)
    FOR c% = 1 TO T%
      c$ = MID$(Text$, c%, 1)
      IF INSTR(Token$, c$) > 0 THEN
         wrd% = wrd% + 1
         IF wrd% = 1 THEN Words& = Words& + 1
      ELSE
         IF wrd% > 0 THEN WrdLen(wrd%) = WrdLen(wrd%) + 1
         wrd% = 0
      END IF
    NEXT
    Text$ = ""
    LOCATE 5, 14: PRINT RIGHT$("     " + STR$(Words&), 8);
    LOCATE 6, 15: Lc# = LOC(Fsrc): Ln# = LOF(Fsrc)
    Lc# = (Lc# * 100#) / Ln#: PRINT USING " ###.##"; Lc#;
    LOCATE 7, 10: Pnow# = TIMER - Pstart#
    P# = INT(Pnow# \ 60): PRINT STR$(P#);
    LOCATE , 17: P# = Pnow# - 60 * P#: PRINT USING " ##.#"; P#;
    IF INKEY$ <> "" THEN
      EXIT DO
    END IF
  LOOP
  CLOSE Fsrc
  COLOR 10, 0
  FOR c% = 1 TO 20
    LOCATE 2 + c%, 25
    PRINT RIGHT$(STR$(c%), 2); " :"; WrdLen(c%)
  NEXT
  FOR c% = 1 TO 20
    LOCATE 2 + c%, 37
    PRINT RIGHT$(STR$(c% + 20), 2); " :"; WrdLen(c% + 20)
  NEXT
  FOR c% = 1 TO 20
    LOCATE 2 + c%, 49
    PRINT RIGHT$(STR$(c% + 40), 2); " :"; WrdLen(c% + 40)
  NEXT
  FOR c% = 1 TO 20
    LOCATE 2 + c%, 61
    PRINT RIGHT$(STR$(c% + 60), 2); " :"; WrdLen(c% + 60)
  NEXT
  Rmsg 8, 5, "  Hit any key...  ", k$
LOOP WHILE k$ <> CHR$(27)
LOCATE 24, 1: SYSTEM
OOps: FError = STR$(ERR): RESUME NEXT
'_|_|_|   end   WRDCNT2.BAS

SUB Rmsg (x%, y%, msg$, k$)
  DO: LOCATE x%, y%: l% = LEN(msg$): COLOR , 0
    FOR c% = 1 TO l%
      COLOR ((RND * 32767) MOD 15) + 1
      PRINT MID$(msg$, c%, 1);
    NEXT
    WAIT &H3DA, 8: WAIT &H3DA, 8, 8
    k$ = INKEY$
  LOOP WHILE k$ = ""
END SUB

