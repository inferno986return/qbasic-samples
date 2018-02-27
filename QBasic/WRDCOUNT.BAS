'_|_|_|   WRDCOUNT.BAS   PUBLIC DOMAIN   Kurt Kuzba   (6/10/1999)
'_|_|_|   A simple example of counting text strings within any
'_|_|_|   text document.
DECLARE SUB Rmsg (x%, y%, msg$, k$)
ON ERROR GOTO OOps
RANDOMIZE TIMER
DIM SHARED FError AS STRING
DO
  SCREEN 0: WIDTH 80, 25: COLOR 10, 0: CLS
  LOCATE 10, 3: PRINT "Please enter name of source file   "
  COLOR 11, 0: LOCATE , 3: INPUT "=> ", Src$
  IF Src$ = "" THEN EXIT DO
  DIM Fsrc AS INTEGER
  Fsrc = FREEFILE
  IF Fsrc = 0 THEN
    PRINT " I/O Error: No file handles available ": EXIT DO
  END IF
  FError = "": OPEN Src$ FOR INPUT AS #Fsrc
  IF FError > "" THEN
    PRINT " DISK Error: Unable to open "; Src$; " ": EXIT DO
  END IF
  CLOSE Fsrc: OPEN Src$ FOR BINARY AS #Fsrc
  Word% = 0: Words& = 0: Pstart# = TIMER
  COLOR 1, 3
  FOR c% = 3 TO 8: LOCATE c%, 3: PRINT STRING$(22, "%"); : NEXT
  COLOR 15, 1
  LOCATE 4, 5: PRINT " Words  :         ";
  LOCATE 5, 5: PRINT " % Done :         ";
  LOCATE 6, 5: PRINT " Min:    Sec:     ";
  DO WHILE NOT EOF(Fsrc)
    Text$ = INPUT$(16384, Fsrc): T% = LEN(Text$)
    FOR c% = 1 TO T%
      SELECT CASE MID$(Text$, c%, 1)
        CASE "A" TO "Z", "a" TO "z": w% = 1
        CASE ELSE: IF w% = 1 THEN Words& = Words& + 1: w% = 0
      END SELECT
    NEXT
    LOCATE 4, 14: PRINT RIGHT$("     " + STR$(Words&), 8);
    LOCATE 5, 15: Lc# = LOC(Fsrc): Ln# = LOF(Fsrc)
    Lc# = (Lc# * 100#) / Ln#: PRINT USING " ###.##"; Lc#;
    LOCATE 6, 10: Pnow# = TIMER - Pstart#
    P# = INT(Pnow# \ 60): PRINT STR$(P#);
    LOCATE , 17: P# = Pnow# - 60 * P#: PRINT USING " ##.#"; P#;
  LOOP
  CLOSE Fsrc
  Rmsg 7, 5, "  Hit any key...  ", k$
LOOP WHILE k$ <> CHR$(27)
LOCATE 24, 1: SYSTEM
OOps: FError = STR$(ERR): RESUME NEXT
'_|_|_|   end   WRDCOUNT.BAS

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

