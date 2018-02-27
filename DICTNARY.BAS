'_|_|_|   DICTNRY.BAS   ( part 1 of 2 )
'_|_|_|   PUBLIC DOMAIN   Kurt Kuzba   (6/8/1999)
'_|_|_|   An example of text parsing and merge sorting.
'_|_|_|   Extracts an alphabetized list of unique alpha strings
'_|_|_|   from a text file and saves them to another file.
DECLARE SUB alphasort (w() AS STRING, hi%)
DECLARE SUB Rmsg (x%, y%, msg$, k$)
ON ERROR GOTO OOps: RANDOMIZE TIMER: DIM SHARED FError AS STRING
DO
  SCREEN 0: WIDTH 80, 25: COLOR 1, 7: CLS
  COLOR 15, 1: LOCATE 3, 5: PRINT SPACE$(40)
  LOCATE , 5: PRINT "  This program will take a source file  "
  LOCATE , 5: PRINT "  and produce an alphabetically sorted  "
  LOCATE , 5: PRINT "  word list from that file.  You may    "
  LOCATE , 5: PRINT "  also add words to the list by using   "
  LOCATE , 5: PRINT "  a previously developed destination    "
  LOCATE , 5: PRINT "  file as your output file.             "
  LOCATE , 5: PRINT SPACE$(40): COLOR 1, 3
  LOCATE 12, 5: PRINT " Please enter name of source file   "
  COLOR 1, 7: LOCATE , 5: INPUT "=> ", Src$: COLOR 1, 3
  IF Src$ = "" THEN EXIT DO
  LOCATE 15, 5: PRINT " Enter name for destination file    "
  COLOR 1, 7: LOCATE , 5: INPUT "=> ", Dst$
  IF Dst$ = "" THEN EXIT DO
  DIM Fsrc AS INTEGER, Fdst AS INTEGER, Ftmp AS INTEGER
  LOCATE 23, 1: COLOR 15, 4: Fsrc = FREEFILE
  IF Fsrc = 0 THEN
    PRINT " I/O Error: No file handles available ": EXIT DO
  END IF
  FError = "": OPEN Src$ FOR INPUT AS #Fsrc
  IF FError > "" THEN
    PRINT " DISK Error: Unable to open "; Src$; " ": EXIT DO
  END IF
  CLOSE Fsrc: OPEN Src$ FOR BINARY AS #Fsrc
  Word% = 0: Words% = 0: lumps% = 0
  LOCATE 18, 5: PRINT " Reading...       ";
  LOCATE 19, 5: PRINT " Sorting...       ";
  LOCATE 20, 5: PRINT " Writing...       ";
  LOCATE 21, 5: PRINT "                  ";
  Pstart# = TIMER: Limit% = 1000
  REDIM wordlist(1 TO Limit%) AS STRING
  DO WHILE (NOT EOF(Fsrc)) OR (Words% > 0)
    lumps% = lumps% + 1
    WHILE (NOT EOF(Fsrc)) AND (Words% < Limit%)
      Wrd$ = "": w% = 0
      LOCATE 18, 16: PRINT lumps%;
      DO WHILE NOT EOF(Fsrc)
        C$ = INPUT$(1, Fsrc)
        SELECT CASE C$
          CASE "A" TO "Z", "a" TO "z": Wrd$ = Wrd$ + C$: w% = 1
          CASE ELSE
            IF w% = 1 THEN
              Words% = Words% + 1: wordlist(Words%) = LCASE$(Wrd$)
              EXIT DO
            END IF
            w% = 0
        END SELECT
      LOOP
    WEND
    LOCATE 19, 16: PRINT lumps%;
    alphasort wordlist(), Words%
    LOCATE 20, 16: PRINT lumps%;
    Fdst = FREEFILE
    IF Fdst = 0 THEN
      CLOSE : PRINT " I/O Error: No file handles available "
      EXIT DO
    END IF
    FError = "": OPEN Dst$ FOR INPUT AS #Fdst
    IF FError > "" THEN
      FError = "": OPEN Dst$ FOR OUTPUT AS #Fdst
      IF FError > "" THEN
        CLOSE : PRINT " DISK Error: Unable to open "; Dst$;
        EXIT DO
      ELSE
        FOR Word% = 1 TO Words%: PRINT #Fdst, wordlist(Word%): NEXT
        CLOSE Fdst
      END IF
    ELSE
      Ftmp = FREEFILE
      IF Ftmp = 0 THEN
        CLOSE : PRINT " I/O Error: No file handles available "
        EXIT DO
      END IF
      FError = "": OPEN "temp.tmp" FOR OUTPUT AS #Ftmp
      IF FError > "" THEN
        CLOSE : PRINT " DISK Error: Unable to open temp file ";
        EXIT DO
      END IF
'_|_|_|   end   DICTNRY.BAS   ( part 1 of 2 )
'_|_|_|   DICTNRY.BAS   ( part 2 of 2 )
'_|_|_|   PUBLIC DOMAIN   Kurt Kuzba   (6/8/1999)
'_|_|_|   An example of text parsing and merge sorting.
'_|_|_|   Extracts an alphabetized list of unique alpha strings
'_|_|_|   from a text file and saves them to another file.
      Word% = 1: LastWord$ = ""
      IF NOT EOF(Fdst) THEN LINE INPUT #Fdst, C$
      WHILE (Word% < Words%) AND (NOT EOF(Fdst))
        SELECT CASE wordlist(Word%)
          CASE IS < C$: Wrd$ = wordlist(Word%): Word% = Word% + 1
            IF wordlist(Word%) = wordlist(Word% - 1) THEN Wrd$ = ""
          CASE IS = C$: Wrd$ = ""
            IF NOT EOF(Fdst) THEN LINE INPUT #Fdst, C$
          CASE IS > C$: Wrd$ = C$
            IF NOT EOF(Fdst) THEN LINE INPUT #Fdst, C$
        END SELECT
        IF (Wrd$ > "") AND (Wrd$ <> LastWord$) THEN
          PRINT #Ftmp, Wrd$: LastWord$ = Wrd$
        END IF: WEND
      IF Word% < Words% THEN
        FOR Word% = Word% TO Words%
          IF LastWord$ <> wordlist(Word%) THEN
            PRINT #Ftmp, wordlist(Word%): LastWord$ = wordlist(Word%)
          END IF: NEXT
      END IF
      WHILE NOT EOF(Fdst)
        LINE INPUT #Fdst, C$
        IF LastWord$ <> C$ THEN PRINT #Ftmp, C$: LastWord$ = C$
      WEND
      CLOSE Ftmp: CLOSE Fdst: KILL Dst$: NAME "temp.tmp" AS Dst$
    END IF
    Words% = 0
    LOCATE 21, 5: Lc# = LOC(Fsrc): Ln# = LOF(Fsrc)
    Lc# = (Lc# * 100#) / Ln#:
    PRINT USING " ###.##"; Lc#; : PRINT "% done   ";
    LOCATE 22, 5: Pnow# = TIMER - Pstart#: P# = INT(Pnow# \ 60)
    PRINT RIGHT$("   " + STR$(P#) + " min", 8);
    P# = Pnow# - 60 * P#: PRINT USING " ##.#"; P#; : PRINT " sec ";
    IF INKEY$ = CHR$(27) THEN EXIT DO
    '_|_|_|  Press escape to bail out...  Just in case... ;>
  LOOP
  CLOSE Fdst: CLOSE Fsrc
  Rmsg 23, 5, "  Hit any key...  ", k$
LOOP WHILE k$ <> CHR$(27)
SYSTEM
OOps: FError = STR$(ERR): RESUME NEXT
'_|_|_|   end   DICTNRY.BAS   ( part 2 of 2 )

SUB alphasort (w() AS STRING, hi%)
  IF hi% < 2 THEN EXIT SUB
  FOR sort% = 2 TO hi%
    median% = sort% \ 2: move% = sort% \ 2
    DO WHILE move% > 0
      SELECT CASE w(median%)
        CASE IS > w(sort%): median% = median% - move
        CASE IS < w(sort%): median% = median% + move
        CASE IS = w(sort%): EXIT DO
      END SELECT
      move% = move% \ 2
    LOOP
    WHILE (median% > 1) AND (w(median%) > w(sort%))
      median% = median% - 1: WEND
    WHILE (median% < sort%) AND (w(median%) < w(sort%))
      median% = median% + 1: WEND
    tmp$ = w(sort%)
    FOR x% = sort% TO median% + 1 STEP -1: w(x%) = w(x% - 1): NEXT
    w(median%) = tmp$
  NEXT
END SUB

SUB Rmsg (x%, y%, msg$, k$)
  DO: LOCATE x%, y%: l% = LEN(msg$): COLOR , 0
    FOR C% = 1 TO l%
      COLOR ((RND * 32767) MOD 15) + 1
      PRINT MID$(msg$, C%, 1);
    NEXT
    WAIT &H3DA, 8: WAIT &H3DA, 8, 8
    k$ = INKEY$
  LOOP WHILE k$ = ""
END SUB

