'_|_|_|   DICTNRY.BAS   (part 1 of 2)
'_|_|_|   A program to process text files into 26 alphabetically
'_|_|_|   sorted word lists without duplicate entries.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (4/11/97)
DECLARE FUNCTION Wordsize% (t$)
DECLARE FUNCTION GetChar$ ()
DECLARE SUB GetStr (X%, Y%, L%, P$, S$)
DECLARE SUB ParseFile (D$, S$)
DIM SHARED alf AS STRING * 26
Sourcefile$ = "none": Dictpath$ = "c:\"
alf = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
DO
   SCREEN 0: COLOR 10, 0: CLS : LOCATE 9, 11, 0
   COLOR 15, 4: PRINT "  Dictionary Compiler   "
   DO
      COLOR 15, 1: LOCATE , , 0
      LOCATE 10, 10: PRINT "::::::::::::::::::::::::::";
      LOCATE 11, 10: PRINT "::  S>  Source File     =>";
      LOCATE 12, 10: PRINT "::  D>  Dictionary Path =>";
      LOCATE 13, 10: PRINT "::  K>  Kill Tempfiles  ::";
      LOCATE 14, 10: PRINT "::  P>  Parse Source    ::";
      LOCATE 15, 10: PRINT "::  Q>  Quit Program    ::";
      LOCATE 16, 10: PRINT "::::::::::::::::::::::::::";
      COLOR 11, 0
      LOCATE 11, 37: PRINT Sourcefile$;
      LOCATE 12, 37: PRINT Dictpath$;
      DO: K$ = UCASE$(GetChar$): LOOP WHILE INSTR("SDKPQ", K$) < 1
      SELECT CASE K$
         CASE "S"
            GetStr 17, 1, 32, "Enter file to parse", Sourcefile$
            IF Sourcefile$ = "" THEN Sourcefile$ = "none"
         CASE "D"
            GetStr 17, 1, 32, "Enter path of dictionaries", Dictpath$
            IF Dictpath$ = "" THEN Dictpath$ = "C:\"
            S$ = RIGHT$(Dictpath$, 1)
            IF S$ <> "\" THEN Dictpath$ = Dictpath$ + "\"
         CASE "K"
            spin$ = "\|/-": spn% = 0
            COLOR 11, 0: LOCATE 13, 37: PRINT "Killing ";
            FOR t% = 1 TO 26:
               spn% = (spn% MOD 4) + 1
               LOCATE 13, 45: PRINT MID$(alf, t%, 1);
               LOCATE 13, 47: PRINT MID$(spin$, spn%, 1);
               kf% = FREEFILE
               kf$ = Dictpath$ + MID$(alf, t%, 1) + "temp.txt"
               OPEN kf$ FOR OUTPUT AS #kf%: CLOSE kf%: KILL kf$: NEXT
               COLOR 15, 5: LOCATE 13, 37: PRINT "   Done!   ";
               COLOR 11, 0: n$ = GetChar$: LOCATE 13, 37
               PRINT "              "
         CASE "P": ParseFile Dictpath$, Sourcefile$
         CASE "Q": LOCATE 15, 12: COLOR 14, 4
            PRINT "  Quit Program (Y/N)? ": COLOR 10, 0
            IF UCASE$(GetChar$) = "Y" THEN SYSTEM
      END SELECT
   LOOP
   PRINT Sourcefile$
   IF Sourcefile$ = "quit" THEN EXIT DO
LOOP
'_|_|_|   end   DICTNRY.BAS   (part 2 of 2)

FUNCTION GetChar$
   DO: K$ = INKEY$: LOOP WHILE K$ = "": GetChar$ = K$: END FUNCTION

SUB GetStr (X%, Y%, L%, P$, S$)
   S$ = "": CL% = 0: LOCATE X%, Y%: Y% = 1 + Y% + LEN(P$)
   COLOR 14, 0: PRINT P$;
   DO
      LOCATE , Y%, 0: PRINT LEFT$(S$ + SPACE$(L%), L%);
      LOCATE , Y% + CL%, 1: K$ = GetChar$
      SELECT CASE K$
         CASE IS >= " "
            IF CL% < L% THEN S$ = S$ + K$: CL% = CL% + 1
         CASE CHR$(8):
            IF CL% > 1 THEN S$ = LEFT$(S$, CL% - 1)
            IF CL% = 1 THEN S$ = ""
            IF CL% > 0 THEN CL% = CL% - 1
         CASE CHR$(13): LOCATE X%, 1: PRINT SPACE$(79); : EXIT DO
      END SELECT
   LOOP
END SUB

REM $DYNAMIC
'_|_|_|   end   DICTNRY.BAS   (part 1 of 2)
'_|_|_|   DICTNRY.BAS   (part 2 of 2)
'_|_|_|   A program to process text files into 26 alphabetically
'_|_|_|   sorted word lists without duplicate entries.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (4/11/97)
SUB ParseFile (D$, S$)
   DIM Match AS STRING * 16, Wordlist(1 TO 4000) AS STRING * 16
   CLOSE : src% = FREEFILE: OPEN S$ FOR INPUT AS #src%
   spin$ = "\\\\||||////----": spn% = 0
   FOR Initial% = 1 TO 26
      FOR aw% = 1 TO 4000: Wordlist(aw%) = " ": NEXT
      I$ = MID$(alf, Initial%, 1): Words% = 0: SEEK src%, 1
      LOCATE 14, 37, 0: PRINT "Parsing "; I$;
      WHILE NOT EOF(src%)
         spn% = (spn% MOD 16) + 1: LOCATE 14, 47, 0
         PRINT MID$(spin$, spn%, 1); : LINE INPUT #src%, L$
         L$ = UCASE$(RTRIM$(LTRIM$(L$)))
         WHILE LEN(L$)
            IF INSTR(alf, LEFT$(L$, 1)) THEN
               ln% = Wordsize%(L$): W$ = LEFT$(L$, ln%)
               L$ = MID$(L$, ln% + 1): WI$ = CHR$(ASC(W$))
               IF (ln% > 2) AND (WI$ = I$) AND (Words% < 4000) THEN
                  Match = W$
                  FOR t% = 1 TO Words%
                     IF Match = Wordlist(t%) THEN
                        Match = "=": EXIT FOR
                     END IF
                  NEXT
                  IF INSTR(Match, "=") = 0 THEN
                     Words% = Words% + 1: Wordlist(Words%) = " "
                     FOR sort% = 1 TO Words% - 1
                        IF Match < Wordlist(sort%) THEN EXIT FOR
                     NEXT
                     FOR t% = Words% TO (sort% + 1) STEP -1
                        Wordlist(t%) = Wordlist(t% - 1): NEXT
                     Wordlist(sort%) = Match
                  END IF
               END IF
            ELSE L$ = MID$(L$, 2)
            END IF
         WEND
      WEND
      IF Words% > 0 THEN
         LOCATE 14, 37, 0: PRINT "Saving  "; I$; : dst% = FREEFILE
         TempOut$ = D$ + MID$(alf, Initial%, 1) + "temp.txt"
         OPEN TempOut$ FOR OUTPUT AS #dst%
         FOR t% = 1 TO Words%
            spn% = (spn% MOD 16) + 1
            LOCATE 14, 47, 0: PRINT MID$(spin$, spn%, 1);
            PRINT #dst%, RTRIM$(Wordlist(t%)): NEXT: CLOSE dst%
      END IF
   NEXT: CLOSE src%
   COLOR 15, 5: LOCATE 14, 37: PRINT "   Done!   "; : SOUND 999, 3
   LOCATE 14, 37: COLOR 11, 0: W$ = GetChar$: PRINT "             ";
END SUB

REM $STATIC
FUNCTION Wordsize% (t$)
   L% = LEN(t$)
   FOR C% = 1 TO L%
      CH$ = MID$(t$, C%, 1)
      IF INSTR(alf, CH$) = 0 THEN EXIT FOR
   NEXT: Wordsize% = C% - 1
END FUNCTION

