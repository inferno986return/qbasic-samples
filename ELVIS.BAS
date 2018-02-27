'_|_|_|   ELVIS.BAS
'_|_|_|   This program allows the input of 'larger than life'
'_|_|_|   strings with limited editing windows on the screen.
'_|_|_|   BACKSPACE, HOME, END, LEFT, RIGHT, INSERT, DELETE,
'_|_|_|   and ESCAPE are all active in the input routine.
'_|_|_|   No warrantees or guarantees are given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (3/21/96)
DECLARE SUB Elvus (prompt$, max%, winsiz%, S$, sequins%)
DECLARE FUNCTION Elvis$ (prompt$, max%, winsiz%, S$, sequins%)
PRINT
Path$ = "C:\": p$ = "Please Enter Your Path =>"
Mystr$ = Elvis$(p$, 32, 8, Path$, 0)
PRINT : PRINT UCASE$(Path$): PRINT Mystr$
Pass$ = "": p$ = "Please Enter Your Password =>"
Mystr$ = Elvis$(p$, 32, 8, Pass$, 1)
PRINT : PRINT UCASE$(Pass$): PRINT Mystr$
IF Mystr$ <> "friend" THEN PRINT "Wrong Password":  ELSE PRINT "OK"
'_|_|_|   end   ELVIS.BAS

FUNCTION Elvis$ (prompt$, max%, winsiz%, S$, sequins%)
   Elvus prompt$, max%, winsiz%, S$, sequins%
   Elvis$ = S$
END FUNCTION

SUB Elvus (prompt$, max%, winsiz%, S$, sequins%)
   S$ = LTRIM$(RTRIM$(S$)): Cursor% = LEN(S$) - (Cursor% < max%)
   F$ = "_": IF sequins% <> 0 THEN F$ = " "
   Fill$ = STRING$(max%, F$)
   S$ = LEFT$(S$ + Fill$, max%): INS% = -1: PRINT prompt$; " ";
   Ybase% = POS(0)
   WHILE done$ <> "DONE"
      Sbase% = Cursor% - winsiz% + 1: IF Sbase% < 1 THEN Sbase% = 1
      LOCATE , Ybase%, 0: Hid$ = STRING$(LEN(RTRIM$(S$)), "*")
      IF sequins% = 0 THEN
         PRINT MID$(S$ + Fill$, Sbase%, winsiz%); " ";
      ELSE
         PRINT MID$(Hid$ + Fill$, Sbase%, winsiz%); " ";
      END IF
      LOCATE , Ybase% + Cursor% - Sbase%, 1
      k$ = "": WHILE k$ = "": k$ = INKEY$: WEND
      k% = ASC(k$): IF k% = 0 THEN k% = -ASC(MID$(k$, 2))
      SELECT CASE k%
         CASE 32 TO 127
            IF INS% AND Cursor% < max% THEN
               MID$(S$, Cursor% + 1) = MID$(S$, Cursor%)
               S$ = LEFT$(S$, max%)
            END IF
            MID$(S$, Cursor%, 1) = k$
            IF Cursor% = max% THEN SOUND 999, 1
            Cursor% = Cursor% - (Cursor% < max%)
         CASE 13: IF S$ = Fill$ THEN S$ = ""
            IF INSTR(S$, F$) > 0 THEN S$ = LEFT$(S$, INSTR(S$, F$) - 1)
            EXIT SUB
         CASE 8
            IF Cursor% > 1 THEN
               Cursor% = Cursor% - 1
               MID$(S$, Cursor%) = MID$(S$, Cursor% + 1)
               MID$(S$, max%) = F$
            ELSE
               SOUND 999, .7
            END IF
         CASE 27: S$ = "": EXIT SUB
         CASE -71: Cursor% = 1
         CASE -79: Cursor% = INSTR(S$, F$)
            IF Cursor% = 0 THEN Cursor% = max%
         CASE -82: INS% = -(INS% + 1): SOUND 1500 + 800 * INS%, .5
         CASE -83
            IF Cursor% < max% THEN
               MID$(S$, Cursor%) = MID$(S$, Cursor% + 1)
               MID$(S$, max%) = F$
            ELSE
               SOUND 999, .7
            END IF
         CASE -75: Cursor% = Cursor% - 1
            lim% = INSTR(S$, F$): lim% = lim% - max% * (lim% = 0)
            IF Cursor% < 1 THEN Cursor% = lim%
         CASE -77: Cursor% = Cursor% + 1
            lim% = INSTR(S$, F$): lim% = lim% - max% * (lim% = 0)
            IF Cursor% > lim% THEN Cursor% = 1
      END SELECT
   WEND
END SUB

