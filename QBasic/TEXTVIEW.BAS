DECLARE FUNCTION viewtext% (B%, TXT$, F$, CL%)
ON ERROR GOTO OOps
'$DYNAMIC
DIM SHARED TextBlock&(1000)
TB% = 0: TextBlock&(0) = 1
COLOR 2, 0: CLS : LOCATE 3, 1: INPUT " Enter file name => ", File$
IF File$ = "" THEN END
OPEN File$ FOR BINARY AS #1
flen& = LOF(1): browsing% = -1: CurrentLine% = 1
DO
   REDIM SHARED LnNdx(15000) AS INTEGER
   REDIM SHARED LnLen(15000) AS INTEGER
   SEEK #1, TB% * 25000 + 1
   TXT$ = INPUT$(30000, 1)
   browsing% = viewtext%(TB%, TXT$, File$, CurrentLine%)
LOOP WHILE browsing%
CLOSE 1: END
OOps:
 PRINT " Disk error or bad file name ["; ERL; "]": er$ = "yipes!"
 'CLOSE 1: END
 RESUME NEXT

REM $STATIC
FUNCTION viewtext% (B%, TXT$, F$, CL%)
   Nlines% = 0: L% = 0: LnNdx(0) = 1: LnLen(0) = 0: T% = 0
   DO
      T% = INSTR(T% + 1, TXT$, CHR$(13))
      IF T% <> 0 THEN
         LnLen(Nlines%) = T% - LnNdx(Nlines%)
         Nlines% = Nlines% + 1: NL% = 1
         WHILE MID$(TXT$, T% + NL%, 1) = CHR$(10): NL% = NL% + 1: WEND
         LnNdx(Nlines%) = T% + NL%
      END IF
   IF (T% > 24999) AND (L% = 0) THEN L% = T%
   LOOP WHILE T% <> 0
   IF L% <> 0 THEN TextBlock&(B% + 1) = L%:
   COLOR 15, 4: LOCATE 1, 1, 0: border$ = STRING$(80, CHR$(178))
   DB$ = border$: MID$(DB$, 40) = " " + F$ + " ": PRINT DB$;
   L% = CL% - TextBlock&(B%)
   DO
      COLOR 15, 1: fill$ = SPACE$(80)
      FOR T% = 0 TO 23
         IF T% + L% < Nlines% THEN
            LN$ = MID$(TXT$, LnNdx(T% + L%), LnLen(T% + L%))
         ELSE LN$ = ""
         END IF
         LOCATE T% + 2, 1: PRINT LEFT$(LN$ + fill$, 80);
      NEXT: k% = 1
      WHILE (k% <> 27) AND (k% <> -1)
         k$ = "": WHILE k$ = "": k$ = INKEY$: WEND
         k% = ASC(k$): IF k% = 0 THEN k% = -ASC(MID$(k$, 2))
         SELECT CASE k%
            CASE -72: L% = L% + (L% > 0): k% = -1
            CASE -73: k% = -1
               L% = L% + 24 * (L% > 24) + L% * (L% < 24)
            CASE -80: L% = L% - ((L% + 1) < Nlines%): k% = -1
            CASE -81: k% = -1: big% = 24 * (L% + 24 < Nlines%)
               L% = L% - big%: IF big% = 0 THEN L% = Nlines% - 1
         END SELECT
      WEND
   LOOP WHILE k% <> 27
   B% = 1: viewtext% = 0
END FUNCTION

