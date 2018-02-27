'_|_|_|   SCROLBOX.BAS
'_|_|_|   This program demonstrates the use of a configurable scroll
'_|_|_|   box with hi-lite marker and line number display.
'_|_|_|   It will read and display itself when it is run.
'_|_|_|   Released to the   PUBLIC DOMAIN   by Kurt Kuzba.
DECLARE FUNCTION ScrollBox% (lim%, high%, wide%, LIN%, COL%)
'$DYNAMIC$
TYPE textline
  l AS STRING * 80
END TYPE
DIM SHARED txt(800) AS textline, Hg AS INTEGER, Wd AS INTEGER
Hg = 23   'max value = 23 : min value = 1
Wd = 76   'max value = 76 : min value = 10
OPEN "scrolbox.bas" FOR INPUT AS #1
t% = 0
WHILE ((t% < 800) AND (NOT EOF(1)))
  LINE INPUT #1, txt(t%).l
  t% = t% + 1
WEND
CLOSE 1
selected% = ScrollBox%(t%, Hg, Wd, 1, 1)
COLOR 2, 0: CLS : PRINT "You hit ENTER on array item"; selected%
END
'_|_|_|   end   SCROLBOX.BAS

REM $STATIC
FUNCTION ScrollBox% (lim%, high%, wide%, LIN%, COL%)
  IF high% > 23 THEN high% = 23
  IF high% < 1 THEN high% = 1
  IF wide% > 76 THEN wide% = 76
  IF wide% < 10 THEN wide% = 10
  COLOR 7, 0: CLS : sel% = 0: top% = 0: ex% = lim%
  COLOR 9, 1: LOCATE 1, 1, 0: s$ = STRING$(wide%, " ")
  FOR t% = 0 TO high% + 1
    LOCATE LIN% + t%, COL%
    PRINT STRING$(wide% + 4, CHR$(177));
  NEXT
  LIN% = LIN% + 1: COL% = COL% + 2
  WHILE lim% = ex%
    COLOR 9, 1: LOCATE LIN% - 1, COL%
    PRINT MID$(STR$(sel%), 2) + STRING$(3, CHR$(177));
    FOR t% = 0 TO (high% - 1)
      LOCATE LIN% + t%, COL%: COLOR 1, 3
      IF t% + top% = sel% THEN COLOR 0, 7
      PRINT LEFT$(txt(top% + t%).l + s$, wide%);
    NEXT: p% = 0: p$ = " HPIQGO" + CHR$(13)
    WHILE p% < 2: p% = INSTR(p$, RIGHT$(INKEY$, 1)): WEND
    SELECT CASE p%
      CASE IS = 2   'CURSOR UP
        IF sel% > 0 THEN sel% = sel% - 1
        IF sel% < top% THEN top% = sel%
      CASE IS = 3   'CURSOR DOWN
        IF sel% < (lim% - 1) THEN sel% = sel% + 1
        IF sel% > (top% + (high% - 2)) THEN top% = sel% - high% + 1
      CASE IS = 4   'PAGE UP
        top% = top% - high%: sel% = sel% - high%
        IF top% < 0 THEN top% = 0
        IF sel% < 0 THEN sel% = 0
      CASE IS = 5   'PAGE DOWN
        top% = top% + high%: sel% = sel% + high%
        IF top% >= lim% - high% THEN top% = lim% - high%
        IF top% < 0 THEN top% = 0
        IF sel% >= lim% THEN sel% = lim% - 1
      CASE IS = 6   'HOME
        top% = 0: sel% = 0
      CASE IS = 7   'END
        sel% = lim% - 1: top% = sel% - high% + 1
        IF top% < 0 THEN top% = 0
      CASE IS = 8   'ENTER
        lim% = sel%
    END SELECT
  WEND
  ScrollBox% = sel%
END FUNCTION

