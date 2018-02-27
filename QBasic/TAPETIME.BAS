'_|_|_|   TAPETIME.BAS   PUBLIC DOMAIN   Kurt Kuzba  (11/23/1999)
'_|_|_|   A simple program to assist in figuring the optimum
'_|_|_|   arrangement of music to make the best use of time on
'_|_|_|   any recording medium.
DECLARE SUB GetKey (k$, k%)
TYPE Playtime
  min AS INTEGER: sec AS INTEGER: ttm AS INTEGER: END TYPE
DECLARE SUB EditTime (R() AS Playtime, S%)
COLOR 14, 1
CLS
REDIM Record(20) AS Playtime
REDIM Backup(20) AS Playtime
Selection% = 0: Edit% = 0
LOCATE 1, 40: COLOR 0, 2
PRINT " Use Cursors: Enter = Edit: Esc = Quit ";
LOCATE 25, 10: PRINT " S = save current values.";
LOCATE , 45: PRINT " R = restore saved values.";
DO
  TotalTime% = 0
  FOR x% = 0 TO 19
    LOCATE ((x% MOD 10) + 1) * 2 + 1, (x% \ 10) * 35 + 10
    COLOR 14, 1: IF x% = Selection% THEN COLOR 1, 7
    PRINT "Selection "; RIGHT$(STR$(x% + 1), 2);
    LOCATE , POS(0) + 5
    PRINT RIGHT$("0" + MID$(STR$(Record(x%).min), 2), 2);
    PRINT ":";
    PRINT RIGHT$("0" + MID$(STR$(Record(x%).sec), 2), 2);
    TotalTime% = TotalTime% + Record(x%).ttm
  NEXT
  LOCATE 23, 10: COLOR 15, 1: PRINT "Total time : ";
  PRINT STR$(TotalTime% \ 60); ":";
  PRINT RIGHT$("0" + LTRIM$(STR$(TotalTime% MOD 60)), 2);
  GetKey k$, k%
  IF k$ = CHR$(27) THEN EXIT DO
  SELECT CASE k%
    CASE -72: Selection% = (Selection% + 19) MOD 20
    CASE -80: Selection% = (Selection% + 1) MOD 20
    CASE -75, -77: Selection% = (Selection% + 10) MOD 20
    CASE 13: EditTime Record(), Selection%
  END SELECT
  SELECT CASE UCASE$(k$)
    CASE "S": FOR x% = 0 TO 20: Backup(x%) = Record(x%): NEXT
    CASE "R": FOR x% = 0 TO 20: Record(x%) = Backup(x%): NEXT
  END SELECT
LOOP

SUB EditTime (R() AS Playtime, S%)
  DIM tempus(3) AS INTEGER
  tempus(0) = R(S%).min \ 10
  tempus(1) = R(S%).min MOD 10
  tempus(2) = R(S%).sec \ 10
  tempus(3) = R(S%).sec MOD 10
  Inc% = 0
  DO
    LOCATE 1, 1: COLOR 15, 5
    PRINT "Selection"; S% + 1; ":: ";
    COLOR 15, 5: IF Inc% = 0 THEN COLOR 0, 3
    PRINT MID$(STR$(tempus(0)), 2);
    COLOR 15, 5: IF Inc% = 1 THEN COLOR 0, 3
    PRINT MID$(STR$(tempus(1)), 2);
    COLOR 15, 5: PRINT ":";
    COLOR 15, 5: IF Inc% = 2 THEN COLOR 0, 3
    PRINT MID$(STR$(tempus(2)), 2);
    COLOR 15, 5: IF Inc% = 3 THEN COLOR 0, 3
    PRINT MID$(STR$(tempus(3)), 2);
    GetKey k$, k%
    SELECT CASE k%
      CASE 13: EXIT DO
      CASE 27: REDIM tempus(3)
      CASE -75: Inc% = (Inc% + 3) MOD 4
      CASE -77: Inc% = (Inc% + 1) MOD 4
      CASE -72
        IF Inc% = 2 THEN
          tempus(Inc%) = (tempus(Inc%) + 1) MOD 6
        ELSE
          tempus(Inc%) = (tempus(Inc%) + 1) MOD 10
        END IF
      CASE -80:
        IF Inc% = 2 THEN
          tempus(Inc%) = (tempus(Inc%) + 5) MOD 6
        ELSE
          tempus(Inc%) = (tempus(Inc%) + 9) MOD 10
        END IF
    END SELECT
  LOOP
  R(S%).min = tempus(0) * 10 + tempus(1)
  R(S%).sec = tempus(2) * 10 + tempus(3)
  R(S%).ttm = R(S%).min * 60 + R(S%).sec
  LOCATE 1, 1: COLOR , 1: PRINT SPACE$(39);
END SUB

SUB GetKey (k$, k%)
  DO: k$ = INKEY$: LOOP WHILE k$ = ""
  k% = ASC(k$): IF k% = 0 THEN k% = -ASC(MID$(k$, 2))
END SUB

