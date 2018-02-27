'_|_|_|   MOVE_EX.BAS   PUBLIC DOMAIN   Kurt Kuzba   6/6/1999
'_|_|_|   An example of using a terrain map for the movement
'_|_|_|   of game characters in QB/QBasic.
DECLARE SUB Putplayer (x%, y%)
DECLARE SUB MapSpace (x%, y%)
DECLARE SUB Movearound (x%, y%)
SCREEN 0: WIDTH 80, 25: CLS
RANDOMIZE TIMER
DIM SHARED Map(1 TO 20, 1 TO 10) AS STRING * 1
Terrain$ = "    ~o*=."
FOR x% = 1 TO 20
  FOR y% = 1 TO 10
    SELECT CASE y%
      CASE 1, 10: Map(x%, y%) = "#"
      CASE ELSE
        Map(x%, y%) = MID$(Terrain$, ((RND * 32767) MOD 9) + 1)
    END SELECT
    IF (x% = 1) OR (x% = 20) THEN Map(x%, y%) = "#"
    MapSpace x%, y%
  NEXT
NEXT
LOCATE 12, 1: COLOR 15, 0:
PRINT "_-^ LEGEND ^-_"
COLOR 0, 2: PRINT "###"; : COLOR 15, 0: PRINT " Border"
COLOR 0, 6: PRINT "   "; : COLOR 15, 0: PRINT " Open spaces"
COLOR 9, 1: PRINT "~~~"; : COLOR 15, 0: PRINT " Water"
COLOR 7, 6: PRINT "ooo"; : COLOR 15, 0: PRINT " Rocks  ( impassable)"
COLOR 2, 6: PRINT "***"; : COLOR 15, 0: PRINT " Trees  ( impassable)"
COLOR 9, 2: PRINT "==="; : COLOR 15, 0: PRINT " Swamp"
COLOR 15, 7: PRINT "..."; : COLOR 15, 0: PRINT " Sand"
PRINT : PRINT "Use Cursor keys to move, ESC to exit program"
Movearound 1, 1: SYSTEM

SUB MapSpace (x%, y%)
    LOCATE y%, x%: C$ = Map(x%, y%)
    SELECT CASE C$
      CASE "#": COLOR 0, 2
      CASE " ": COLOR 0, 6
      CASE "~": COLOR 9, 1
      CASE "o": COLOR 7, 6
      CASE "*": COLOR 2, 6
      CASE "=": COLOR 9, 2
      CASE ".": COLOR 15, 7
    END SELECT
    PRINT C$;
END SUB

SUB Movearound (x%, y%)
  Putplayer x%, y%
  DO
    DO: k$ = INKEY$: LOOP WHILE k$ = ""
    k% = ASC(k$): IF k% = 0 THEN k% = -ASC(MID$(k$, 2))
    SELECT CASE k%
      CASE 27: EXIT SUB
      CASE -72: nx% = x%: ny% = y% + (y% > 1)
      CASE -80: nx% = x%: ny% = y% - (y% < 10)
      CASE -75: ny% = y%: nx% = x% + (x% > 1)
      CASE -77: ny% = y%: nx% = x% - (x% < 20)
    END SELECT
    IF (ny% <> y%) OR (nx% <> x%) THEN
      SELECT CASE Map(nx%, ny%)
        CASE " ", "~", "=", ".", "#"
          MapSpace x%, y%
          x% = nx%: y% = ny%
          Putplayer x%, y%
        CASE ELSE: SOUND 500, 2
      END SELECT
    ELSE SOUND 200, 1
    END IF
  LOOP
END SUB

SUB Putplayer (x%, y%)
  LOCATE y%, x%: COLOR 14, SCREEN(y%, x%, 1) \ 16: PRINT "@";
END SUB

