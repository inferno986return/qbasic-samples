'_|_|_|   MAPMAKER.BAS   ( begin: part 1 of 2 )
'_|_|_|   PUBLIC DOMAIN   Kurt Kuzba  (6/29/1999)
'_|_|_|   A simple example of loading, editing, and saving the
'_|_|_|   data for a game terrain map in array format.
DECLARE SUB SprDisplay ()
DECLARE SUB MoveMap (Top%, Left%, X%, Y%, D%)
DECLARE SUB MoveCursor (S%, Sel%, X%, Y%, m%)
DECLARE SUB ShowCursor (S%, Sel%, X%, Y%)
DECLARE SUB SpriteDefine ()
DECLARE SUB ScrDisplay (Top%, Left%, wide%, high%)
DEFINT A-Z
SCREEN 13
CONST EDIT = 0, PICK = 1
REDIM SHARED Terrain(201, 9) AS INTEGER, Cursor(201, 1) AS INTEGER
REDIM SHARED Map(1 TO 320) AS STRING * 200
SpriteDefine
ScrTop = 1: ScrLeft = 1: CursorX = 1: CursorY = 1
Selection = 0: Segment = EDIT
CLS
PRINT : PRINT "This is a simple map editor example."
PRINT : PRINT "It will create a 64,000 byte file"
PRINT : PRINT "called MAPDAT.DAT on your hard drive."
PRINT : PRINT "The TAB key changes between the"
PRINT : PRINT "sprite selector and the map editor."
PRINT : PRINT "The SPACE key is used to place the"
PRINT : PRINT "selected sprite on the map."
PRINT : PRINT "Press any key to begin."
PRINT : PRINT "Press Ctrl/L or F3 to load map."
PRINT : PRINT "Press Ctrl/S or F2 to save map."
PRINT : PRINT "Press ESCAPE key to end program."
WHILE INKEY$ > "": WEND: WHILE INKEY$ = "": WEND
ScrDisplay ScrTop, ScrLeft, 15, 9: COLOR 99
LOCATE 24, 16: PRINT "Arrow keys move cursor";
LOCATE 25, 16: PRINT "Shift/Arrow moves map";
SprDisplay
DO: ShowCursor Segment, Selection, CursorX, CursorY
  DO: k$ = INKEY$: IF k$ = "" THEN k$ = "!"
    k% = ASC(k$): IF k% = 0 THEN k% = -ASC(MID$(k$, 2))
    DEF SEG = &H40: Shift = (PEEK(&H17) AND 3) > 0: DEF SEG
    SELECT CASE k%
      CASE 27, -68: k% = 27: EXIT DO
      CASE -72, 56
        IF Shift THEN
          IF Segment = EDIT THEN
            MoveMap ScrTop, ScrLeft, CursorX, CursorY, 8
          END IF
        ELSE
          MoveCursor Segment, Selection, CursorX, CursorY, 8
        END IF
      CASE -75, 52
        IF Shift THEN
          IF Segment = EDIT THEN
            MoveMap ScrTop, ScrLeft, CursorX, CursorY, 4
          END IF
        ELSE
          MoveCursor Segment, Selection, CursorX, CursorY, 4
        END IF
      CASE -77, 54
        IF Shift THEN
          IF Segment = EDIT THEN
            MoveMap ScrTop, ScrLeft, CursorX, CursorY, 6
          END IF
        ELSE
          MoveCursor Segment, Selection, CursorX, CursorY, 6
        END IF
      CASE -80, 50
        IF Shift THEN
          IF Segment = EDIT THEN
            MoveMap ScrTop, ScrLeft, CursorX, CursorY, 2
          END IF
        ELSE
          MoveCursor Segment, Selection, CursorX, CursorY, 2
        END IF
      CASE 9: ShowCursor Segment, Selection, CursorX, CursorY
        IF Segment = EDIT THEN Segment = PICK:  ELSE Segment = EDIT
        ShowCursor Segment, Selection, CursorX, CursorY
      CASE 56: SOUND 999, 2 'up
      CASE 54: SOUND 700, 2 'right
      CASE 52: SOUND 500, 2 'left
      CASE 50: SOUND 100, 2 'down
      CASE 13, 32
        IF Segment = EDIT THEN
          ShowCursor Segment, Selection, CursorX, CursorY
          X = CursorX + ScrLeft - 1: Y = CursorY + ScrTop - 1
          MID$(Map(X), Y) = CHR$(Selection)
          ScrDisplay ScrTop, ScrLeft, 15, 9
          ShowCursor Segment, Selection, CursorX, CursorY
        END IF
'_|_|_|   MAPMAKER.BAS   ( end: part 1 of 2 )
'_|_|_|   MAPMAKER.BAS   ( begin: part 2 of 2 )
'_|_|_|   PUBLIC DOMAIN   Kurt Kuzba  (6/29/1999)
'_|_|_|   A simple example of loading, editing, and saving the
'_|_|_|   data for a game terrain map in array format.
      CASE 12, -61:  'load
        LOCATE 10, 5: COLOR 15: PRINT " Loading... ";
        OPEN "mapdat.dat" FOR RANDOM AS #1 LEN = 200
        m$ = " ERROR - Hit a key "
        IF LOF(1) = 64000 THEN
          FOR Y = 1 TO 320: GET #1, Y, Map(Y): NEXT
          m$ = " Hit a key "
        END IF: CLOSE 1: PRINT m$
        WHILE INKEY$ > "": WEND: WHILE INKEY$ = "": WEND
        ScrDisplay ScrTop, ScrLeft, 15, 9
        ShowCursor Segment, Selection, CursorX, CursorY
      CASE 19, -60:  'save
        LOCATE 10, 5: COLOR 15: PRINT " Saving... ";
        OPEN "mapdat.dat" FOR RANDOM AS #1 LEN = 200
        FOR Y = 1 TO 320: PUT #1, Y, Map(Y): NEXT
        CLOSE 1: PRINT " Hit a key ";
        WHILE INKEY$ > "": WEND: WHILE INKEY$ = "": WEND
        ScrDisplay ScrTop, ScrLeft, 15, 9
        ShowCursor Segment, Selection, CursorX, CursorY
    END SELECT
  LOOP: IF k% = 27 THEN EXIT DO
LOOP: SCREEN 0: WIDTH 80, 25: SYSTEM
SUB MoveCursor (S, Sel, X, Y, m)
  SELECT CASE S
    CASE EDIT
      ShowCursor S, Sel, X, Y
      SELECT CASE m
        CASE 8: Y = (Y + 7) MOD 9 + 1
        CASE 6: X = X MOD 15 + 1
        CASE 4: X = (X + 13) MOD 15 + 1
        CASE 2: Y = Y MOD 9 + 1
      END SELECT
      ShowCursor S, Sel, X, Y
    CASE PICK
      ShowCursor S, Sel, X, Y
      SELECT CASE m
        CASE 8: Sel = (Sel + 9) MOD 10
        CASE 2: Sel = (Sel + 1) MOD 10
      END SELECT
      ShowCursor S, Sel, X, Y
  END SELECT: END SUB
SUB MoveMap (Top, Left, X, Y, D)
  SELECT CASE D
    CASE 8: IF Top > 1 THEN Update = -1: Top = Top - 1
    CASE 6: IF Left < 306 THEN Update = -1: Left = Left + 1
    CASE 4: IF Left > 1 THEN Update = -1: Left = Left - 1
    CASE 2: IF Top < 192 THEN Update = -1: Top = Top + 1
  END SELECT
  IF Update THEN
    ScrDisplay Top, Left, 15, 9: ShowCursor EDIT, 0, X, Y
  ELSE BEEP
  END IF: END SUB
SUB ScrDisplay (Top, Left, wide, high)
  FOR X% = 0 TO wide - 1: FOR Y% = 0 TO high - 1
    Spr = ASC(MID$(Map(X% + Left), Y% + Top))
    PUT (X% * 20, Y% * 20), Terrain(0, Spr), PSET
  NEXT: NEXT: END SUB
SUB ShowCursor (S, Sel, X, Y): SHARED ScrTop, ScrLeft
  SELECT CASE S
    CASE EDIT
      PUT ((X - 1) * 20, (Y - 1) * 20), Cursor(0, 0), XOR
      PUT ((X - 1) * 20, (Y - 1) * 20), Cursor(0, 1), XOR
    CASE PICK
      PUT (299, Sel * 20), Cursor(0, 0), XOR
      PUT (299, Sel * 20), Cursor(0, 1), XOR
  END SELECT:  COLOR 33
  LOCATE 24, 1: PRINT "X ="; X + ScrLeft - 1; "      ";
  LOCATE 25, 1: PRINT "Y ="; Y + ScrTop - 1; "      ";
END SUB
DEFINT A-Z
SUB SprDisplay
  FOR Y% = 0 TO 9: PUT (299, Y% * 20), Terrain(0, Y%), PSET
  NEXT: END SUB
SUB SpriteDefine: LINE (0, 0)-(19, 19), 255, BF
  LINE (0, 0)-(19, 19), 0, B: GET (0, 0)-(19, 19), Cursor(0, 0)
  LINE (0, 0)-(19, 19), 15, B: GET (0, 0)-(19, 19), Cursor(0, 1)
  FOR Spr% = 0 TO 9: LINE (0, 0)-(19, 19), Spr% + 1, BF
    GET (0, 0)-(19, 19), Terrain(0, Spr%)
    PUT (20 * Spr% + 30, 10 * Spr% + 30), Terrain(0, Spr%), PSET
  NEXT
  FOR Y% = 1 TO 320: Map(Y%) = STRING$(200, 0): NEXT
END SUB
'_|_|_|   MAPMAKER.BAS   ( end: part 2 of 2 )
 
