'_|_|_|   PLAYMOVE.BAS   PUBLIC DOMAIN   Kurt Kuzba  (6/19/1999)
'_|_|_|   Simple example of testing maps for sprite movement
DECLARE SUB initMap ()
DECLARE SUB Drawmap ()
DECLARE FUNCTION CheckMap% (x%, y%)
DECLARE SUB Mover (x%, y%, d%)
DECLARE SUB PMove (px%, py%, npx%, npy%)
DEFINT A-Z
SCREEN 13
RANDOMIZE TIMER
TYPE MapHorizontal: m AS STRING * 16: END TYPE
DIM SHARED background(201) AS INTEGER, Player(201) AS INTEGER
DIM SHARED MoveYes(201) AS INTEGER, MoveNo(201) AS INTEGER
DIM SHARED Map(1 TO 10) AS MapHorizontal
LINE (0, 0)-(39, 39), 6, BF
CIRCLE (9, 9), 9, 33: PAINT (9, 9), 33
GET (0, 0)-(19, 19), Player
GET (20, 20)-(39, 39), MoveYes
CIRCLE (29, 29), 9, 4: LINE (23, 23)-(34, 34), 4
GET (20, 20)-(39, 39), MoveNo
initMap
Drawmap
GET (0, 0)-(19, 19), background
PUT (0, 0), Player, PSET
DO
  k% = 0: k$ = INKEY$
  IF k$ > "" THEN
    k% = ASC(k$): IF k% = 0 THEN k% = -ASC(MID$(k$, 2))
  END IF
  SELECT CASE k%
    CASE -71, 55: IF (py > 4) AND (px > 4) THEN Mover px, py, 7
    CASE -72, 56: IF (py > 4) THEN Mover px, py, 8
    CASE -73, 57: IF (py > 4) AND (px < 296) THEN Mover px, py, 9
    CASE -75, 52: IF (px > 4) THEN Mover px, py, 4
    CASE -77, 54: IF (px < 296) THEN Mover px, py, 6
    CASE -79, 49: IF (py < 176) AND (px > 4) THEN Mover px, py, 1
    CASE -80, 50: IF (py < 176) THEN Mover px, py, 2
    CASE -81, 51: IF (py < 176) AND (px < 296) THEN Mover px, py, 3
    CASE 27: EXIT DO
  END SELECT
LOOP
SCREEN 0: WIDTH 80, 25

FUNCTION CheckMap% (x, y)
      IF MID$(Map(y \ 20 + 1).m, x \ 20 + 1, 1) = "1" THEN CheckMap% = 1
END FUNCTION

DEFSNG A-Z
SUB Drawmap
  FOR x% = 1 TO 16: FOR y% = 1 TO 10
      IF MID$(Map(y%).m, x%, 1) = "0" THEN
        PUT ((x% - 1) * 20, (y% - 1) * 20), MoveYes, PSET
      ELSE
        PUT ((x% - 1) * 20, (y% - 1) * 20), MoveNo, PSET
      END IF
    NEXT: NEXT: END SUB

SUB initMap : legend$ = "0000011"
  FOR y% = LBOUND(Map) TO UBOUND(Map): FOR x% = 1 TO 16
      MID$(Map(y%).m, x%) = MID$(legend$, RND * 6 + 1, 1)
    NEXT: NEXT: MID$(Map(1).m, 1, 1) = "0": END SUB

DEFINT A-Z
SUB Mover (x, y, d)
  SELECT CASE d
    CASE 1: Mover x, y, 2: Mover x, y, 4
    CASE 2
      ok = CheckMap(x + 15, y + 20) + CheckMap(x, y + 20)
      IF (ok = 0) THEN PMove x, y, x, y + 5
    CASE 3: Mover x, y, 2: Mover x, y, 6
    CASE 4
      ok = CheckMap(x - 1, y + 15) + CheckMap(x - 1, y)
      IF (ok = 0) THEN PMove x, y, x - 5, y
    CASE 6
      ok = CheckMap(x + 20, y) + CheckMap(x + 20, y + 15)
      IF (ok = 0) THEN PMove x, y, x + 5, y
    CASE 7: Mover x, y, 8: Mover x, y, 4
    CASE 8
      ok = CheckMap(x + 15, y - 1) + CheckMap(x, y - 1)
      IF (ok = 0) THEN PMove x, y, x, y - 5
    CASE 9: Mover x, y, 8: Mover x, y, 6
  END SELECT
END SUB

SUB PMove (px, py, npx, npy)
  WAIT &H3DA, 8: WAIT &H3DA, 8, 8
  PUT (px, py), background, PSET
  GET (npx, npy)-(npx + 19, npy + 19), background
  PUT (npx, npy), Player, PSET: px = npx: py = npy: END SUB

