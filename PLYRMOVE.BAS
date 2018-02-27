'_|_|_|   PLYRMOVE.BAS   PUBLIC DOMAIN   Kurt Kuzba  (11/23/1999)
'_|_|_|   A demonstration of moving a block sprite without erasing
'_|_|_|   the background data, using GET and PUT.
DECLARE SUB PlayerMove (px%, py%, npx%, npy%, mask%(), sprite%())
DEFINT A-Z
SCREEN 13
UpKey$ = CHR$(0) + CHR$(72)
DownKey$ = CHR$(0) + CHR$(80)
RightKey$ = CHR$(0) + CHR$(77)
LeftKey$ = CHR$(0) + CHR$(75)
DIM SHARED bgb(201) AS INTEGER
DIM pum(201) AS INTEGER, pu(201) AS INTEGER
DIM pdm(201) AS INTEGER, pd(201) AS INTEGER
DIM plm(201) AS INTEGER, pl(201) AS INTEGER
DIM prm(201) AS INTEGER, pr(201) AS INTEGER
FOR x% = 0 TO 19: FOR y% = 0 TO 19: PSET (x%, y%), 14: NEXT: NEXT
GET (0, 0)-(19, 19), pum: GET (0, 0)-(19, 19), pu
GET (0, 0)-(19, 19), pdm: GET (0, 0)-(19, 19), pd
GET (0, 0)-(19, 19), prm: GET (0, 0)-(19, 19), pr
GET (0, 0)-(19, 19), plm: GET (0, 0)-(19, 19), pl
FOR x% = 0 TO 319
  FOR y% = 0 TO 199
    PSET (x%, y%), (x% MOD 32) + (y% MOD 20) AND 127
  NEXT
NEXT
GET (px, py)-(px + 19, py + 19), bgb
PUT (px, py), pr, PSET
 DO
  k% = 0: k$ = INKEY$
  IF k$ > "" THEN
    k% = ASC(k$): IF k% = 0 THEN k% = -ASC(MID$(k$, 2))
  END IF
  SELECT CASE k%
    CASE -80, 50
      IF py < 176 THEN
        PlayerMove px, py, px, py + 5, pum(), pu()
      END IF
    CASE -72, 56
      IF py > 4 THEN
        PlayerMove px, py, px, py - 5, pdm(), pd()
      END IF
    CASE -77, 54
      IF px < 296 THEN
        PlayerMove px, py, px + 5, py, prm(), pr()
      END IF
    CASE -75, 52
      IF px > 4 THEN
        PlayerMove px, py, px - 5, py, plm(), pl()
      END IF
    CASE 49
      IF (px > 4) AND (py < 176) THEN
        PlayerMove px, py, px - 5, py + 5, plm(), pl()
      END IF
    CASE 51
      IF (px < 296) AND (py < 176) THEN
        PlayerMove px, py, px + 5, py + 5, prm(), pr()
      END IF
    CASE 55
      IF (px > 4) AND (py > 4) THEN
        PlayerMove px, py, px - 5, py - 5, plm(), pl()
      END IF
    CASE 57
      IF (px < 296) AND (py > 4) THEN
        PlayerMove px, py, px + 5, py - 5, prm(), pr()
      END IF
    CASE 27: EXIT DO
  END SELECT
 LOOP
SCREEN 0: WIDTH 80, 25

SUB PlayerMove (px, py, npx, npy, mask(), sprite())
  WAIT &H3DA, 8: WAIT &H3DA, 8, 8
  PUT (px, py), bgb, PSET
  GET (npx, npy)-(npx + 19, npy + 19), bgb
  PUT (npx, npy), mask, AND
  PUT (npx, npy), sprite, OR
  px = npx
  py = npy
END SUB

