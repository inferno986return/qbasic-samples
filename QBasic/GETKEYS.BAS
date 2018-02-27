DECLARE FUNCTION GetKey% (w%)
DO
  k% = GetKey(0)
  IF k% <> 0 THEN PRINT k%;
LOOP WHILE k% <> 27

FUNCTION GetKey% (w%)
  DO
    k$ = INKEY$
    IF (w% = 0) AND (k$ = "") THEN k$ = CHR$(0)
  LOOP WHILE k$ = ""
  k% = ASC(k$)
  IF (k% = 0) AND (w% > 0) THEN k% = -ASC(MID$(k$, 2))
  GetKey% = k%
END FUNCTION

