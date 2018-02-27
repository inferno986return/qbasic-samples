SCREEN 12
RANDOMIZE TIMER
TYPE XYCoordinates: x AS INTEGER: y AS INTEGER: END TYPE
DIM shape(701) AS INTEGER, template(701) AS INTEGER
DIM Position(2595) AS XYCoordinates
FOR x% = 0 TO 2595
  Position(x%).x = (x% MOD 59) * 10
  Position(x%).y = (x% \ 59) * 10
NEXT
FOR x% = 1 TO 10000
  Sw% = RND * 2595
  SWAP Position(x% MOD 2596), Position(Sw%)
NEXT
LINE (0, 0)-(49, 49), RND * 15, BF
x% = RND * 15
CIRCLE (24, 24), 19, x%
PAINT (24, 24), x%
GET (0, 0)-(49, 49), template
FOR x% = 0 TO 701: shape(x%) = template(x%): NEXT
WHILE INKEY$ > "": WEND
x% = 0
DO
  IF INKEY$ > "" THEN EXIT DO
  SELECT CASE INT(RND * 4)
    CASE 0: PUT (Position(x%).x, Position(x%).y), shape, PRESET
    CASE 1: PUT (Position(x%).x, Position(x%).y), shape, PSET
    CASE 2: PUT (Position(x%).x, Position(x%).y), shape, AND
    CASE 3: PUT (Position(x%).x, Position(x%).y), shape, XOR
    CASE 4: PUT (Position(x%).x, Position(x%).y), shape, OR
  END SELECT
  x% = x% + 1
  x% = x% MOD 2596
  IF x% = 0 THEN
    SELECT CASE RND
      CASE IS > .3
        GET (53, 53)-(102, 102), shape
      CASE ELSE
        FOR x% = 0 TO 701: shape(x%) = template(x%): NEXT
    END SELECT
  END IF
LOOP
SCREEN 0: SYSTEM

