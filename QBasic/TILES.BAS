'Here's an interesting bent .. how about using TILING ?
' --- Cut Here --- Cut Here --- Cut Here --- Cut Here --- Cut Here
RANDOMIZE TIMER
SCREEN 12
RANDOMIZE TIMER
LINE (220, 140)-(419, 339), 7, B
DO
  Tile$ = ""
  FOR I% = 1 TO 63          ' Construct 63 Different Tile Patterns
     FOR J% = 0 TO 3          ' Loop SideWays thru the Bit Planes
        Char% = RND * 255       ' Random Tile Value
        Tile$ = Tile$ + CHR$(Char%)   ' Build Tiling String
     NEXT J%                  ' Go Back and Do it again ...
  NEXT I%
  FOR I% = 1 TO 100
    k$ = INKEY$: IF k$ > "" THEN EXIT FOR
    WAIT &H3DA, 8, 8
    WAIT &H3DA, 8, 0
  NEXT I%
  IF k$ > "" THEN EXIT DO
  LINE (221, 141)-(418, 338), 0, BF
  PAINT (320, 240), Tile$, 7
LOOP
' --- Cut Here --- Cut Here --- Cut Here --- Cut Here --- Cut Here
 

