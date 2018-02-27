DECLARE SUB UpdateStats (L%)
TYPE SpriteDefinition
  X AS INTEGER  ' Sprite horizontal coordinate
  Y AS INTEGER  ' Sprite vertical coordinate
  W AS INTEGER  ' Sprite width, in pixels
  H AS INTEGER  ' Sprite height, in pixels
  d AS INTEGER  ' Sprite definition, which array
  r AS INTEGER  ' Sprite rotational direction, for rocks
END TYPE
'_|_|_|   We need a sprite type to keep our data tidy
TYPE ScoreData
  L AS INTEGER       ' Level completed
  s AS LONG          ' Score attained
  n AS STRING * 16   ' Player name
END TYPE
'_|_|_|   A score type make handling the data much simpler
DECLARE FUNCTION Menu% (M%)
'_|_|_|   Main menu.  Returns value of chosen menu item
DECLARE SUB GetNewHi (Hi() AS ScoreData, L%)
'_|_|_|   Add new high score to hi score array using insertion sort
DECLARE SUB DefaultHiScores (H() AS ScoreData)
'_|_|_|   Set fame defaults for high score screen
DECLARE SUB AboutLander ()
'_|_|_|   Show About screen.  General instructions and PD notice
DECLARE SUB HighScores ()
'_|_|_|   Show high scores screen, add new high if appropriate
DECLARE SUB DefinePad (L%)
'_|_|_|   Create landing pad according to current level
DECLARE SUB Falling ()
'_|_|_|   Gravity's Angel ( G )  Enables downward force on ship
DECLARE FUNCTION Thrust% (inc%, dec%, res%, lim%, Sprite%)
'_|_|_|   Apply movement to ship when thrusters are fired
DECLARE SUB GameMessage (M$)
'_|_|_|   Display Crash and success messages onscreen
DECLARE SUB Shipdocked (L%)
'_|_|_|   Roll rocks and wait for ship to launch
DECLARE FUNCTION PlayLevel% (L%)
'_|_|_|   The actual game code itself resides in here
DECLARE SUB DefineShip ()
'_|_|_|   This creates the array data for the ship
DECLARE SUB SetupShip ()
'_|_|_|   This creates the ship sprites
DECLARE SUB SetupRocks ()
'_|_|_|   This creates the rock sprites
DECLARE SUB DefineRock ()
'_|_|_|   This creates the array data for the rocks
DECLARE SUB SetupForLevel (L%)
'_|_|_|   This initializes level data and sets up the background
DECLARE SUB SetupBackground ()
'_|_|_|   This is called by SetupForLevel to initialize background
DECLARE SUB UpdateDisplay ()
'_|_|_|   This creates the sceen and PUTs it to the video memory
DECLARE SUB RotatePalette ()
'_|_|_|   This makes the stars shift to random colors
DECLARE SUB DefineBackground ()
'_|_|_|   This creates the planetary surface
DECLARE SUB MakeBackground (spr() AS STRING, d%)
'_|_|_|   This makes PUTable arrays from the background data
DEFINT A-Z
'_|_|_|   Integers make everything work faster
'$DYNAMIC
'_|_|_|   We will need a good chunk of memory from the system
ON ERROR GOTO Oops
'_|_|_|   We'll need this to test for the hiscores file
RANDOMIZE TIMER
'_|_|_|   It's always nice to have a randomized random
SCREEN 13
'_|_|_|   A nice linear video memory map to work with
DIM SHARED Rock(4, 3) AS SpriteDefinition
'_|_|_|   We need a bunch of sprites for our worldlets
DIM SHARED RockDef(27) AS STRING * 170
'_|_|_|   Our wordlets need data to put on the screen
DIM SHARED Ship AS SpriteDefinition
'_|_|_|   The ship needs a sprite, too
DIM SHARED ShipDef(3) AS STRING * 65
'_|_|_|   The ship also needs a few data blocks
DIM SHARED Pad AS SpriteDefinition
'_|_|_|   The pad needs a sprite
DIM SHARED PadDef AS STRING * 200
'_|_|_|   And the pad needs data
DIM SHARED BackDrop(27001) AS INTEGER
'_|_|_|   This will be our sketchpad for the screen
DIM SHARED BackupDrop(27001) AS INTEGER
'_|_|_|   This will hold the unchanging background data
DIM SHARED Ground(51, 4) AS INTEGER
'_|_|_|   This is for the planetary surface data
DIM SHARED Score AS LONG
'_|_|_|   This is pretty obvious...  :)
CLS
'_|_|_|   We start out with a nice, clean screen
DefineShip
'_|_|_|   Set up the ship data
SetupRocks
'_|_|_|   Set up the rock data
DefineBackground
'_|_|_|   Set up the background data
Celect = 0
'_|_|_|   Default menu selection is PLAY
Playspeed% = 0
'_|_|_|   Set gamespeed delay to zero
DO
'_|_|_|   begin game loop
  Score = 0
  '_|_|_|   Initialize player data
  Celect = Menu%(Celect)
  '_|_|_|   Get player choice from Menu
  SELECT CASE Celect
  '_|_|_|   Process player choice
    CASE 0
    '_|_|_|   Play game
      Lv = 0
      '_|_|_|   Initialize Level
      Fuel = 0
      '_|_|_|   Initialize Fuel
      DO
      '_|_|_|   Begin play loop
      LOOP WHILE PlayLevel%(Lv) = 0
      '_|_|_|   Loop while game not over
      HighScores
      '_|_|_|   View/Add_to high score data
    CASE 1
    '_|_|_|   View high scores
      Lv = 0
      '_|_|_|   Initialize level
      HighScores
      '_|_|_|   View high scores
    CASE 2
    '_|_|_|   View About screen
      AboutLander
      '_|_|_|   View About screen
    CASE 3
    '_|_|_|   Quit Game
      EXIT DO
      '_|_|_|   Exit Game loop
  END SELECT
LOOP
'_|_|_|   Rerun game loop
SCREEN 0
'_|_|_|   Reset to text screen
WIDTH 80, 25
'_|_|_|   Reset screen to 80x25 text
SYSTEM
'_|_|_|   Return whence called
Oops:
'_|_|_|   Error handling
  FError$ = STR$(ERR)
  '_|_|_|   Reset error variable to show error value
  RESUME NEXT
  '_|_|_|   Return to point of error

REM $STATIC
SUB AboutLander
  CLS
  FOR X = 0 TO 28
    LINE (0 + X, 0 + X)-(319 - X, 199 - X), 128 + X, B
  NEXT
  '_|_|_|   clear screen and set up background
  LOCATE 5
  COLOR 9
  LOCATE , 5: PRINT "QBLander is compiled in QB 4.5. "
  LOCATE , 5: PRINT "It needs Pentium 166 or better. "
  COLOR 11
  LOCATE , 5: PRINT "Pilot your ship to the surface  "
  LOCATE , 5: PRINT "and back.  Use down, right, and "
  LOCATE , 5: PRINT "left arrow keys for thrusters.  "
  LOCATE , 5: PRINT "Avoid the moving worlds, landing"
  LOCATE , 5: PRINT "softly ONLY in red landing zone."
  LOCATE , 5: PRINT "While docked, you may hit S to  "
  LOCATE , 5: PRINT "change the game delay speed.    "
  COLOR 10
  LOCATE , 5: PRINT "New levels give you 400 fuel and"
  LOCATE , 5: PRINT "999 Timer Bonus points. You may "
  LOCATE , 5: PRINT "collect up to 999 fuel points.  "
  COLOR 7
  LOCATE , 5: PRINT "  QB Lander released to Public  "
  LOCATE , 5: PRINT "   Domain September 20, 1999.   "
  COLOR 15
  LOCATE , 5: PRINT "    -( Updated 8/19/2001 )-     "
  COLOR 14
  LOCATE , 5: PRINT "  All code and graphics are the "
  LOCATE , 5: PRINT "  original work of Kurt Kuzba.  "
  '_|_|_|   Display About information
  DO: LOOP WHILE INKEY$ > ""
  '_|_|_|   Clear keyboard buffer
  DO: LOOP WHILE INKEY$ = ""
  '_|_|_|   Wait for a keypress
END SUB

SUB DefaultHiScores (H() AS ScoreData)
  H(0).L = 1
  '_|_|_|   Define player level completed as level one
  H(0).s = 100
  '_|_|_|   Define player high score as one hundred
  H(0).n = "QuikBasic Lander"
  '_|_|_|   Define player name as game name
  FOR X = 1 TO 9
    H(X) = H(0)
  NEXT
  '_|_|_|   Initialize all array elements to match the first
END SUB

SUB DefineBackground
  DIM spr(11 TO 15) AS STRING
  '_|_|_|   Set up a local array for the data
  spr(11) = ".....o...ww........."
  spr(12) = "..ww....wsw....w...."
  spr(13) = "..ww....wssw..www..."
  spr(14) = ".wwsw..wwssswwssswww"
  spr(15) = "wwwsswwwwssswwssssww"
  MakeBackground spr(), 0
  '_|_|_|   Define and save the array data
  spr(11) = ".....w..a..........."
  spr(12) = "...www.....www......"
  spr(13) = "..wwsw....wwwsw....."
  spr(14) = ".wwssw...wwwssw....."
  spr(15) = "wwwssswwwwwsssswwwww"
  MakeBackground spr(), 1
  '_|_|_|   Define and save the array data
  spr(11) = ".....o..ww.........."
  spr(12) = "......wwssw.......c."
  spr(13) = "....wwwwsssw........"
  spr(14) = "..wwwwwwwsssw..wsw.."
  spr(15) = "wwwwwwwwwsssswwwssww"
  MakeBackground spr(), 2
  '_|_|_|   Define and save the array data
  spr(11) = ".......a....www...o."
  spr(12) = "...www.....wwwsw...."
  spr(13) = "..wwwsw....wwwssw..."
  spr(14) = "wswwwsswwwwwwwsssww."
  spr(15) = "swwwwwsswwwwwwswsssw"
  MakeBackground spr(), 3
  '_|_|_|   Define and save the array data
  spr(11) = "..w.....wwww........"
  spr(12) = ".ww....wwssw.....w.."
  spr(13) = ".wsw..wwwsssw...wsw."
  spr(14) = "wwsswwwwwsssw..wwssw"
  spr(15) = "wwwssswwwwsswwwwwsss"
  MakeBackground spr(), 4
  '_|_|_|   Define and save the array data
END SUB

SUB DefinePad (L)
  Pad.W = 32 - L * 2
  '_|_|_|   Make the landing pad shrink at higher levels
  IF Pad.W < 10 THEN Pad.W = 10
  '_|_|_|   Define a minimum pad width
  Pad.H = 5
  '_|_|_|   Define the height of the pad
  Pad.Y = 175
  '_|_|_|   Define the vertical location of the pad
  SELECT CASE L
  '_|_|_|   Define horizontal location of pad by level
    CASE IS < 3
    '_|_|_|   If player is level one or two, then
      Pad.X = RND * 70 + 100
      '_|_|_|   the pad should be near the screen center
    CASE 3 TO 6
    '_|_|_|   If player is level three to six, then
      Pad.X = RND * 150 + 70
      '_|_|_|   the pad should range further toward screen edge
    CASE IS > 6
    '_|_|_|   If player is above level six, then
      Pad.X = RND * 250 + 10
      '_|_|_|   the pad should range nearly to the screen edge
  END SELECT
  PadDef = STRING$(Pad.W * 3, 255) + STRING$(Pad.W * 2, 12)
  '_|_|_|   Place pixel data into sprite data array
END SUB

SUB DefineRock
  DIM Rd(0 TO 12) AS STRING
  '_|_|_|   Create local array for rock pixel data
  Rd(0) = "xxccxxxxxccxxxc"
  Rd(1) = "cxxxxcccxxxxcccccxxxxccxxxxccxxxxcc"
  Rd(2) = "bbbbbzzzzzzzzzbbbbbbzzxxxccccxxxbbbbbbbbbbzzzz"
  Rd(3) = "vvbzzbvbzzzzzzzbbbvvbzbbbzzzzzzzzzzzzzzzzbbbvvbzzbvbzzz"
  Rd(4) = "vvbzbbbzzzzzzzbbzbbbbbbvbbbbbbvzzzzzzzzbbbbbvvbzbbbzzzz"
  Rd(5) = "vvvvbbbvzzzzzzbbbbbbbzbbbbbbbbbbbbzzzzzzzzzzzbbzbvvvvvvvbbbvzzzzz"
  Rd(6) = "bbbvvvbzzvzzzzbbvvvvbbbzzzbbbbbbbzzzzbbzzzzzbbbbbbbbbbbvvvbzzvzzz"
  Rd(7) = "zzbbbvbzzvvzzzbbvvvbbbzzzzzzzbbbbbzzzbbbzzzbbvvbbzzzzzbbbvbzzvvzz"
  Rd(8) = "zzbbbbzzzzzbbvvvvbzzzzzzzvvvvvvvvzzzvvbbbzzzzzbbbbzzzzz"
  Rd(9) = "zzzzzbbbzzbbvvvvbbzzzzzzzzzvvvvvvvvvvbbbzzzzzzzzzbbbzzb"
  Rd(10) = "cccvvxbbbbbbbbxxccccxxcvvvvvvvvbxxccccccvvxbbb"
  Rd(11) = "ccxxxcccxxxxcccxxxxxcccxxxccccxxxcc"
  Rd(12) = "ccxxxcxxxcccccx"
  '_|_|_|   Define pixel data from previously developed Mercatur
  '_|_|_|   projection of rock with extraneous spaces removed
  FOR r = 0 TO 12
  '_|_|_|   For each of the data strings
    L = LEN(Rd(r))
    '_|_|_|   Make L equal to the length of the string to avoid
    '_|_|_|   performing math inside the FOR loop which follows
    FOR P = 1 TO L
    '_|_|_|   For every element in the string
      SELECT CASE MID$(Rd(r), P, 1)
      '_|_|_|   Determine array element content and convert it
        CASE "z"
        '_|_|_|   If the string element is a 'z'
          MID$(Rd(r), P, 1) = CHR$(125)    'blue
          '_|_|_|   replace it with pixel data for blue
        CASE "x"
        '_|_|_|   If the string element is an 'x'
          MID$(Rd(r), P, 1) = CHR$(100)    'white
          '_|_|_|   replace it with pixel data for white
        CASE "c"
        '_|_|_|   If the string element is a 'c'
          MID$(Rd(r), P, 1) = CHR$(101)    'blue/white
          '_|_|_|   replace it with pixel data for blue-white
        CASE "v"
        '_|_|_|   If the string element is a 'v'
          MID$(Rd(r), P, 1) = CHR$(114)    'brown
          '_|_|_|   replace it with pixel data for brown
        CASE "b"
        '_|_|_|   If the string element is a 'b'
          MID$(Rd(r), P, 1) = CHR$(191)    'green
          '_|_|_|   replace it with the pixel data for green
      END SELECT
      '_|_|_|   The string data has been converted to pixel data
    NEXT
    '_|_|_|   Convert the next element of string data
  NEXT
  '_|_|_|   Convert the next string
  FOR r = 0 TO 27
  '_|_|_|   Create the 27 rock pictures from the string data.
  '_|_|_|   We have used the second string as being on a one
  '_|_|_|   to one ratio for the rotation.  All the other
  '_|_|_|   strings will be on another ratio, as determined by
  '_|_|_|   their length and the number 27.  This ratio will be
  '_|_|_|   used to determine what portion of the string should
  '_|_|_|   appear in each rock picture.  A zero pixel value will
  '_|_|_|   be inserted to indicate that there is no pixel data
  '_|_|_|   at some locations and the background data should be
  '_|_|_|   allowed to remain when the sprite is inserted into
  '_|_|_|   the video screen data at the time it is displayed.
    r$ = STRING$(5, 0)
    '_|_|_|   Begin with five transparent pixels
    r$ = r$ + MID$(Rd(0), 1 + r * .42857, 3) + STRING$(8, 0)
    '_|_|_|   Add a portion from n90 string, and transparents
    r$ = r$ + MID$(Rd(1), 1 + r, 7) + STRING$(5, 0)
    '_|_|_|   Add data from n75 string, and transparents
    r$ = r$ + MID$(Rd(2), 1 + r * 1.2857, 9) + STRING$(3, 0)
    '_|_|_|   Add data from n60 string and transparents
    r$ = r$ + MID$(Rd(3), 1 + r * 1.5714, 11) + STRING$(2, 0)
    '_|_|_|   Add data from n45 string and transparents
    r$ = r$ + MID$(Rd(4), 1 + r * 1.5714, 11) + STRING$(1, 0)
    '_|_|_|   Add data from n30 string and transparent
    r$ = r$ + MID$(Rd(5), 1 + r * 1.8571, 13)
    '_|_|_|   Add data from n15 string
    r$ = r$ + MID$(Rd(6), 1 + r * 1.8571, 13)
    '_|_|_|   Add data from equatorial string
    r$ = r$ + MID$(Rd(7), 1 + r * 1.8571, 13) + STRING$(1, 0)
    '_|_|_|   Add data from s15 string and transparent
    r$ = r$ + MID$(Rd(8), 1 + r * 1.5714, 11) + STRING$(2, 0)
    '_|_|_|   Add data from s30 string and transparents
    r$ = r$ + MID$(Rd(9), 1 + r * 1.5714, 11) + STRING$(3, 0)
    '_|_|_|   Add data from s45 string and transparents
    r$ = r$ + MID$(Rd(10), 1 + r * 1.2857, 9) + STRING$(5, 0)
    '_|_|_|   Add data from s60 string and transparents
    r$ = r$ + MID$(Rd(11), 1 + r, 7) + STRING$(8, 0)
    '_|_|_|   Add data from s75 string and transparents
    r$ = r$ + MID$(Rd(12), 1 + r * .42857, 3) + STRING$(5, 0)
    '_|_|_|   Add data from s90 string and transparents
    RockDef(r) = r$
    '_|_|_|   Place data into string array
  NEXT
  '_|_|_|   Next rotational position/string
END SUB

DEFSNG A-Z
SUB DefineShip
  DIM s AS STRING
  '_|_|_|   Define a string without using the $ identifier
  s = ""
  '_|_|_|   Clear the string.  I only do this to make the ship
  '_|_|_|   definition strings line up on the screen for ease
  '_|_|_|   of visualization during the design phase.. :)
  s = s + "..====.."
  s = s + ".==uu==."
  s = s + "==uoou=="
  s = s + "========"
  '_|_|_|   Create the top for lines of the ship
  FOR X = 0 TO 3
    ShipDef(X) = s
  NEXT
  '_|_|_|   Make all ship sprites use the same top data
  s = ""
  s = s + "=..uu..="
  s = s + "=......="
  s = s + "=......="
  s = s + "==....=="
  MID$(ShipDef(0), 33) = s
  '_|_|_|   Define bottom of ship without thrust applied
  s = ""
  s = s + "=\/uu..="
  s = s + "/\/\...="
  s = s + "\/\....="
  s = s + "==....=="
  MID$(ShipDef(1), 33) = s
  '_|_|_|   Define bottom of ship with left thrust applied
  s = ""
  s = s + "=..uu/\/"
  s = s + "=...\/\="
  s = s + "=....\/\"
  s = s + "==....=="
  MID$(ShipDef(2), 33) = s
  '_|_|_|   Define bottom of ship with right thrust applied
  s = ""
  s = s + "=..uu/.="
  s = s + "=.\/.\.="
  s = s + "=.\///.="
  s = s + "==/.\.=="
  MID$(ShipDef(3), 33) = s
  '_|_|_|   Define bottom of ship with downward thrust applied
  FOR Sh = 0 TO 3
  '_|_|_|   For each ship sprite
    FOR P = 1 TO 64
    '_|_|_|   For each element of the string
      SELECT CASE MID$(ShipDef(Sh), P, 1)
      '_|_|_|   Convert according to data in string
        CASE "="
        '_|_|_|   If the string data is '='
          MID$(ShipDef(Sh), P, 1) = CHR$(15)
          '_|_|_|   convert to pixel data for white
        CASE "o"
        '_|_|_|   If the string data is 'o'
          MID$(ShipDef(Sh), P, 1) = CHR$(9)
          '_|_|_|   convert to pixel data for light blue
        CASE "u"
        '_|_|_|   If the string data is 'u'
          MID$(ShipDef(Sh), P, 1) = CHR$(7)
          '_|_|_|   convert to pixel data for light gray
        CASE "\"
        '_|_|_|   If the string data is '\'
          MID$(ShipDef(Sh), P, 1) = CHR$(14)
          '_|_|_|   convert to pixel data for yellow
        CASE "/"
        '_|_|_|   If the string data is '/'
          MID$(ShipDef(Sh), P, 1) = CHR$(4)
          '_|_|_|   convert to pixel data for red
        CASE ELSE
        '_|_|_|   If the string data is none of the above
          MID$(ShipDef(Sh), P, 1) = CHR$(0)
          '_|_|_|   convert to pixel data for transparence
      END SELECT
      '_|_|_|   Conversion completed
    NEXT
    '_|_|_|   Next string element
  NEXT
  '_|_|_|   Next string
END SUB

DEFINT A-Z
SUB Falling
  SHARED ThrustDown, ThrustUp
  '_|_|_|   Need to know present thrust status
  IF Ship.Y < 181 THEN
  '_|_|_|   If the ship is not a low as it can go
    ThrustDown = ThrustDown + (ThrustDown > 0)
    '_|_|_|   Decrease upward momentum
    ThrustUp = ThrustUp - (ThrustUp < 80)
    '_|_|_|   Increase downward momentum
  END IF
END SUB

SUB GameMessage (M$)
  DIM Buf(4656): GET (10, 32)-(300, 55), Buf
  '_|_|_|   Create local array to remember screen data
  COLOR 12
  '_|_|_|   Change to color for message display
  LOCATE 5, 5
  '_|_|_|   Place cursor at message space
  PRINT " "; M$; " ";
  '_|_|_|   Print message
  LOCATE 7, 5
  '_|_|_|   Place cursor for 'continue' message
  PRINT " Hit SPACE to continue. ";
  '_|_|_|   Print message
  DO: LOOP WHILE INKEY$ > ""
  '_|_|_|   Clear keyboard buffer
  DO: LOOP WHILE INKEY$ <> " "
  '_|_|_|   Wait for space key to be pressed
  PUT (10, 32), Buf, PSET
  '_|_|_|   Restore screen data destroyed in message display
END SUB

SUB GetNewHi (H() AS ScoreData, L)
  SHARED Lv, FError$
  '_|_|_|   Need to know what level player is on
  '_|_|_|   Also need to share File Error data with ON ERROR
  DIM Hi(9) AS ScoreData
  '_|_|_|   Need an array to hold high score data
  FOR X = 0 TO 9
    Hi(X) = H(X)
  NEXT
  '_|_|_|   Copy old high score data to new array
  COLOR 79
  '_|_|_|   Set color for input display background
  FOR X = 5 TO 11
    LOCATE X, 10
    PRINT STRING$(20, "ฒ");
  NEXT
  '_|_|_|   Print out a block of background characters
  FOR W = 9 TO 1 STEP -1
  '_|_|_|   Starting at the bottom of the high score list
    Hi(W) = Hi(W - 1)
    '_|_|_|   Move data from the next highest score down
    IF Score <= Hi(W).s THEN EXIT FOR
    '_|_|_|   If this score is higher than present score
    '_|_|_|   then we are done moving data, so exit loop
  NEXT
  '_|_|_|   W should now be the proper slot for new high score
  L = W
  '_|_|_|   Set variable for display highlight
  Hi(W).L = Lv - 1
  '_|_|_|   Set high score level to last level player completed
  Hi(W).s = Score
  '_|_|_|   Set high score score to player score
  Hi(W).n = STRING$(16, " ")
  '_|_|_|   Initialize high score name to blank spaces
  C = 1
  '_|_|_|   Set input cursor at string position 1
  COLOR 14
  '_|_|_|   Set text display color for input routine display
  LOCATE 6, 11: PRINT " A new high score "
  '_|_|_|   Print line one of display at specified location
  LOCATE 8, 11: PRINT "   Enter a name   "
  '_|_|_|   Print line two of display at specified location
  COLOR 15
  '_|_|_|   Set text color for input routine
  DO
  '_|_|_|   Initialize input routine loop
    LINE (88, 80)-(215, 80), 0
    '_|_|_|   Clear a line below input text for blinking cursor
    LOCATE 10, 12: PRINT Hi(W).n;
    '_|_|_|   Print present player name at specified location
    DEF SEG = &H40
    '_|_|_|   Define memory segment as the BIOS area
    Blink = PEEK(&H6C) AND 8: B = 1
    '_|_|_|   Set a variable by bit 3 of the BIOS clock
    LINE ((10 + C) * 8, 80)-((11 + C) * 8 - 1, 80), B * 15
    '_|_|_|   Place cursor under present text entry location
    DO
    '_|_|_|   Initialize key input loop
      k$ = INKEY$
      '_|_|_|   Get a key from keyboard queue
      IF Blink <> (PEEK(&H6C) AND 8) THEN
      '_|_|_|   If bit three of the BIOS clock has changed
        B = (B + 1) AND 1
        '_|_|_|   Change blink attribute of line cursor
        LINE ((10 + C) * 8, 80)-((11 + C) * 8 - 1, 80), B * 15
        '_|_|_|   Display line cursor onscreen
        Blink = PEEK(&H6C) AND 8
        '_|_|_|   Reset blink variable to bit three of BIOS clock
      END IF
    LOOP WHILE k$ = ""
    '_|_|_|   Continue key input loop until a key is pressed
    k% = ASC(k$)
    '_|_|_|   Determine ascii value of keypress
    IF k% = 0 THEN k% = -(ASC(MID$(k$, 2)))
    '_|_|_|   If an extended key is used, use negative value
    SELECT CASE k%
    '_|_|_|   Act upon key input value
      CASE 8
      '_|_|_|   If the key pressed was the backspace
        IF C = 1 THEN Hi(W).n = MID$(Hi(W).n, 2)
        '_|_|_|   If the cursor cannot move left, delete character
        '_|_|_|   under cursor by moving remaining text left by one
        IF (C > 1) THEN
        '_|_|_|   If the cursor can move left then
          C = C - 1: MID$(Hi(W).n, C) = MID$(Hi(W).n, C + 1)
          '_|_|_|   Move cursor and text, from cursor, left by one
        END IF
        MID$(Hi(W).n, 16) = " "
        '_|_|_|   Make last character of string equal to a space
      CASE 32 TO 127
      '_|_|_|   If the key pressed is valid text character
        MID$(Hi(W).n, C) = k$
        '_|_|_|   Add character at present cursor location
        C = C - (C < 16)
        '_|_|_|   Increment cursor unless at end of string
      CASE -75
      '_|_|_|   If the key pressed is the left arrow
        C = C + (C > 1)
        '_|_|_|   move the cursor one space left if possible
      CASE -77
      '_|_|_|   If the key pressed is the right arrow
        C = C - (C < 16)
        '_|_|_|   move the cursor one space right if possible
      CASE 13
      '_|_|_|   If the key pressed is the ENTER key
        IF Hi(W).n = SPACE$(16) THEN DEF SEG : EXIT SUB
        '_|_|_|   If no name data was entered, exit sub
        FOR X = 0 TO 9
        '_|_|_|   For every high score in the array
          H(X) = Hi(X):
          '_|_|_|   Copy from temp array to passed array
        NEXT
        '_|_|_|   Next high score array element
        Fch = FREEFILE
        '_|_|_|   Get a handle for file access
        IF Fch = 0 THEN EXIT SUB
        '_|_|_|   If no handle available, skip disk access
        FError$ = "ok"
        '_|_|_|   Initialize error test variable
        OPEN "hiscores.dat" FOR RANDOM AS #Fch LEN = LEN(H(0))
        '_|_|_|   Open high scores file for random access
        IF FError$ = "ok" THEN
        '_|_|_|   If there is not a disk error, then
          FOR X = 0 TO 9
          '_|_|_|   For each element of the array
            PUT #Fch, X + 1, H(X)
            '_|_|_|   Save array data to the file
          NEXT
          '_|_|_|   Next array element
          CLOSE Fch
          '_|_|_|   Close the file
        END IF
        DEF SEG
        '_|_|_|   Define memory segment as default segment
        EXIT SUB
        '_|_|_|   Exit the sub, data entry completed
      CASE 27
      '_|_|_|   If the key pressed is the ESCape key
        DEF SEG
        '_|_|_|   Define memory segment as default segment
        EXIT SUB
        '_|_|_|   Exit the sub, data entry aborted
    END SELECT
    '_|_|_|   Keypress data has been processed
  LOOP
  '_|_|_|   Continue input routine loop
END SUB

SUB HighScores
  SHARED FError$, Lv
  '_|_|_|   Need to know the player level
  '_|_|_|   Need to know File Error status for data input routine
  CLS
  '_|_|_|   Start with a nice clean screen
  DIM HiScor(9) AS ScoreData
  '_|_|_|   Define local array to hold high scores data
  FError$ = "ok"
  '_|_|_|   Initialize error status variable
  Fch = FREEFILE
  '_|_|_|   Obtain file handle from system
  L = 10
  '_|_|_|   Set variable for highlight
  IF Fch > 0 THEN
  '_|_|_|   If a valid file handle has been obtained
    OPEN "hiscores.dat" FOR INPUT AS #Fch
    '_|_|_|   Open the file for input
    IF FError$ = "ok" THEN
    '_|_|_|   If the file already exists, then
      CLOSE Fch
      '_|_|_|   Close the file ( error variable is still set )
      OPEN "hiscores.dat" FOR RANDOM AS #Fch LEN = LEN(HiScor(0))
      '_|_|_|   Open the file for random access
      FOR X = 0 TO 9
      '_|_|_|   For each element in the array
        GET #Fch, X + 1, HiScor(X)
        '_|_|_|   Input the data from the file to the array
      NEXT
      '_|_|_|   Next array element
      CLOSE Fch
      '_|_|_|   Close the file
    END IF
    ELSE
    '_|_|_|   If no file handle was available from system
      FError$ = "no handle"
      '_|_|_|   Set error variable to indicate file access failure
  END IF
  IF FError$ <> "ok" THEN DefaultHiScores HiScor()
  '_|_|_|   If file access failed, use default high scores
  FOR X = 0 TO 14
    LINE (0 + X, 0 + X)-(319 - X, 199 - X), 15 + X, B
  NEXT
  LINE (0 + X, 0 + X)-(319 - X, 199 - X), 15 + X, BF
  '_|_|_|   Set up display background
  IF (Score > HiScor(9).s) AND (Lv > 1) THEN GetNewHi HiScor(), L
  '_|_|_|   If Player has surpassed level one, and their score
  '_|_|_|   is better than the lowest high score, then send them
  '_|_|_|   to the new high score sub. Pass array and highlight
  COLOR 49
  '_|_|_|   Set text color for display
  LOCATE 4, 3: PRINT "                                    ";
  LOCATE 5, 3: PRINT " ีัััออออออออออออออออออออออออออัััธ ";
  LOCATE 6, 3: PRINT " ณณ High Scores Hall of Fame ณณ ";
  LOCATE 7, 3: PRINT " ฦุัออออออออออออออออออออออออออออัุต ";
  LOCATE 8, 3: PRINT " ณณ Score Level  Player Name   ณณ ";
  LOCATE 9, 3: PRINT " ฦุอออออออัออออัอออออออออออออออออุต ";
  FOR X = 1 TO 11
    LOCATE X + 9, 3
    PRINT " ณณ       ณ    ณ                 ณณ ";
  NEXT
  LOCATE 21, 3: PRINT " ิฯอออออออฯออออฯอออออออออออออออออฯพ ";
  LOCATE 22, 3: PRINT "                                    ";
  '_|_|_|   Print high scores display framework
  FOR X = 0 TO 9
  '_|_|_|   For each high scores array element
    COLOR 12: IF X = L THEN COLOR 14
    '_|_|_|   Set text color. If new high score, set to highlight
    LOCATE X + 10, 6: PRINT STR$(HiScor(X).s);
    '_|_|_|   Print high score score data at specified location
    LOCATE X + 10, 14: PRINT STR$(HiScor(X).L);
    '_|_|_|   Print high score level data at specified location
    LOCATE X + 10, 19: PRINT HiScor(X).n;
    '_|_|_|   Print high score name data at specified location
  NEXT
  '_|_|_|   Next array element
  DO: LOOP WHILE INKEY$ > ""
  '_|_|_|   Clear keyboard buffer
  DO: LOOP WHILE INKEY$ = ""
  '_|_|_|   Wait for key
END SUB

SUB MakeBackground (spr() AS STRING, d)
  CLS
  '_|_|_|   Start with a clear screen
  FOR Y% = 11 TO 15
  '_|_|_|   For each string
    FOR X% = 1 TO 20
    '_|_|_|   For each character in string
      SELECT CASE MID$(spr(Y%), X%, 1)
      '_|_|_|   Convert to pixel data by character data
        CASE "a"
        '_|_|_|   If character data is 'a'
          MID$(spr(Y%), X%, 1) = CHR$(33)
          '_|_|_|   set pixel data for star
        CASE "o"
        '_|_|_|   If character data is 'o'
          MID$(spr(Y%), X%, 1) = CHR$(35)
          '_|_|_|   set pixel data for star
        CASE "c"
        '_|_|_|   If character data is 'c'
          MID$(spr(Y%), X%, 1) = CHR$(37)
          '_|_|_|   set pixel data for star
        CASE "w"
        '_|_|_|   If character data is 'w'
          MID$(spr(Y%), X%, 1) = CHR$(10)
          '_|_|_|   set pixel data for light green
        CASE "s"
        '_|_|_|   If character data is 's'
          MID$(spr(Y%), X%, 1) = CHR$(2)
          '_|_|_|   set pixel data for dark green
        CASE ELSE
        '_|_|_|   If character data is anything else
          MID$(spr(Y%), X%, 1) = CHR$(0)
          '_|_|_|   set pixel data for transparence
      END SELECT
      '_|_|_|   Data conversion complete
      PSET (X% - 1, Y% - 11), ASC(MID$(spr(Y%), X%))
      '_|_|_|   Display pixel data on screen
    NEXT
    '_|_|_|   Next string element
  NEXT
  '_|_|_|   Next string
  GET (0, 0)-(19, 4), Ground(0, d)
  '_|_|_|   Save pixel data to array for PUT
END SUB

FUNCTION Menu% (M)
  CLS
  '_|_|_|   Start with a nice clean screen
  DIM Mnu(0 TO 3) AS STRING
  '_|_|_|   Define an array to hold display data
  COLOR 13: LOCATE 6, 5: PRINT "Quik Basic Lander Main Menu"
  LINE (28, 36)-(251, 50), 15, B
  LINE (29, 37)-(250, 49), 7, B
  LINE (251, 37)-(251, 50), 8
  LINE (28, 50)-(250, 50), 8
  FOR X = 1 TO 10
    LINE (65 + X, 58 + X)-(213 - X, 142 - X), 20 + X, B
  NEXT
  '_|_|_|   Prepare display background
  Mnu(0) = "Play Game"
  Mnu(1) = "High Scores"
  Mnu(2) = "About QB Lander"
  Mnu(3) = "Quit QB Lander"
  '_|_|_|   Load menu selection array
  DO
  '_|_|_|   Initialize input loop
    FOR X = 0 TO 3
    '_|_|_|   For each menu choice
      LOCATE X * 2 + 10, 11
      '_|_|_|   Place cursor
      COLOR 9: IF X = M THEN COLOR 14
      '_|_|_|   Set color, or highlight color for present choice
      PRINT Mnu(X);
      '_|_|_|   Print menu item
    NEXT
    '_|_|_|   Next array element
    DO
    '_|_|_|   Initialize keypress processing loop
      DO
      '_|_|_|   Initialize keypress loop
        k$ = INKEY$
        '_|_|_|   Get keypress from queue
      LOOP WHILE k$ = ""
      '_|_|_|   If no key is pressed, continue loop
      k = ASC(k$)
      '_|_|_|   Get ascii value of key pressed
      IF k = 0 THEN k = -(ASC(MID$(k$, 2)))
      '_|_|_|   If it is an extended key, use negative value
      SELECT CASE k
      '_|_|_|   Act on keypress data
        CASE -72, -75
        '_|_|_|   If up arrow or left arrow was pressed
          M = (M + 3) AND 3
          '_|_|_|   decrement menu choice
          EXIT DO
          '_|_|_|   Exit keypress loop
        CASE -77, -80
        '_|_|_|   If down arrow or right arrow was pressed
          M = (M + 1) AND 3
          '_|_|_|   increment menu choice
          EXIT DO
          '_|_|_|   Exit keypress loop
        CASE 13, 32
        '_|_|_|   If ENTER or SPACE was pressed
          Menu% = M
          '_|_|_|   Set function to return menu choice
          EXIT FUNCTION
          '_|_|_|   Exit function
        CASE 27
        '_|_|_|  If ESCape key is pressed
          Menu% = 3
          '_|_|_|   Set function to return QUIT choice
          EXIT FUNCTION
          '_|_|_|   Exit function
      END SELECT
      '_|_|_|   End key processing
    LOOP
    '_|_|_|   Continue keypress loop
  LOOP
  '_|_|_|   Continue menu loop
END FUNCTION

FUNCTION PlayLevel% (L)
  SHARED Fuel, Bottom, Collision
  SHARED Docked, Passenger, Passengers, PadWidth, Bonus
  SHARED ThrustUp, ThrustDown, ThrustLeft, ThrustRight
  '_|_|_|   This is the main meat of the game, where all the
  '_|_|_|   actual gameplay takes place.  It is able to share
  '_|_|_|   all of its data with other subs.
  L = L + 1
  '_|_|_|   Increment player level, which begins at zero
  SetupForLevel L
  '_|_|_|   Setup screen and intialize data for each level
  CollideMsg$ = "Asteroid collision!!"
  '_|_|_|   Default game message is for an asteroid collision
  DO
  '_|_|_|   Initialize level play loop
    k$ = INKEY$
    '_|_|_|   Get keypress from queue.  We won't use this, but
    '_|_|_|   we want to keep the queue cleaned out
    RotatePalette
    '_|_|_|   Make the stars sparkle!
    IF Docked > 0 THEN Shipdocked L
    '_|_|_|   If the ship is docked, go wait for a launch
    Bonus = Bonus + (Bonus > 0)
    '_|_|_|   Decrement bonus value, unless it is already zero
    '_|_|_|   If bonus is greater than, then (Bonus > 0) = -1
    SELECT CASE INP(&H60)
    '_|_|_|   Read and act upon keyboard hardware interface
      CASE 1
      '_|_|_|   If ESCape key is pressed
        PlayLevel% = 1
        '_|_|_|   Set function to return 'Game Over' notice
        EXIT FUNCTION
        '_|_|_|   Exit play function
      CASE 75
      '_|_|_|   If left arrow key is pressed
        Ship.d = Thrust%(ThrustLeft, ThrustRight, 1, 10, 1)
        '_|_|_|   Define sprite by Applying left thrust to ship
      CASE 77
      '_|_|_|   If right arrow key is pressed
        Ship.d = Thrust%(ThrustRight, ThrustLeft, 1, 10, 2)
        '_|_|_|   Define sprite by Applying right thrust to ship
      CASE 80
      '_|_|_|   If down arrow key is pressed
        Ship.d = Thrust%(ThrustDown, ThrustUp, 4, 60, 3)
        '_|_|_|   Define sprite by Applying downward thrust to ship
      CASE ELSE
      '_|_|_|   If none of the command keys is pressed
        Ship.d = 0
        '_|_|_|   Define sprite as ship without thrust
        Falling
        '_|_|_|   Apply gravity to ship
    END SELECT
    '_|_|_|   Keypress has been processed
    Ship.X = Ship.X + (ThrustLeft - ThrustRight) \ 4
    '_|_|_|   Calculate lateral thrust forces on ship
    Speed = ThrustUp - ThrustDown
    Ship.Y = Ship.Y + Speed \ 20
    '_|_|_|   Calculate vertical thrust forces on ship
    IF Ship.Y > Bottom THEN Ship.Y = Bottom
    '_|_|_|   Test for display bottom
    IF Ship.Y < 0 THEN Ship.Y = 0
    '_|_|_|   Test for display top
    IF Ship.X > 292 THEN Ship.X = 292
    '_|_|_|   Test for display right boundary
    IF Ship.X < 0 THEN Ship.X = 0
    '_|_|_|   Test for display left boundary
    UpdateDisplay
    '_|_|_|   Update the screen display
    IF Collision > 0 THEN
    '_|_|_|   If a collision state exists
      FOR X = 1 TO 50
        FOR Y = 1 TO 50
          C = RND * 200 + 10
          a! = RND: a! = a! * 7!
          r = RND * 20 + 5
          CIRCLE (Ship.X + 14, Ship.Y + 14), r, C, , , a!
        NEXT
        SOUND 73 + RND * 20, .7
      NEXT
      '_|_|_|   Make a little explosion display, using the Circle
      '_|_|_|   command with variable aspect ratios and diameters
      GameMessage CollideMsg$
      '_|_|_|   Display game message
      PlayLevel% = 1
      '_|_|_|   Set function to return 'Game Over' notice
      EXIT FUNCTION
      '_|_|_|   Exit function
    END IF
    IF Ship.Y = Bottom THEN
    '_|_|_|   If the ship has reached the surface, then
      ThrustUp = 0
      '_|_|_|   Negate gravitational effect
      ThrustLeft = 0
      '_|_|_|   Negate left thrust effect
      ThrustRight = 0
      '_|_|_|   Negate right thrust effect
      IF (Ship.X < Pad.X) OR (Ship.X > Pad.X + Pad.W - Ship.W) THEN
      '_|_|_|   If the ship is not on the pad, then
        CollideMsg$ = "Crashed into surface!!"
        '_|_|_|   Set message to notify of surface collision
        Collision = 1
        '_|_|_|   Set collision condition variable
      ELSE
      '_|_|_|   If the ship is on the pad, then
        IF Speed > 50 THEN
        '_|_|_|   If the ship is falling too quickly, then
          Collision = 1
          '_|_|_|   Set collision condition variable
          CollideMsg$ = "Crash landed!!"
          '_|_|_|   Set message to notify of crash landing
        ELSE
        '_|_|_|   If the ship has not crash landed
          IF Passenger = 0 THEN
          '_|_|_|   If there is not already a passenger, then
            Passenger = 1
            '_|_|_|   Pick up a passenger
            ThrustUp = 0
            '_|_|_|   Negate gravity
            ThrustLeft = 0
            '_|_|_|   Negate left thrust
            ThrustRight = 0
            '_|_|_|   Negate right thrust
            ThrustDown = 0
            '_|_|_|   Negate downward thrust
            GameMessage "One survivor rescued!"
            '_|_|_|   Set message to notify of survivor rescue
          END IF
          '_|_|_|   End passenger test
        END IF
        '_|_|_|   End crash test
      END IF
      '_|_|_|   End pad test
    END IF
    '_|_|_|   End ship touched down test
    IF (Ship.Y = 0) AND (Passenger > 0) THEN
    '_|_|_|   If the ship is able to dock, then
      Passengers = Passengers - 1
      '_|_|_|   Decrement number of survivors to be rescued
      Passenger = 0
      '_|_|_|   Remove passenger from ship
      Docked = 1
      '_|_|_|   Set ship as docked
      Score = Score + 100 * L
      '_|_|_|   Add one hundred times the level to score
      IF Passengers = 0 THEN
      '_|_|_|   If all the passengers have been rescued, then
        FOR X = (Bonus \ 30) TO 0 STEP -1
          COLOR 12
          LOCATE 25, 19
          PRINT " B:" + RIGHT$(" " + STR$(X \ 3), 3); " ";
          LOCATE 1, 13: COLOR 10
          Score = Score + 10
          PRINT " Score:"; RIGHT$("     " + STR$(Score) + " ", 8)
          SOUND 500, .1
          WAIT &H3DA, 8
          WAIT &H3DA, 8, 8
        NEXT
        '_|_|_|   Add score points for remaining bonus points
        FOR X = (Fuel \ 10) TO 0 STEP -1
          COLOR 12
          LOCATE 25, 3
          PRINT " F:"; RIGHT$(" " + STR$(X * 10), 3); " ";
          LOCATE 1, 13
          COLOR 10
          Score = Score + 10
          PRINT " Score:"; RIGHT$("     " + STR$(Score) + " ", 8)
          SOUND 999, .1
          WAIT &H3DA, 8
          WAIT &H3DA, 8, 8
        NEXT
        '_|_|_|   Add score points for remaining Fuel points
        GameMessage "Level" + STR$(L) + " completed."
        '_|_|_|   Display level completed message
        EXIT FUNCTION
        '_|_|_|   Exit function without setting it to return
        '_|_|_|   the 'Game Over' notification
      END IF
      '_|_|_|   End last passenger test
    END IF
    '_|_|_|   End able to dock test
    UpdateStats L
    '_|_|_|   Show game stats on screen borders
  LOOP
  '_|_|_|   Continue level play loop
END FUNCTION

SUB RotatePalette
  STATIC colour
  '_|_|_|   We'll use this to know which color to change,
  '_|_|_|   changing one of the sixteen on each call to this sub
  colour = (colour + 1) AND 15
  '_|_|_|   Increment and/or rotate the variable value
  OUT &H3C8, colour + 32
  '_|_|_|   Set up harware to receive color data for palette
  OUT &H3C9, ((RND * 32767) MOD 48) + 15
  '_|_|_|   Output red data to video card
  OUT &H3C9, ((RND * 32767) MOD 48) + 15
  '_|_|_|   Output green data to video card
  OUT &H3C9, ((RND * 32767) MOD 48) + 15
  '_|_|_|   Output blue data to video card
END SUB

SUB SetupBackground
  CLS
  '_|_|_|   Start with a nice clean screen
  DefineBackground
  '_|_|_|   Get the ground data read into arrays
  ATop = 10
  '_|_|_|   Set top of display area
  ABot = 189
  '_|_|_|   Set bottom of display area
  ALeft = 10
  '_|_|_|   Set left edge of display area
  ARight = 309
  '_|_|_|   Set right edge of display area
  LINE (10, 10)-(309, 17), 182, BF
  '_|_|_|   Create docking area strip at top of play area
  FOR t = 1 TO 200
    WAIT &H3DA, 1
    WAIT &H3DA, 1, 1
    PSET (RND * 319, RND * 199), RND * 15 + 32
  NEXT
  '_|_|_|   Randomly place two hundred 'stars' in the sky
  FOR X = 1 TO 10
    L = ALeft - X
    t = ATop - X
    r = ARight + X
    B = ABot + X
    LINE (L, t)-(r, B), X + 21, B
  NEXT
  '_|_|_|   Use grayscale boxes to make playfield border
  FOR X = 10 TO 290 STEP 20
    PUT (X, 185), Ground(0, RND * 32767 MOD 5), PSET
  NEXT
  '_|_|_|   Randomly place planetary surface data on screen
  GET (10, 10)-(309, 189), BackupDrop
  '_|_|_|   Store screen data in an array
END SUB

SUB SetupForLevel (L)
  SHARED Fuel, Bottom
  SHARED Docked, Passenger, Passengers, Bonus
  '_|_|_|   We will need to initialize these variables
  Fuel = Fuel + 400
  '_|_|_|   Add four hundred fuel for each level played
  IF Fuel > 999 THEN Fuel = 999
  '_|_|_|   Fuel maxes out at nine hundred ninety-nine
  Passengers = 3 + L \ 2
  '_|_|_|   Increase survivors by one every two levels
  SetupBackground
  '_|_|_|   Make the background screen for game display
  Docked = 1
  '_|_|_|   Set ship as docked
  Bonus = 3000
  '_|_|_|   Set bonus at maximum
  Ship.X = 146
  '_|_|_|   Place ship in center of play area
  Ship.Y = 0
  '_|_|_|   Place ship in docking strip
  Ship.H = 8
  '_|_|_|   Define ship as being 8 pixels high
  Ship.W = 8
  '_|_|_|   Define ship as being 8 pixels wide
  Ship.d = 0
  '_|_|_|   Define ship sprite as non-thrusting
  Bottom = 171
  '_|_|_|   Define bottom of playfield
END SUB

SUB SetupRocks
  DefineRock
  '_|_|_|   Get rocks read into rock data arrays
  FOR r = 0 TO 4
  '_|_|_|   For each rock
    FOR L = 0 TO 3
    '_|_|_|   For each level of rocks
      Rock(r, L).d = (RND * 32767) MOD 28
      '_|_|_|   Randomly select rock sprite data
      Rock(r, L).W = 13
      '_|_|_|   Define rock as being 13 pixels wide
      Rock(r, L).r = (RND * 32767) AND 1
      '_|_|_|   Randomly select rotational direction of rock
      Rock(r, L).H = 13
      '_|_|_|   Define rock as being 13 pixels high
      Rock(r, L).X = ((RND * 32767) AND 31) * 12
      '_|_|_|   Randomly select roch horizontal position
      Rock(r, L).Y = L * 32 + 35
      '_|_|_|   Select rock vertical position by rock level
    NEXT
    '_|_|_|   Next level
  NEXT
  '_|_|_|   Next rock
END SUB

SUB Shipdocked (L)
  SHARED Docked, Fuel, Bonus, Passenger, Passengers, Collision
  SHARED ThrustUp, ThrustLeft, ThrustRight, ThrustDown, Playspeed%
  '_|_|_|   We'll need to update these variables
  ThrustUp = 0
  '_|_|_|   Negate gravity
  ThrustLeft = 0
  '_|_|_|   Negate left thrust
  ThrustRight = 0
  '_|_|_|   Negate right thrust
  ThrustDown = 0
  '_|_|_|   Negate downward thrust
  DefinePad L
  '_|_|_|   Define a new landing pad for this next rescue run
  GameMessage "Docked: Up Arrow Launches"
  '_|_|_|   Notify player that the ship is docked
  Ship.d = 0
  '_|_|_|   Select non-thrusting ship sprite data
  Collision = 0
  '_|_|_|   Zero collision condition variable
  Passenger = 0
  '_|_|_|   Zero passenger condition variable
  DO
  '_|_|_|   Initialize input routine while waiting for launch
    k$ = INKEY$
    '_|_|_|   Get a key from keyboard queue
    RotatePalette
    '_|_|_|   Make the stars sparkle
    UpdateDisplay
    '_|_|_|   Keep the rocks rolling
    SELECT CASE INP(&H60)
    '_|_|_|   Act on direct keyboard reading data
      CASE 75
      '_|_|_|   If the left arrow key is pressed
        Ship.X = Ship.X - 8
        '_|_|_|   move the ship left by eight pixels
        IF Ship.X < 0 THEN Ship.X = 0
        '_|_|_|   test for left edge of playfield
      CASE 77
      '_|_|_|   If the right arrow key is pressed
        Ship.X = Ship.X + 8
        '_|_|_|   move the ship right by eight pixels
        IF Ship.X > 292 THEN Ship.X = 292
        '_|_|_|   test for right edge of playfield
      CASE 31, 54
        Playspeed% = (Playspeed% + 1) MOD 3
        LOCATE 4, 10: PRINT "Delay ="; Playspeed%; ": Hit Space";
        WHILE INKEY$ <> " ": WEND
      CASE 1
      '_|_|_|   If the ESCape key is pressed
        EXIT SUB
        '_|_|_|   Exit the sub: ESC key should still be pressed
        '_|_|_|   when tested in PlayLevel, exiting game
      CASE 72
      '_|_|_|   If the up arrow key is pressed
        Docked = 0
        '_|_|_|   Clear the ship docked variable
        Ship.Y = 8
        '_|_|_|   Drop the ship out of the docking strip
        EXIT SUB
        '_|_|_|   Launch!   Exit the sub
    END SELECT
    '_|_|_|   Keypress has been processed
    UpdateStats L
    '_|_|_|   Display game status in screen borders
  LOOP
  '_|_|_|   Continue input routine waiting for launch
END SUB

FUNCTION Thrust% (inc, dec, res, lim, Sprite)
'_|_|_|   We need to know which element to increment, which to
'_|_|_|   decrement, how many increments by which they should be
'_|_|_|   altered, what the limit of change is, and which sprite
'_|_|_|   we will be using if thrust is possible
  SHARED Fuel
  '_|_|_|   We will need to test and alter Fuel supplies here
  Falling
  '_|_|_|   First, we will take gravity into account
  IF Fuel > 0 THEN
  '_|_|_|   If there is fuel for the thrusters, then
    inc = inc - res * (inc < lim)
    '_|_|_|   increment selected thrust
    dec = dec + res * (dec > 0)
    '_|_|_|   decrement reverse thrust
    Thrust% = Sprite
    '_|_|_|   set function to return sprite data value
    '_|_|_|   if not set, it will be zero, the nonthrust sprite
    Fuel = Fuel - 1
    '_|_|_|   expend one unit of fuel
  END IF
  '_|_|_|   You may have noticed that the main thruster performs
  '_|_|_|   much better than the lateral thrusters, yielding four
  '_|_|_|   times the performance when used, per fuel spent.
  '_|_|_|   This is one of the keys to piloting for maximum fuel
  '_|_|_|   conservation.  A feather touch on the thrusters, and
  '_|_|_|   the main thruster should be the first choice.
END FUNCTION

SUB UpdateDisplay
  SHARED Collision, Playspeed%
  '_|_|_|   Here is where we will test for asteroid collisions
  FOR Y = 0 TO 27001
    BackDrop(Y) = BackupDrop(Y)
  NEXT
  '_|_|_|   Copy the background data into the work area from
  '_|_|_|   the screen data stored when the level was set up
  BSeg& = VARSEG(BackDrop(0))
  '_|_|_|   Define memory segment as that for the work area array
  BPtr& = VARPTR(BackDrop(2))
  '_|_|_|   Define a pointer value for the work area array
  FOR Layer = 0 TO 3
  '_|_|_|   For each layer of rocks
    Toss$ = INKEY$
    '_|_|_|   Grab a keypress and toss it to keep the keyboard
    '_|_|_|   queue from overflowing
    FOR Stone = 0 TO 4
    '_|_|_|   For each rock in each level
      Rot = Rock(Stone, Layer).r
      '_|_|_|   Get the direction of rotation
      IF Rot = 0 THEN Rot = 27
      '_|_|_|   Adjust rotation data for our rock array
      Rock(Stone, Layer).d = (Rock(Stone, Layer).d + Rot) MOD 28
      '_|_|_|   Roll the stone
      SprX& = Rock(Stone, Layer).X
      '_|_|_|   Get X position for sprite
      SprY& = Rock(Stone, Layer).Y
      '_|_|_|   Get Y position for sprite
      SprH& = Rock(Stone, Layer).H
      '_|_|_|   Get sprite height
      SprW& = Rock(Stone, Layer).W
      '_|_|_|   Get sprite width
      SprX& = SprX& + Layer \ 2 + 1
      '_|_|_|   Move sprite to the right.  Lower levels move faster
      IF SprX& > 310 THEN
      '_|_|_|   If the rock has rolled off the screen, then
        SprX& = -10 * (((RND * 32767) MOD 5) + 1)
        '_|_|_|   Randomly place rock offscreen to the left
        Rock(Stone, Layer).r = (RND * 32767) AND 1
        '_|_|_|   Randomly select direction of rock rotation
      END IF
      '_|_|_|   End test for rock rolling off screen
      Rock(Stone, Layer).X = SprX&
      '_|_|_|   Set new sprite X position data
      SprLen& = SprW& * SprH& - 1
      '_|_|_|   Get length of sprite data string
      FOR X& = 0 TO SprLen&
      '_|_|_|   For the length of the sprite data string
        SBX& = SprX& + (X& MOD SprW&)
        '_|_|_|   Get the X position for the pixel
        IF (SBX& > -1) AND (SBX& < 300) THEN
        '_|_|_|   If the pixel is inside the playfield, then
          P = ASC(MID$(RockDef(Rock(Stone, Layer).d), X& + 1))
          '_|_|_|   Get the ascii value of the pixel data character
          IF P > 0 THEN
          '_|_|_|   If the pixel is not transparent, then
            DEF SEG = BSeg&
            '_|_|_|   Define memory segment as work area array
            Ptr& = (SprY& + X& \ SprW&) * 300
            '_|_|_|   Calculate pixel Y position
            Ptr& = Ptr& + SBX& + BPtr&
            '_|_|_|   Calculate pixel X position
            POKE Ptr&, P
            '_|_|_|   Place pixel into work area array
          END IF
          '_|_|_|   End transparence test
        END IF
        '_|_|_|   End playfield boundary test
      NEXT
      '_|_|_|   Next pixel
    NEXT
    '_|_|_|   Next rock
  NEXT
  '_|_|_|   Next level of rocks
  SprX& = Pad.X
  '_|_|_|   Get pad X position
  SprY& = Pad.Y
  '_|_|_|   Get pad Y position
  SprH& = Pad.H
  '_|_|_|   Get pad height
  SprW& = Pad.W
  '_|_|_|   Get pad width
  SprLen& = SprW& * SprH& - 1
  '_|_|_|   Calculate length of pad sprite data string
  FOR X& = 0 TO SprLen&
  '_|_|_|   For the length of the data string
    SBX& = SprX& + (X& MOD SprW&)
    '_|_|_|   Calculate pixel X position
    P = ASC(MID$(PadDef, X& + 1))
    '_|_|_|   Get the ascii value of the pixel data character
    IF P > 0 THEN
    '_|_|_|   If the pixel is not transparent
      DEF SEG = BSeg&
      '_|_|_|   Define memory segment as the work area array
      Ptr& = (SprY& + X& \ SprW&) * 300
      '_|_|_|   Calculate pixel Y position
      Ptr& = Ptr& + SBX& + BPtr&
      '_|_|_|   Calculate pixel X position
      POKE Ptr&, P
      '_|_|_|   Place pixel into work area array
    END IF
    '_|_|_|   End test for transparence
  NEXT
  '_|_|_|   Next pixel
  SprX& = Ship.X
  '_|_|_|   Get ship X position
  SprY& = Ship.Y
  '_|_|_|   Get ship Y position
  SprH& = Ship.H
  '_|_|_|   Get ship height
  SprW& = Ship.W
  '_|_|_|   Get ship width
  SprLen& = SprW& * SprH& - 1
  '_|_|_|   Calculate length of ship sprite data string
  FOR X& = 0 TO SprLen&
  '_|_|_|   For every element of the string
    SBX& = SprX& + (X& MOD SprW&)
    '_|_|_|   Calculate the X value of the pixel
    P = ASC(MID$(ShipDef(Ship.d), X& + 1))
    '_|_|_|   Get the ascii value of the pixel data
    IF P > 0 THEN
    '_|_|_|   If the pixel is not transparent, then
      DEF SEG = BSeg&
      '_|_|_|   Define memory segment for work area array
      Ptr& = (SprY& + X& \ SprW&) * 300
      '_|_|_|   Calculate pixel Y position
      Ptr& = Ptr& + SBX& + BPtr&
      '_|_|_|   Calculate pixel X position
      C = PEEK(Ptr&)
      '_|_|_|   Get pixel already at that position
      IF (C > 100) AND (C < 200) AND (Ship.Y > 10) THEN Collision = 1
      '_|_|_|   If that pixel indicates the presence of a rock
      '_|_|_|   at that location, then set collision variable
      POKE Ptr&, P
      '_|_|_|   Place pixel data in work area array
    END IF
    '_|_|_|   End transparency test
  NEXT
  '_|_|_|   Next pixel
  wt% = Playspeed%
  DO
    WAIT &H3DA, 8
    '_|_|_|   Wait for vertical retrace to begin
    WAIT &H3DA, 8, 8
    '_|_|_|   Wait for end of vertical retrace
    IF wt% = 0 THEN EXIT DO
    wt% = wt% - 1
  LOOP
  PUT (10, 10), BackDrop, PSET
  '_|_|_|   PUT updated work area data onto screen
END SUB

SUB UpdateStats (L)
  SHARED Fuel, Bonus, Passengers, Bottom
  '_|_|_|   We will need these to display them properly
  COLOR 12
  LOCATE 25, 3
  '_|_|_|   Set color and location for text output
  PRINT " F:"; RIGHT$(" " + STR$(Fuel), 3); " ";
  LOCATE , 11: PRINT " A:";
  PRINT RIGHT$(" " + STR$(Bottom - Ship.Y), 3); " ";
  LOCATE , 19
  PRINT " B:" + RIGHT$(" " + STR$(Bonus \ 3), 3); " ";
  LOCATE , 27
  PRINT " L:" + RIGHT$(STR$(L), 2); " ";
  PRINT " P:" + RIGHT$(STR$(Passengers), 2); " ";
  '_|_|_|   Print game stats along bottom of screen
  LOCATE 1, 13
  COLOR 10
  '_|_|_|   Set color and location for text output
  PRINT " Score:"; RIGHT$("     " + STR$(Score) + " ", 8)
  '_|_|_|   Print game score along top of screen
END SUB

