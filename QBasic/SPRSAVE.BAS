'_|_|_|   SPRSAVE.BAS   PUBLIC DOMAIN   Kurt Kuzba (3/3/1999)
'_|_|_|   Example of making and saving sprites for later use.
DECLARE SUB SpriteLoad (s%())
DECLARE SUB SpriteSave (s%())
DECLARE SUB MakeSprite ()
DECLARE SUB SaveSprite ()
ON ERROR GOTO OOps
   'set screen mode, dim array, display data
SCREEN 13
REDIM Sprite(130) AS INTEGER
MakeSprite
   'get data, save data to file: done: prompt for next step
GET (0, 0)-(15, 15), Sprite
SpriteSave Sprite()
COLOR 11
LOCATE 12, 1
PRINT "This is the original sprite."
PRINT "I have saved it to disk."
PRINT "Hit any key to continue"
WHILE INKEY$ = "": WEND
   'redim array to clear it, clear screen, get empty sprite
REDIM Sprite(130)
CLS
   'Since I'm using PUT, I need the first two integers for the
   'sprite size in my array, so I read in a blank sprite and
   'then load in the actual sprite data from disk. If I were
   'using my own blit routine, I would only need the disk data
GET (0, 0)-(15, 15), Sprite
   'load sprite data from file, display sprite: prompt to end
SpriteLoad Sprite()
PUT (60, 60), Sprite, PSET
LOCATE 20, 1
PRINT "This is a copy of the sprite."
PRINT "I have loaded it from disk."
PRINT "Hit any key to continue"
WHILE INKEY$ = "": WEND
   'reset video mode and screen parameters, exit program
SCREEN 0: WIDTH 80, 25: SYSTEM
OOps: FError$ = STR$(ERR): RESUME NEXT

SUB MakeSprite
   s$ = ""
   s$ = s$ + "   oooooooooo   "
   s$ = s$ + " oooooooooooooo "
   s$ = s$ + "oooooo    oooooo"
   s$ = s$ + "oooo   xx   oooo"
   s$ = s$ + "oo   xxxxxx   oo"
   s$ = s$ + "oo xxxx  xxxx oo"
   s$ = s$ + "oo xxx    xxx oo"
   s$ = s$ + "oo xx      xx oo"
   s$ = s$ + "oo xx      xx oo"
   s$ = s$ + "oo xxx    xxx oo"
   s$ = s$ + "oo xxxx  xxxx oo"
   s$ = s$ + "oo   xxxxxx   oo"
   s$ = s$ + "oooo   xx   oooo"
   s$ = s$ + "oooooo    oooooo"
   s$ = s$ + " oooooooooooooo "
   s$ = s$ + "   oooooooooo   "
   l% = LEN(s$)
   DEF SEG = &HA000
   FOR x% = 0 TO 15
      FOR y% = 0 TO 15
         ch$ = MID$(s$, x% * 16 + y% + 1)
         SELECT CASE ASC(ch$)
            CASE ASC(" "): POKE x% * 320 + y%, 0
            CASE ASC("x"): POKE x% * 320 + y%, 33
            CASE ASC("o"): POKE x% * 320 + y%, 125
         END SELECT
      NEXT
   NEXT
END SUB

SUB SpriteLoad (s%())
   'If I were using my own blit routine, I would redim the
   'array at this point, using the Length Of File as the
   'variable to determine array size I might also use an
   'array of type STRING
   'bytes% = LOF(Spr%)
   'REDIM s as STRING * bytes
   's = INPUT$(bytes%, Spr%)    <- either of these is good
   'GET #Spr%, 1, s             <- They do the same thing
   'at any rate, you should get the idea now
   Spr% = FREEFILE
   OPEN "sprtest.dat" FOR RANDOM AS Spr% LEN = 2
   bytes% = LOF(Spr%) \ 2
   FOR t% = 1 TO bytes%
      GET #Spr%, t%, s%(t% + 2)
   NEXT
   CLOSE Spr%
END SUB

SUB SpriteSave (s%())
  SHARED FError$
  Spr% = FREEFILE: FError$ = "ok"
  OPEN "sprtest.dat" FOR INPUT AS #Spr%
  CLOSE Spr%
  IF FError$ = "ok" THEN KILL "sprtest.dat"
  KILL "sprtest.dat"
  OPEN "sprtest.dat" FOR RANDOM AS #Spr% LEN = 2
  FOR t% = 3 TO UBOUND(s%)
     PUT #Spr%, t% - 2, s%(t%)
  NEXT
  CLOSE Spr%
END SUB

