'_|_|_|   BIN_RAND.BAS
'_|_|_|   A program to illustrate how RANDOM and BINARY file
'_|_|_|   modes are similar, and their use for databases.
'_|_|_|   No warrantee or guarantee given or implied.
'_|_|_|   PUBLIC DOMAIN   by Kurt Kuzba.  (7/7/1997)
DECLARE SUB ShowfileRandom (c%)
DECLARE SUB ShowFileBinary (c%)
DECLARE SUB ShowPlayer (P AS ANY)
DECLARE FUNCTION Weapon$ (w$)
DECLARE SUB KeyWait ()
TYPE gameCharacter
   Pname AS STRING * 21: hits AS INTEGER: strength AS INTEGER
   gold AS LONG: experience AS INTEGER: Weapon AS STRING * 1
END TYPE
'_|_|_|   TYPE gameCharacter requires 32 bytes. Remember this!
DIM Player AS gameCharacter
'_|_|_|   We will write three instances to our file, in BINARY
save% = FREEFILE: OPEN "players.sav" FOR BINARY AS #save%
Player.Pname = "Nobody One": PUT #save%, 32 * 1 - 31, Player
Player.Pname = "Nobody Two": PUT #save%, 32 * 2 - 31, Player
Player.Pname = "Nobody Three": PUT #save%, 32 * 3 - 31, Player
CLOSE save%
'_|_|_|   Now we may read them using the BINARY method
ShowFileBinary 11
'_|_|_|   Now we will edit player 3 using BINARY methods
save% = FREEFILE: OPEN "players.sav" FOR BINARY AS #save%
GET #save%, 3 * 32 - 31, Player
Player.Pname = "Kurt Kuzba": Player.hits = 100: Player.gold = 1000
Player.strength = 100: Player.experience = 1000: Player.Weapon = "1"
PUT #save%, 3 * 32 - 31, Player
CLOSE save%: ShowFileBinary 14
'_|_|_|  Now we will demonstrate loading and saving in RANDOM
save% = FREEFILE: OPEN "players.sav" FOR RANDOM AS #save% LEN = 32
GET #save%, 2, Player
Player.Pname = "Jerimiah Short": Player.hits = 99: Player.gold = 999
Player.strength = 99: Player.experience = 999: Player.Weapon = "2"
PUT #save%, 2, Player
CLOSE save%: ShowfileRandom 10
SUB KeyWait : DO: LOOP UNTIL INKEY$ > "": END SUB
SUB ShowFileBinary (c%)
   DIM PD(1 TO 3) AS gameCharacter
   save% = FREEFILE: OPEN "players.sav" FOR BINARY AS #save%
   FOR P% = 1 TO 3: GET #save%, P% * 32 - 31, PD(P%): NEXT
   CLOSE save%: COLOR c%, 0: CLS
   FOR P% = 1 TO 3: ShowPlayer PD(P%): PRINT : NEXT: KeyWait
END SUB
SUB ShowfileRandom (c%)
   DIM PD(1 TO 3) AS gameCharacter
   save% = FREEFILE: OPEN "players.sav" FOR RANDOM AS #save% LEN = 32
   FOR P% = 1 TO 3: GET #save%, P%, PD(P%): NEXT
   CLOSE save%: COLOR c%, 0: CLS
   FOR P% = 1 TO 3: ShowPlayer PD(P%): PRINT : NEXT: KeyWait
END SUB
SUB ShowPlayer (P AS gameCharacter)
   PRINT "Name       :"; P.Pname
   PRINT "Hits       :"; MID$(STR$(P.hits), 2); "%"
   PRINT "Strength   :"; MID$(STR$(P.strength), 2); "%"
   PRINT "Gold       :"; MID$(STR$(P.gold), 2)
   PRINT "Experience :"; MID$(STR$(P.experience), 2)
   PRINT "Weapon     :"; Weapon$(P.Weapon)
END SUB
FUNCTION Weapon$ (w$)
   SELECT CASE w$
      CASE "1": Weapon$ = "A Stick"
      CASE "2": Weapon$ = "A Knife"
      CASE "3": Weapon$ = "A Shortsword"
      CASE "4": Weapon$ = "A Longsword"
      CASE "5": Weapon$ = "A Greatsword"
      CASE ELSE: Weapon$ = "Bare Hands"
   END SELECT
END FUNCTION
