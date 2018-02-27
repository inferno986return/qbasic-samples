'_|_|_|   WUMPUS.BAS   PUBLIC DOMAIN   (part 1 of 3)
'From: KEN CONKLIN          Conference: QUIK_BAS -Qui (298)
'The following program can be run under QBASIC or BASICA
'   WUMPUS 1/28/77
'   I did not write this program, but altered it.
'   The date above is the date of original programming.
'_|_|_|   Further modifications to modular format, 4/9/1997
'_|_|_|   by Kurt Kuzba.
'$DYNAMIC
RANDOMIZE TIMER
SCREEN 0: COLOR 15, 4: PRINT "  WMP23-HUNT THE WUMPUS  ";
COLOR 10, 0: CR$ = CHR$(13): PRINT CR$; "INSTRUCTIONS (Y/N)?";
IF UCASE$(GetChar$(0)) = "Y" THEN instructions
DIM SHARED M(8) AS INTEGER, S(20, 3) AS INTEGER      '   SET UP CAVE
FOR J = 1 TO 20: FOR K = 1 TO 3: READ S(J, K): NEXT: NEXT
DATA 2, 5, 8, 1, 3, 10, 2, 4, 12, 3, 5, 14, 1, 4, 6, 5, 7
DATA 15, 6, 8, 17, 1, 7, 9, 8, 10, 18, 2, 9, 11, 10, 12
DATA 19, 3, 11, 13, 12, 14, 20, 4, 13, 15, 6, 14, 16, 15
DATA 17, 20, 7, 16, 18, 9, 17, 19, 11, 18, 20, 13, 16, 19
DO: PlaceArrayItems: M(8) = M(1)
   PRINT CR$; "ENTRANCE IS IN CAVE "; M(8)
   DO: Gameplay Q$: LOOP WHILE Q$ <> "Quit"
   PRINT "WANT TO TRY AGAIN? (Y/N)";
LOOP WHILE UCASE$(GetChar$(0)) = "Y": SYSTEM
SUB ArrowsLeft : SHARED A: PRINT "ARROWS LEFT:"; A: END SUB
SUB DropArrows : SHARED A: IF A = 0 THEN EXIT SUB
   J = INT(RND * A): PRINT "YOU DROPPED ALL YOUR ARROWS"
   IF J = A THEN PRINT "ALL ARROWS FOUND"
   IF J < A THEN PRINT "YOU RECOVERED"; J; "ARROWS": A = J
END SUB
SUB Gameplay (Q$) : SHARED L: IF RND > .4 THEN MoveWumpus
   IF RND < .08 THEN MoveEverything
   DO: IF J = 38 THEN L = M(5)
      DO: Go% = 0: FOR J = 1 TO 8: Go% = Go% + (L = M(J)): NEXT
         IF NOT Go% THEN EXIT DO
         IF (L = M(5)) OR (L = M(6)) THEN
            PRINT "ZAP..SUPERBAT SNATCH!": DropArrows
            DO: L = INT(1 + RND * 20)
            LOOP WHILE (L = M(1)) + (L = M(6)) + (L = M(7)) <> 0
         END IF
         IF L = M(2) THEN
            PRINT "OOPS! BUMPED A WUMPUS!"
            IF RND < .75 THEN
               : MoveWumpus: DropArrows: M(1) = L: EXIT DO
            ELSE
               Lose "TSK TSK TSK...  THE WUMPUS GOT YOU"
               Q$ = "Quit": EXIT SUB
            END IF
         END IF
         IF (L = M(3)) OR (L = M(4)) THEN
            Lose "YYYIIIIEEEE...FELL IN PIT": Q$ = "Quit": EXIT SUB
         END IF
         IF L = M(7) THEN
            PRINT "CAVE ENTRANCE IS BLOCKED": DropArrows: EXIT DO
         END IF
         IF L = M(8) THEN PRINT "EXIT NEARBY": M(1) = L: EXIT DO
      LOOP
      DO: Hazards: PRINT "YOU ARE IN CAVE "; M(1): Ringers
      PRINT "TUNNELS LEAD TO CAVES "; S(M(1), 1); S(M(1), 2); S(M(1), 3)
      PRINT
         PMoves% = PMoves% + 1: PRINT "[S:A:M:E:H:Q:?]> ";
         DO: K$ = UCASE$(GetChar$(0))
         LOOP WHILE INSTR("SAMEHQ?", K$) = 0: PRINT K$
         SELECT CASE K$
            CASE "S": ShootArrow
            CASE "A": ArrowsLeft
            CASE "M":
               C1% = S(M(1), 1): C2% = S(M(1), 2): C3% = S(M(1), 3)
               DO: PRINT "WHERE TO ["; C1%; MID$(STR$(C2%), 2); C3%;
               INPUT "]? =>", L
               LOOP WHILE (L <> C1%) AND (L <> C2%) AND (L <> C3%)
               M(1) = L
            CASE "E":
               IF M(1) = M(8) THEN
                  PRINT "OUT OF THE CAVES": Ratings
                  PRINT : Q$ = "Quit": EXIT SUB
               ELSE
                  PRINT "THE EXIT IS IN CAVE"; M(8)
               END IF
            CASE "H": EXIT DO
            CASE "Q": Q$ = "Quit": EXIT SUB
         END SELECT
      LOOP: Moves
   LOOP
END SUB
'_|_|_|   end   WUMPUS.BAS   PUBLIC DOMAIN   (part 1 of 3)
'_|_|_|   WUMPUS.BAS   PUBLIC DOMAIN   (part 2 of 3)
FUNCTION GetChar$ (p%) : IF p% THEN PRINT "Hit any key"
   DO: K$ = INKEY$: LOOP WHILE K$ = "": GetChar$ = K$: END FUNCTION
SUB Hazards : PRINT
   FOR K = 1 TO 3: FOR J = 2 TO 6
      IF S(M(1), K) = M(J) THEN
         SELECT CASE J
            CASE 2, 7: PRINT "I SMELL A WUMPUS"
            CASE 3, 4: PRINT "I FEEL A DRAFT"
            CASE 5, 6: PRINT "BATS NEARBY"
         END SELECT
      END IF: NEXT: NEXT: END SUB
SUB instructions
   PRINT "WELCOME TO HUNT THE WUMPUS"
   PRINT "THE WUMPUS COLONIES INHABIT A BURROW OF 20 CAVES."
   PRINT " EACH CAVE HAS THREE TUNNELS LEADING TO OTHER CAVES."
   PRINT "THE OBJECTIVE IS TO DESCEND INTO THE BURROW AND HUNT WUMPI."
   PRINT "AFTER YOU HAVE KILLED AT LEAST ONE, YOU MUST RETURN TO THE"
   PRINT "EXIT AND LEAVE THE BURROW. NOTE THAT THERE MAY BE MORE"
   PRINT "THAN ONE WUMPUS IN THE BURROW. YOU HAVE 5 ARROWS. "
   PRINT "UNDER CONDITIONS OF STRESS, YOU MAY DROP YOUR ARROWS."
   PRINT "YOUR RATING IS A FUNCTION OF THE NUMBER OF WUMPI SHOT,"
   PRINT "AND THE TIME SPENT IN THE BURROW.": PRINT : K$ = GetChar$(-1)
   PRINT "HAZARDS": PRINT : PRINT "1---BOTTOMLESS PITS"
   PRINT "TWO ROOMS CONTAIN BOTTOMLESS PITS."
   PRINT "IF YOU FALL IN A PIT, YOU LOSE."
   PRINT : PRINT : PRINT "2---SUPERBATS"
   PRINT "TWO ROOMS CONTAIN NESTS OF SUPERBATS."
   PRINT "THEY ARE PEACEFUL CREATURES WHEN LEFT ALONE. "
   PRINT "SHOULD YOU INADVERTENTLY STUMBLE ACROSS THEIR NEST,"
   PRINT "YOU WILL BE PICKED UP AND BE DEPOSITED IN ANOTHER"
   PRINT "CAVE AT RANDOM. TOO BAD IF IT CONTAINS ANOTHER HAZARD."
   PRINT : K$ = GetChar$(-1): PRINT : PRINT "3---**EARTHQUAKES**"
   PRINT "EARTHQUAKES ARE A COMMON OCCURRENCE. SHOULD ONE OCCUR,"
   PRINT "THE BURROW ENTRANCE MAY BE BLOCKED AND A NEW ONE OPEN UP."
   PRINT "THE GEOGRAPHY OF THE CAVES WILL BE CHANGED."
   PRINT "THE PITS MAY FILL UP AND A CAVE MAY BE BLOCKED BY A"
   PRINT "ROCKFALL. THE BATS WILL BE DISTURBED AND MAY SEEK OUT"
   PRINT "A NEW CAVE. THE WUMPUS WILL ALSO BECOME ALARMED AND"
   PRINT "MAY MOVE TO A NEW CAVE.": PRINT : PRINT "4---WUMPUS"
   PRINT "THE WUMPUS IS NOT BOTHERED BY ANY HAZARDS"
   PRINT "(IT HAS SUCKERS ON ITS FEET AND IS TOO BIG FOR A BAT TO LIFT)."
   PRINT "THE WUMPUS IS MOVING IN THE CAVES LOOKING FOR FOOD."
   PRINT "IT NORMALLY EATS A SPECIES OF MUSHROOM, BUT MAY EAT YOU,"
   PRINT "SHOULD YOU ENTER THE CAVE IN WHICH IT IS FEEDING."
   PRINT : K$ = GetChar$(-1): Moves
   PRINT "WARNINGS"
   PRINT "WHEN YOU ARE ONE CAVE AWAY FROM A HAZARD,"
   PRINT "YOU WILL BE WARNED AS FOLLOWS:": PRINT
   PRINT " WUMPUS..... I SMELL A WUMPUS"
   PRINT " BATS............ BATS NEARBY"
   PRINT " PIT.......... I FEEL A DRAFT": PRINT
   PRINT "IF YOU TRY TO ENTER A CAVE BLOCKED BY A FALL OR IF YOU"
   PRINT "RETURN TO THE ENTRANCE CAVE, YOU WILL BE NOTIFIED.": PRINT
END SUB
SUB Lose (T$) : PRINT T$: PRINT "HA HA HA...  YOU LOSE!": Ratings
END SUB
SUB MoveEverything : PRINT "**EARTHQUAKE**"
   FOR J = 3 TO 7: Rearrange (J): NEXT: DropArrows
   DO: M(2) = INT(RND * 20 + 1)
   LOOP WHILE (M(1) = M(2)) + (M(2) = M(7)) <> 0
   IF RND > .1 THEN EXIT SUB:  ELSE M(8) = INT(RND * 20 + 1)
   FOR J = 3 TO 7                     '   MOVE CAVE ENTRANCE
      WHILE M(8) = M(J): M(8) = INT(RND * 20 + 1): WEND: NEXT: END SUB
SUB Moves
   PRINT : PRINT "EACH TURN YOU MAY:"
   PRINT " M>  MOVE TO AN ADJACENT CAVE THROUGH A CONNECTING TUNNEL"
   PRINT " S>  SHOOT AN ARROW THROUGH A TUNNEL TO TRY AND HIT THE"
   PRINT "     WUMPUS. IT IS BIG ENOUGH SO THAT AN ARROW ENTERING"
   PRINT "     THE CAVE THAT IT IS OCCUPYING, WILL HIT IT."
   PRINT " A>  CHECK ON NUMBER OF ARROWS LEFT"
   PRINT " E>  EXIT FROM THE CAVES IF YOU ARE IN THE ENTRANCE CAVE"
   PRINT " H>  ASK FOR INSTRUCTIONS  <Help>": PRINT : K$ = GetChar$(-1)
END SUB
'_|_|_|   end   WUMPUS.BAS   PUBLIC DOMAIN   (part 2 of 3)
'_|_|_|   WUMPUS.BAS   PUBLIC DOMAIN   (part 3 of 3)
SUB MoveWumpus : SHARED W: IF W = 1 THEN M(2) = 0: EXIT SUB
   DO: M(2) = S(M(2), (INT(3 * RND + 1)))
   LOOP WHILE (M(1) = M(2)) + (L = M(2)) + (M(2) = M(7)) <> 0: END SUB
SUB PlaceArrayItems : SHARED W, F, PMoves%, A
   DIM ITM(1 TO 20) AS INTEGER: FOR J = 1 TO 20: ITM(J) = J: NEXT
   FOR J = 0 TO 100: SWAP ITM((J MOD 20) + 1), ITM(RND * 19 + 1)
   NEXT: FOR J = 1 TO 7: M(J) = ITM(J): NEXT: ERASE ITM
   W = 0: F = 0: PMoves% = 0: A = 5: END SUB
SUB Ratings : SHARED F, PMoves%: IF F > 0 THEN PRINT "GOOD HUNTING"
   R% = 0: IF PMoves% > 0 THEN R% = INT((F * 1000) / PMoves%)
   PRINT "YOUR RATING IS"; R%
   IF F < 1 THEN PRINT "BETTER LUCK NEXT TIME"
END SUB
SUB Rearrange (J AS INTEGER) : DO: M(J) = INT(RND * 20) + 1
   LOOP WHILE (M(J) = M(1)) + (M(J) = M(8)) <> 0: END SUB
SUB Ringers : SHARED A
   SELECT CASE (RND * 40)
      CASE 1, 2: PRINT "AHA! ... WUMPUS TRACKS"
      CASE 3: PRINT "AHA! ... YOU FOUND AN OLD ARROW"
         PRINT "LUCKY YOU": A = A + 1
      CASE 4, 5: PRINT "OOPS, YOU SLIPPED ON SOME LOOSE GRAVEL"
         DropArrows
      CASE 6: PRINT "OOPS, YOU JUST FELL INTO AN UNDERGROUND POOL"
         DropArrows
      CASE 7: PRINT "THIS LOOKS LIKE A NICE CAVE,"
         PRINT "WHY NOT STOP FOR LUNCH"
      CASE 8: PRINT "TAKE CARE WITH THAT FLASHLIGHT"
   END SELECT: END SUB
SUB ShootArrow : SHARED A, F, W
   IF A < 1 THEN PRINT "WHAT WITH?  DUMMY": EXIT SUB
   DO: INPUT "ENTER ZERO OR CAVE TO SHOOT INTO ", L
      IF L = 0 THEN EXIT SUB
      C1% = S(M(1), 1): C2% = S(M(1), 2): C3% = S(M(1), 3)
      IF (L = C1%) OR (L = C2%) OR (L = C3%) THEN EXIT DO
      PRINT "THAT IS NOT POSSIBLE"
   LOOP: A = A - 1: ArrowsLeft
   IF L <> M(2) THEN PRINT "MISSED": MoveWumpus: EXIT SUB
   IF RND >= .8 THEN
      PRINT "YOU WOUNDED THE WUMPUS": MoveWumpus: EXIT SUB
   END IF: PRINT "YOU GOT THE WUMPUS": F = F + 1: M(2) = 0
   IF RND < .75 THEN
      PRINT "BEWARE OF ITS MATE": J = 2: Rearrange (J)
   ELSE
      W = 1: PRINT "HEE HEE HEE...  THE WUMPUS'L GET YOU NEXT TIME"
   END IF: END SUB
'_|_|_|   end   WUMPUS.BAS   PUBLIC DOMAIN   (part 3 of 3)
