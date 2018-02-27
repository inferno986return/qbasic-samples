'_|_|_|   HISCOREX.BAS
'_|_|_|   This program illustrates a modular method for creating
'_|_|_|   and maintaining a High Scores file for a game.
'_|_|_|   released to   PUBLIC DOMAIN   by Kurt Kuzba.  (2/17/96)
CLEAR : CLS
TYPE Score
   dat AS STRING * 40
END TYPE
DIM SHARED hs(11) AS Score: HiInit (10)
HiScores "Alvin Chipmunk", 10392: HiScores "Beavis V. Bis", 15777
HiScores "Butt-Head Baud", 21543: HiScores "Rocket J. Squirrel", 7559
HiScores "Bullwinkle Moose", 11123: HiScores "Popeye Sailorman", 15223
HiScores "Beavis V. Bis", 19999: HiScores "Homer J. Simpson", 12000
HiScores "Ren & Stimpy", 21212: n$ = "begin"
WHILE n$ <> "quit"
   HiShow 10
   LOCATE 1, 1: COLOR 11, 0: PRINT STRING$(160, " "); : LOCATE 1, 1
   INPUT "Name  (or [quit])"; n$: INPUT "Score (or RETURN)"; s&
   HiScores n$, s&:
WEND: CLOSE 1
SUB HiInit (n%)
   FOR t% = 0 TO n%: hs(t%).dat = STRING$(40, CHR$(0)): NEXT
   ScoreInit$ = "": CLOSE 1
   WHILE ScoreInit$ <> "OK"
      OPEN "topfive.dat" FOR RANDOM AS #1 LEN = 40
      IF LOF(1) <> (n% * 40) THEN
         hs(0).dat = STRING$(40, CHR$(0))
         FOR t% = 1 TO n%: PUT #1, t%, hs(0).dat: NEXT
      ELSE
         FOR t% = 1 TO n%: GET #1, t%, hs(t%).dat: NEXT
         ScoreInit$ = "OK"
      END IF
   WEND
END SUB
SUB HiScores (n$, s&)
   MAX% = 10: r% = 0: hs(0).dat = STRING$(40, CHR$(0))
   fill$ = STRING$(30, ".")
   s$ = LEFT$(LTRIM$(n$) + fill$, 30) + LEFT$(STR$(s&) + fill$, 10)
   FOR t% = 1 TO MAX%
      IF INSTR(hs(t%).dat, LTRIM$(n$)) <> 0 THEN r% = t%
   NEXT
   IF r% = 0 OR VAL(MID$(s$, 31)) > VAL(MID$(hs(r%).dat, 31)) THEN
      hs(r%).dat = s$
   ELSE
      EXIT SUB
   END IF
   FOR t% = 0 TO MAX%
      FOR s% = t% TO MAX%
         IF VAL(MID$(hs(s%).dat, 31)) > VAL(MID$(hs(t%).dat, 31)) THEN
            s$ = hs(t%).dat: hs(t%).dat = hs(s%).dat: hs(s%).dat = s$
         END IF
      NEXT
   NEXT
FOR t% = MAX% TO 1 STEP -1: hs(t%).dat = hs(t% - 1).dat: NEXT
END SUB
SUB HiShow (n%)
   COLOR 15, 1: x% = 11 - (n% \ 2)
   FOR t% = 1 TO n%
      PUT #1, t%, hs(t%).dat: LOCATE x% + t%, 20: PRINT hs(t%).dat
   NEXT
END SUB
'_|_|_|   end   HISCOREX.BAS
 
