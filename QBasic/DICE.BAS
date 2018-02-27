DECLARE SUB DrawDice (vl%, x%, y%)
RANDOMIZE TIMER: play$ = "yes"
WHILE play$ = "yes"
   COLOR 7, 0: CLS
   LOCATE 3, 10: PRINT "SPACE to begin play, or RETURN to quit"
   k$ = "": WHILE k$ <> CHR$(13) AND k$ <> CHR$(32): k$ = INKEY$: WEND
   IF k$ = CHR$(13) THEN
      play$ = "no"
   ELSE
      player% = 0: computer% = 0
      COLOR 7, 0: CLS : COLOR 15, 1: LOCATE 9
      LOCATE , 23: PRINT "[][][][][][][][][][][][][][][][][][]"
      LOCATE , 23: PRINT "[]  Press SPACE to roll the die.  []"
      LOCATE , 23: PRINT "[]  Player #1       Computer      []"
      LOCATE , 23: PRINT "[]                                []"
      LOCATE , 23: PRINT "[]                                []"
      LOCATE , 23: PRINT "[]                                []"
      LOCATE , 23: PRINT "[]  Presently playing turn #      []"
      LOCATE , 23: PRINT "[][][][][][][][][][][][][][][][][][]"
      FOR turn% = 1 TO 10
         COLOR 15, 3
         LOCATE 11, 37: PRINT player%
         LOCATE 11, 52: PRINT computer%
         LOCATE 15, 52: PRINT turn%
         WHILE INKEY$ <> CHR$(32): WEND
         FOR roll% = 1 TO 20
            vl% = ((RND * 999) MOD 6) + 1: DrawDice vl%, 12, 30
         NEXT: player% = player% + vl%
         FOR roll% = 1 TO 20
            vl% = ((RND * 999) MOD 6) + 1: DrawDice vl%, 12, 45
         NEXT: computer% = computer% + vl%
      NEXT
   COLOR 15, 3
   LOCATE 11, 37: PRINT player%
   LOCATE 11, 52: PRINT computer%
   LOCATE 15, 52: PRINT turn%
   LOCATE 16, 23: COLOR 15, 1
   IF player% > computer% THEN
      PRINT "[]     You Won the Game!!         []"
   END IF
   IF player% < computer% THEN
      PRINT "[]     The Computer Won!!         []"
   END IF
   IF player% = computer% THEN
      PRINT "[]     It was a Tie Score.        []"
   END IF
   LOCATE , 23: PRINT "[]     Hit RETURN to continue     []"
   LOCATE , 23: PRINT "[][][][][][][][][][][][][][][][][][]"
   WHILE INKEY$ <> CHR$(13): WEND
   END IF
WEND
END

SUB DrawDice (vl%, x%, y%)
   COLOR 15, 1: LOCATE x%
   d$ = "                     "
   WAIT &H3DA, 8: WAIT &H3DA, 8, 8
   LOCATE , y%: PRINT LEFT$(d$, 7)
   LOCATE , y%: PRINT MID$(d$, 8, 7)
   LOCATE , y%: PRINT RIGHT$(d$, 7)
   SELECT CASE vl%
      CASE IS = 1: d$ = "          O          "
      CASE IS = 2: d$ = " O                 O "
      CASE IS = 3: d$ = " O        O        O "
      CASE IS = 4: d$ = " O   O         O   O "
      CASE IS = 5: d$ = " O   O    O    O   O "
      CASE IS = 6: d$ = " O   O  O   O  O   O "
   END SELECT
   COLOR 1, 7: LOCATE x%
   WAIT &H3DA, 8: WAIT &H3DA, 8, 8
   WAIT &H3DA, 8: WAIT &H3DA, 8, 8
   LOCATE , y%: PRINT LEFT$(d$, 7)
   LOCATE , y%: PRINT MID$(d$, 8, 7)
   LOCATE , y%: PRINT RIGHT$(d$, 7)
END SUB

