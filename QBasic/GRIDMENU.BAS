'_|_|_|   GRIDMENU.BAS   PUBLIC DOMAIN   by  Kurt Kuzba  9/11/1997
'_|_|_|   A demonstration of constructing a grid-type menu for
'_|_|_|   a QuickBasic application.
DO: Gridmenu running%
   SELECT CASE running%
      CASE 1: Mysub1
      CASE 2: Mysub2
      CASE 3: Mysub3
      CASE 4: Mysub4
      CASE 5: Mysub5
      CASE 6: Mysub6
      CASE 7: Mysub7
      CASE 8: Mysub8
      CASE 9: Mysub9
   END SELECT: LOOP WHILE running%: SYSTEM
SUB Gridmenu (Grid%) : COLOR 2, 0: CLS : ShowGrid 0, 0
   IF Grid% = 0 THEN Grid% = 1
   ShowGrid Grid%, 1
   DO: k% = 0: k$ = UCASE$(INKEY$)
      IF k$ > "" THEN k% = ASC(k$): IF k% = 0 THEN k% = -ASC(MID$(k$, 2))
      SELECT CASE k%
         CASE -72, ASC("U"), ASC("N"): ShowGrid Grid%, 0
            Grid% = (Grid% + 5) MOD 9 + 1: ShowGrid Grid%, 1
         CASE -80, ASC("D"), ASC("S"): ShowGrid Grid%, 0
            Grid% = (Grid% + 2) MOD 9 + 1: ShowGrid Grid%, 1
         CASE -75, ASC("L"), ASC("E")
            ShowGrid Grid%, 0: row% = (Grid% - 1) \ 3
            Grid% = (Grid% + 1) MOD 3 + 1 + row% * 3: ShowGrid Grid%, 1
         CASE -77, ASC("R"), ASC("W")
            ShowGrid Grid%, 0: row% = (Grid% - 1) \ 3
            Grid% = (Grid% MOD 3) + 1 + row% * 3: ShowGrid Grid%, 1
         CASE ASC("1") TO ASC("9"): Grid% = k% - ASC("0"): EXIT DO
         CASE 13: EXIT DO
         CASE 27, ASC("0"): Grid% = 0: EXIT DO
      END SELECT: LOOP: END SUB
SUB MenuMessage (s%) :  LOCATE 20, 10: COLOR 10, 0
   SELECT CASE s%
      CASE 1: s$ = "Menu selection ONE"
      CASE 2: s$ = "Menu selection TWO"
      CASE 3: s$ = "Menu selection THREE"
      CASE 4: s$ = "Menu selection FOUR"
      CASE 5: s$ = "Menu selection FIVE"
      CASE 6: s$ = "Menu selection SIX"
      CASE 7: s$ = "Menu selection SEVEN"
      CASE 8: s$ = "Menu selection EIGHT"
      CASE 9: s$ = "Menu selection NINE"
   END SELECT: PRINT LEFT$(s$ + SPACE$(80), 60); : END SUB
SUB Mysub1 : CLS : PRINT "This is the first menu selection."
   WHILE INKEY$ <> "": WEND: WHILE INKEY$ = "": WEND: END SUB
SUB Mysub2 : CLS : PRINT "This is the second menu selection."
   WHILE INKEY$ <> "": WEND: WHILE INKEY$ = "": WEND: END SUB
SUB Mysub3 : CLS : PRINT "This is the third menu selection."
   WHILE INKEY$ <> "": WEND: WHILE INKEY$ = "": WEND: END SUB
SUB Mysub4 : CLS : PRINT "This is the fourth menu selection."
   WHILE INKEY$ <> "": WEND: WHILE INKEY$ = "": WEND:  END SUB
SUB Mysub5 : CLS : PRINT "This is the fifth menu selection."
   WHILE INKEY$ <> "": WEND: WHILE INKEY$ = "": WEND:  END SUB
SUB Mysub6 : CLS : PRINT "This is the sixth menu selection."
   WHILE INKEY$ <> "": WEND: WHILE INKEY$ = "": WEND:  END SUB
SUB Mysub7 : CLS : PRINT "This is the seventh menu selection."
   WHILE INKEY$ <> "": WEND: WHILE INKEY$ = "": WEND:  END SUB
SUB Mysub8 : CLS : PRINT "This is the eighth menu selection."
   WHILE INKEY$ <> "": WEND: WHILE INKEY$ = "": WEND:  END SUB
SUB Mysub9 : CLS : PRINT "This is the ninth menu selection."
   WHILE INKEY$ <> "": WEND: WHILE INKEY$ = "": WEND:  END SUB
SUB ShowGrid (which%, hilite%) : border$ = STRING$(3, 205)
   COLOR 2, 0: IF hilite% <> 0 THEN COLOR 15, 1
   SELECT CASE which%
      CASE 1 TO 9: y% = ((which% - 1) MOD 3) * 10 + 10
         LOCATE ((which% - 1) \ 3) * 5 + 5, y%
         PRINT CHR$(201); border$; CHR$(187)
         LOCATE , y%: PRINT CHR$(186); which%; CHR$(186)
         LOCATE , y%: PRINT CHR$(200); border$; CHR$(188)
         MenuMessage which%
      CASE ELSE: FOR t% = 1 TO 9: ShowGrid t%, 0: NEXT
   END SELECT: END SUB
'_|_|_|   end   GRIDMENU.BAS   PUBLIC DOMAIN
