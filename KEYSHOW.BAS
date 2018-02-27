'_|_|_|   KEYSHOW.BAS   PUBLIC DOMAIN   Kurt Kuzba   (2/18/1998)
stored% = INP(&H60)
keyin% = stored
CLS
LOCATE 5, 20: PRINT "Hit some keys. ESC exits."
LOCATE 7, 20: PRINT "Shift Status::"
LOCATE 9, 20: PRINT "Keyboard Code:"
DO
   DO
      shift% = PEEK(&H17)
      DEF SEG = &H40: LOCATE 7, 35: PRINT shift%; "  ";
      keyin% = INP(&H60)
      a$ = INKEY$
   LOOP WHILE keyin% = stored%
   LOCATE 9, 35: PRINT keyin%; "   ": stored% = keyin%
   IF ((shift% AND 8) > 0) AND (keyin% = 15) THEN BEEP
   IF keyin% = 1 THEN EXIT DO
LOOP
