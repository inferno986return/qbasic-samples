DECLARE SUB CapsOn (x%)
CapsOn 1
PRINT "The Capslock key is now ON"
PRINT "Please hit Enter now"
WHILE INKEY$ = "": WEND
CapsOn 0
PRINT "The Capslock key is now OFF"
PRINT "Please hit Enter now"
WHILE INKEY$ = "": WEND

SUB CapsOn (x%)
   DEF SEG = &H40: sh% = PEEK(&H17)
   IF x% = 0 THEN POKE &H17, sh% AND 191:  ELSE POKE &H17, sh% OR 64
   DEF SEG
END SUB

