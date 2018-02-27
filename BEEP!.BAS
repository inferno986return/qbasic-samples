DECLARE SUB phone ()
DECLARE SUB frog ()
DECLARE SUB wolfwhistle ()
DECLARE SUB cricket ()
COLOR 7, 0
CLS
COLOR 15, 1: LOCATE 10
LOCATE , 25: PRINT "께께께께께께께께께께께께께�"
LOCATE , 25: PRINT "께  C ]=->  Cricket      께"
LOCATE , 25: PRINT "께  F ]=->  Frog         께"
LOCATE , 25: PRINT "께  W ]=->  Wolfwhistle  께"
LOCATE , 25: PRINT "께  P ]=->  Phone        께"
LOCATE , 25: PRINT "께  RETURN = EXIT        께"
LOCATE , 25: PRINT "께께께께께께께께께께께께께�"
WHILE t$ <> CHR$(13)
   t$ = "": WHILE t$ = "": t$ = INKEY$: WEND
   IF UCASE$(t$) = "C" THEN cricket
   IF UCASE$(t$) = "F" THEN frog
   IF UCASE$(t$) = "W" THEN wolfwhistle
   IF UCASE$(t$) = "P" THEN phone
WEND

SUB cricket
   FOR t% = 0 TO 40 STEP 4
      SOUND t% + 2000, .11
      SOUND t% + 5000, .11
      SOUND t% + 6000, .11
      SOUND t% + 3000, .11
      SOUND t% + 2000, .2
   NEXT
END SUB

SUB frog
   FOR t% = 100 TO 200 STEP 5
      SOUND 300 - t%, .11
      SOUND t%, .11
      SOUND t% - 50, .11
   NEXT
END SUB

SUB phone
   FOR t! = .1 TO .3 STEP .01
      SOUND 3136, t!
      SOUND 1568, t!
      SOUND 784, t!
   NEXT
END SUB

SUB wolfwhistle
      FOR t% = 1000 TO 4000 STEP 70
         SOUND t%, .03
         SOUND t% + 10, .03
         SOUND t% + 30, .03
      NEXT
      FOR t% = 1 TO 50: WAIT &H3DA, 8: WAIT &H3DA, 255, 8: NEXT
      FOR t% = 700 TO 2500 STEP 80
         SOUND t%, .03
         SOUND t% + 10, .03
         SOUND t% + 30, .03
      NEXT
      FOR t% = 2500 TO 400 STEP -80
         SOUND t%, .03
         SOUND t% + 10, .03
         SOUND t% + 30, .03
      NEXT
END SUB

