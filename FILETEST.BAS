ON ERROR GOTO BooBoo
DO
   INPUT "Name of file to test => ", FileN$
   IF FileN$ > "" THEN
      FTest% = FREEFILE: FError$ = "ok"
      OPEN FileN$ FOR INPUT AS #FTest%
      PRINT FileN$; " does ";
      IF FError$ = "ok" THEN CLOSE FTest%:  ELSE PRINT "not ";
      PRINT "exist."
      tmr# = TIMER
      FOR t& = 0 TO 1000000: NEXT
      t# = TIMER:
      PRINT t# - tmr#
   END IF
LOOP WHILE FileN$ > ""
SYSTEM
BooBoo: FError$ = STR$(ERR): RESUME NEXT

