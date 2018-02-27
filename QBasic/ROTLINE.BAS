DECLARE SUB UpdateFifteen (Myfile$, GnewLyne$)
DO
   INPUT "Line to add => ", NL$
   IF NL$ = "" THEN EXIT DO
   UpdateFifteen "test.txt", NL$
   SHELL "type test.txt"
LOOP

SUB UpdateFifteen (Myfile$, GnewLyne$)
   DIM Fifteen$(16)
   FyleIO% = FREEFILE
   OPEN Myfile$ FOR INPUT AS #FyleIO%
   LyneNum% = 0
   DO
      IF NOT EOF(FyleIO%) THEN
         LINE INPUT #FyleIO%, Fifteen$(LyneNum%)
         LyneNum% = LyneNum% + 1
      ELSE
         EXIT DO
      END IF
   LOOP WHILE LyneNum% < 15
   CLOSE FyleIO%
   IF LyneNum% = 15 THEN
      BGN% = 1: ND% = 15: Fifteen$(15) = GnewLyne$
   ELSE
      BGN% = 0: ND% = LyneNum%: Fifteen$(LyneNum%) = GnewLyne$
   END IF
   OPEN Myfile$ FOR OUTPUT AS #FyleIO%
   FOR T% = BGN% TO ND%: PRINT #FyleIO%, Fifteen$(T%): NEXT
   CLOSE FyleIO%
END SUB

