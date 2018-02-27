DECLARE FUNCTION commas$ (x#)
DO
   INPUT "Number => ", n#
   num$ = STR$(n#)
   PRINT num$; commas$(n#); : PRINT USING "#############,.#####&"; n#
LOOP WHILE n# <> 0

FUNCTION commas$ (x#)
   temp$ = STR$(x#): result$ = "": comma% = 0
   IF INSTR(temp$, ".") THEN
      count% = INSTR(temp$, ".") - 1: result$ = MID$(temp$, count% + 1)
   ELSE count% = LEN(temp$)
   END IF
   FOR count% = count% TO 1 STEP -1
      result$ = MID$(temp$, count%, 1) + result$
      comma% = (comma% + 1) MOD 3
      IF comma% = 0 AND count% > 2 THEN result$ = "," + result$
   NEXT
   commas$ = result$
END FUNCTION

