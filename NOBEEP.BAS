DECLARE SUB outchr (a$)
outchr ("hello" + CHR$(7) + CHR$(12))
WHILE INKEY$ = "": WEND

SUB outchr (a$)
l% = LEN(a$)
mask$ = CHR$(7) + CHR$(12)
FOR t% = 1 TO l%
   x% = CSRLIN: y% = POS(0): o$ = MID$(a$, t%, 1)
   IF INSTR(mask$, o$) <> 0 THEN
      PRINT " "; : DEF SEG = &HB800
      POKE (x% * 80 + y%) * 2 - 162, ASC(o$)
   ELSE PRINT o$;
   END IF
NEXT
END SUB

