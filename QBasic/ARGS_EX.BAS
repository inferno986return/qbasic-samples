DIM cmd$(32)
arg$ = LTRIM$(RTRIM$(COMMAND$))
temp$ = arg$: args% = 0: cmd$(0) = ""
WHILE LEN(temp$) <> 0
   IF INSTR(temp$, " ") = 0 THEN
      cmd$(args%) = temp$
      temp$ = ""
   ELSE
      WHILE ASC(temp$) <> 32
         cmd$(args%) = cmd$(args%) + CHR$(ASC(temp$))
         temp$ = MID$(temp$, 2)
      WEND
   END IF
   args% = args% + 1
   temp$ = LTRIM$(temp$)
WEND
COLOR 10, 0: CLS
PRINT : PRINT "There were"; args%; "commands."
FOR t% = 1 TO args%: PRINT CHR$(34); cmd$(t% - 1); CHR$(34): NEXT

