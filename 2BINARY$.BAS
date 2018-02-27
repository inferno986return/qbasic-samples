DECLARE FUNCTION binary$ (ToBin&)
INPUT "Enter a number to represent in binary"; number&
PRINT binary$(number&); " ="; number&

FUNCTION binary$ (ToBin&)
   result$ = ""
   FOR bit% = 0 TO 15
      result$ = CHR$(48 - ((ToBin& AND (2 ^ bit%)) <> 0)) + result$
   NEXT
   binary$ = result$
END FUNCTION

