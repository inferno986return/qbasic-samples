'_|_|_|  826_BIT.BAS
'_|_|_|  This program demonstrates one method of encoding data
'_|_|_|  to conform to low ASCII requirements by turning three
'_|_|_|  8-bit values into four 6-bit values and vice-verse.
'_|_|_|  No warrantees or guarantees are given or implied.
'_|_|_|  Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (6/1/96)
DECLARE FUNCTION ENCODE$ (Bytes3$)
DECLARE FUNCTION UNCODE$ (Bytes4$)
PRINT : PRINT
test$ = CHR$(176) + CHR$(177) + CHR$(178)
PRINT test$, ENCODE$(test$), UNCODE$(ENCODE$(test$))
test$ = CHR$(254) + CHR$(219) + CHR$(129)
PRINT test$, ENCODE$(test$), UNCODE$(ENCODE$(test$))
test$ = CHR$(17) + CHR$(21) + CHR$(7)
PRINT test$, ENCODE$(test$), UNCODE$(ENCODE$(test$))
test$ = "ABC"
PRINT test$, ENCODE$(test$), UNCODE$(ENCODE$(test$))
'_|_|_|  end 826_BIT.BAS

FUNCTION ENCODE$ (Bytes3$)
   Result$ = "": B& = 0
   FOR t% = 3 TO 1 STEP -1
      B& = B& * 256 + ASC(MID$(Bytes3$, t%))
   NEXT
   FOR t% = 1 TO 4
      Result$ = Result$ + CHR$(48 + (B& AND 63)): B& = B& \ 64
   NEXT: ENCODE$ = Result$
END FUNCTION

FUNCTION UNCODE$ (Bytes4$)
   Result$ = "": B& = 0
   FOR t% = 4 TO 1 STEP -1
      B& = B& * 64 + ASC(MID$(Bytes4$, t%)) - 48
   NEXT
   FOR t% = 1 TO 3
      Result$ = Result$ + CHR$(B& AND 255): B& = B& \ 256
   NEXT: UNCODE$ = Result$
END FUNCTION

