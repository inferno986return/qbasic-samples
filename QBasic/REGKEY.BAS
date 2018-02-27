'_|_|_|   REGKEY.BAS   PUBLIC DOMAIN   Kurt Kuzba  (11/23/1999)
'_|_|_|   Create and test registration key codes.  This example
'_|_|_|   uses a very simple encoding sequence, and any full
'_|_|_|   implementation should use a more complex algorithm.
'_|_|_|   Creation code would be used by author, and test code
'_|_|_|   placed in software to be registered.
DECLARE FUNCTION Regkey$ (n$)
PRINT : PRINT "Create new regfile?"
k$ = "": WHILE INSTR(" YN", k$) < 2: k$ = UCASE$(INKEY$): WEND
IF k$ = "Y" THEN
   INPUT "Name to use for key"; n$
   IF LEN(n$) = 0 THEN END
   OPEN "regdata.txt" FOR OUTPUT AS #1
   rk$ = Regkey$(n$)
   PRINT #1, n$: PRINT #1, rk$: CLOSE 1
   PRINT "Reg Name: "; n$: PRINT "Reg Key:: "; rk$
   END
END IF
OPEN "regdata.txt" FOR INPUT AS #1
LINE INPUT #1, n$: LINE INPUT #1, rk$: CLOSE 1
key$ = Regkey$(n$)
IF rk$ = key$ THEN
   PRINT "This registration code is GOOD!"
   PRINT n$: PRINT rk$
ELSE
   PRINT "This registration code is BOGUS"
   rk$ = Regkey$(n$)
   PRINT "For "; n$; ", It SHOULD have been "; rk$
END IF

FUNCTION Regkey$ (n$)
   k$ = "": l% = LEN(n$)
   key$ = "This_is_a_secret_registration_password_key_code_phrase"
   '_|_|_|   The keycode string should always be the same.
   FOR n% = 1 TO l%: k$ = MID$(n$, n%, 1) + k$: NEXT
   FOR n% = 1 TO l%
      a% = ASC(MID$(n$, n%, 1)) + ASC(MID$(k$, n%, 1))
      MID$(k$, n%, 1) = CHR$((a% + ASC(MID$(key$, n%, 1))) MOD 64 + 48)
   NEXT
   Regkey$ = k$
END FUNCTION

