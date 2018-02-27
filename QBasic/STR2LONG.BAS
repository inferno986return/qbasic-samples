DECLARE FUNCTION Str2Long& (v$)
PRINT Str2Long("1,250,001")
i% = INT(Str2Long("256")): PRINT i%
PRINT Str2Long("1,234.567")

FUNCTION Str2Long& (v$)
   v& = 0: l% = LEN(v$): p% = 0
   FOR t% = 1 TO l%
      v% = ASC(MID$(v$, t%)) - 48: IF v% = -2 THEN p% = v%
      IF v% > -1 AND v% < 10 AND p% = 0 THEN v& = 10 * v& + v%
   NEXT
   Str2Long& = v&
END FUNCTION

