DECLARE FUNCTION Hex32Dec& (AlpNum$)
DECLARE FUNCTION Dec32Hex2$ (DecNum&, LenNum%)
DECLARE FUNCTION H36$ (N&, L%)
DECLARE FUNCTION D36& (S$)
DEFINT A-Z
PRINT : PRINT "START"
DEF SEG = &H40
tm1& = (PEEK(&H6D) AND 255): tm1& = tm1& * 256 + (PEEK(&H6C) AND 255)
FOR t% = 1 TO 10000: a& = D36&(H36$(123456, 4)): NEXT
tm2& = (PEEK(&H6D) AND 255): tm2& = tm2& * 256 + (PEEK(&H6C) AND 255)
PRINT "Execution required"; tm2& - tm1&; "BIOS clock ticks"
p$ = H36$(123456, 4): p& = D36&(p$)
PRINT p$, p&
tm1& = (PEEK(&H6D) AND 255): tm1& = tm1& * 256 + (PEEK(&H6C) AND 255)
FOR t% = 1 TO 10000: a& = Hex32Dec&(Dec32Hex2$(123456, 4)): NEXT
tm2& = (PEEK(&H6D) AND 255): tm2& = tm2& * 256 + (PEEK(&H6C) AND 255)
PRINT "Execution required"; tm2& - tm1&; "BIOS clock ticks"
p$ = Dec32Hex2$(123456, 4): p& = Hex32Dec&(p$)
PRINT p$, p&

DEFSNG A-Z
FUNCTION D36& (S$)
   N& = 0: L% = LEN(S$): S$ = UCASE$(S$)
   FOR t% = 1 TO L%
      v% = ASC(MID$(S$, t%)) - 48
      SELECT CASE v%
         CASE 0 TO 9: N& = N& * 36 + v%
         CASE 17 TO 42: N& = N& * 36 + v% - 7
      END SELECT
   NEXT: D36& = N&
END FUNCTION

DEFINT A-Z
FUNCTION Dec32Hex2$ (DecNum&, LenNum)
'-========================-
'--- Function: DEC32HEX$ ---
'-========================-  
'      Written by Carl Gorringe; optimized by Churl
' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'-- This will Convert a Decimal (Base-10) number         ---
'---    to a Hexatridecimal (Base-36) number.            ---
'--- Use LenNum to specify length of string, 0 for Auto. ---
'-----------------------------------------------------------
'AlpNum$ = ""                         'only needed if STATIC
DecNum2& = DecNum&                    'make copies
LenNum2 = LenNum
DO
   DecDigit = DecNum2& MOD 36
   DecNum2& = DecNum2& \ 36
  
   IF DecDigit >= 10 THEN
      DecDigit = DecDigit + 7         'DecDigit& = DecDigit& + 55
   END IF
   AlpNum$ = CHR$(DecDigit + 48) + AlpNum$
   LenNum2 = LenNum2 - 1
LOOP WHILE LenNum2 > 0
Dec32Hex2$ = AlpNum$
END FUNCTION

DEFSNG A-Z
FUNCTION H36$ (N&, L%)
   H$ = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ": S$ = ""
   FOR t% = 1 TO L%
      S$ = MID$(H$, (N& MOD 36) + 1, 1) + S$: N& = N& \ 36
   NEXT: H36$ = S$
END FUNCTION

DEFINT A-Z
FUNCTION Hex32Dec& (AlpNum$)
'-========================-
'--- Function: HEX32DEC& ---
'-========================-
'      Written by Carl Gorringe; optimized by Churl
' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'---   This will Convert a Hexatridecimal (Base-36)    ---
'---      number to a Decimal (Base-10) number.        ---
'---------------------------------------------------------
AlpNum$ = LTRIM$(RTRIM$(AlpNum$))
LenNum = LEN(AlpNum$)
Power& = 1
'DecNum& = 0                  'Only needed if STATIC
IF LenNum < 5 THEN
   FOR Digit = LenNum TO 1 STEP -1
      AlpDigit = ASC(MID$(AlpNum$, Digit, 1)) - 48
      IF AlpDigit > (64 - 48) THEN AlpDigit = AlpDigit - 7  '48 + 7 = 55
      DecNum& = DecNum& + (AlpDigit * Power&)
      Power& = Power& * 36
   NEXT
END IF
Hex32Dec& = DecNum&
END FUNCTION

