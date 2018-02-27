'_|_|_|   SUBGOSUB.BAS
'_|_|_|   A program to demonstrate the differences and use of BASIC
'_|_|_|   SUBs and GOSUBs.
'_|_|_|   No warrantees or guarantees are given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (5/1/96)
DECLARE SUB ONE (X%)
CLS : COLOR 12: var% = 0
PRINT "We begin with var% having a value of  :";
GOSUB TWO
PRINT "After a call to ONE, passed by address:";
ONE var%: GOSUB TWO
PRINT "After a call to ONE, passed by value  :";
ONE (var%): GOSUB TWO
PRINT "Variables passed to a SUB without parentheses are passed"
PRINT "by address, which means that the variable may be changed"
PRINT "by the SUB to which it is being passed."
PRINT "When the passed value is enclosed in parentheses, then it"
PRINT "is passed by value, and is uneffected by actions upon it"
PRINT "within the SUB to which it is passed."
PRINT "All variables within a SUBROUTINE, accessed via a GOSUB,"
PRINT "are shared within the module in which the SUBROUTINE is"
PRINT "found. A SUBROUTINE may only be called from the module"
PRINT "within which it resides."
END
TWO:
   PRINT var%
RETURN
 
SUB ONE (X%)
   X% = X% + 1
END SUB
'_|_|_|   end   SUBGOSUB.BAS
