'_|_|_|   CHNGE_EX.BAS
'_|_|_|   Simple change-making program for QuickBasic.
'_|_|_|   Released to the   Public Domain   by Kurt Kuzba
'_|_|_|
Process:
   INPUT "Cash Tendered"; Cash#
   INPUT " - Total Owed"; Due#
   Cash# = Cash# - Due#: Change& = Cash# * 100
   PRINT "--------------------------"
   dnm% = 10000: d2$ = " Hundreds ": d1$ = " Hundred  ": GOSUB Change
   dnm% = 5000: d2$ = " Fifties  ": d1$ = " Fifty    ": GOSUB Change
   dnm% = 2000: d2$ = " Twenties ": d1$ = " Twenty   ": GOSUB Change
   dnm% = 1000: d2$ = " Tens     ": d1$ = " Ten      ": GOSUB Change
   dnm% = 500: d2$ = " Fives    ": d1$ = " Five     ": GOSUB Change
   dnm% = 100: d2$ = " Ones     ": d1$ = " One      ": GOSUB Change
'  dnm% = 50: d2$ = " Halves   ": d1$ = " Half     ": GOSUB Change
   dnm% = 25: d2$ = " Quarters ": d1$ = " Quarter  ": GOSUB Change
   dnm% = 10: d2$ = " Dimes    ": d1$ = " Dime     ": GOSUB Change
   dnm% = 5: d2$ = " Nickels  ": d1$ = " Nickel   ": GOSUB Change
   dnm% = 1: d2$ = " Pennies  ": d1$ = " Penny    ": GOSUB Change
   PRINT "--------------------------"
   PRINT "** TOTAL ** "; : PRINT USING "$######.##"; Cash#
   PRINT "Thank You ... Process another transaction? (Y/N)"
   K$ = "": WHILE K$ <> "Y" AND K$ <> "N": K$ = UCASE$(INKEY$): WEND
   IF K$ = "Y" THEN GOTO Process:  ELSE END
Change:
   Num& = (Change& \ dnm%)
   IF Num& = 1 THEN PRINT STR$(Num&); d1$;
   IF Num& > 1 THEN PRINT STR$(Num&); d2$;
   IF Num& > 0 THEN PRINT USING "$######.##"; (Num& * dnm%) / 100
   Change& = Change& MOD dnm%
   RETURN

