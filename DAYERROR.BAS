'_|_|_|   DAYERROR.BAS
'_|_|_|   Returns errorlevel. Sunday = 0 TO Saturday = 7
'_|_|_|   Requires compiling with /L command line argument.
'_|_|_|   No warrantee or guarantee given or implied.
'_|_|_|   PUBLIC DOMAIN   by Kurt Kuzba.  (7/23/1997)
DECLARE SUB ExitErr ALIAS "_exit" (BYVAL N AS INTEGER)
DECLARE FUNCTION DaysInMonth% (M%, Y%)
DECLARE FUNCTION WhatDay% (M%, D%, Y%)
DECLARE FUNCTION IsLeap% (Y%)
D$ = DATE$
M% = VAL(D$)
D% = VAL(MID$(D$, 4))
Y% = VAL(MID$(D$, 7))
D% = WhatDay%(M%, D%, Y%)
ExitErr (D%)
'_|_|_|   DAYOWEEK.BAS

FUNCTION DaysInMonth% (M%, Y%)
   D% = VAL(MID$("303232332323", M%, 1)) + 28
   IF M% = 2 THEN D% = D% + IsLeap%(Y%)
   DaysInMonth% = D%: END FUNCTION

FUNCTION IsLeap% (Y%)
   IsLeap% = ((Y% MOD 100) = 0) - ((Y% MOD 4) = 0) - ((Y% MOD 400) = 0)
END FUNCTION

FUNCTION WhatDay% (M%, D%, Y%) : A& = 0
   FOR T% = 1900 TO Y% - 1: A& = A& + 1 + IsLeap(T%): NEXT
   FOR T% = 1 TO M% - 1: A& = A& + DaysInMonth%((T%), (Y%)): NEXT
   WhatDay% = (A& + D%) MOD 7: END FUNCTION

