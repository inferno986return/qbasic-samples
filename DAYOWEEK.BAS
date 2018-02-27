'_|_|_|   DAYOWEEK.BAS
'_|_|_|   A collection of routines designed to allow the user to
'_|_|_|   calculate the day of week for a given calender date.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (2/23/97)
DECLARE FUNCTION DayNames$ (D&)
DECLARE FUNCTION DaysInMonth% (M%, Y%)
DECLARE FUNCTION WhatDay$ (M%, D%, Y%)
DECLARE FUNCTION IsLeap% (Y%)
CLS
DO
   DO: INPUT "Enter Month [mm]   => ", M$
      M% = VAL(M$): IF M% = 0 THEN EXIT DO
   LOOP WHILE (M% < 1) OR (M% > 12): IF M% = 0 THEN EXIT DO
   DO: INPUT "Enter Day   [dd]   => ", D$
      D% = VAL(D$): IF D% = 0 THEN EXIT DO
   LOOP WHILE (D% < 1) OR (D% > 31): IF D% = 0 THEN EXIT DO
   DO: INPUT "Enter Year  [yyyy] => ", Y$
      Y% = VAL(Y$): IF Y% = 0 THEN EXIT DO
   LOOP WHILE (Y% < 1800) OR (Y% > 2999): IF Y% = 0 THEN EXIT DO
   Abrv% = 0: PRINT WhatDay$(M%, D%, Y%)
   Abrv% = 1: PRINT WhatDay$(M%, D%, Y%)
LOOP WHILE M% <> 0: SYSTEM
FUNCTION DayNames$ (D&)
   SHARED Abrv%
   N$ = "Sunday   Monday   Tuesday  WednesdayThursday Friday   Saturday"
   A$ = "SunMonTueWedThuFriSat"
   DayNames$ = RTRIM$(MID$(N$, (D& MOD 7) * 9 + 1, 9))
   IF Abrv% <> 0 THEN DayNames$ = RTRIM$(MID$(A$, (D& MOD 7) * 3 + 1, 3))
END FUNCTION
FUNCTION DaysInMonth% (M%, Y%)
   D% = VAL(MID$("303232332323", M%, 1)) + 28
   IF M% = 2 THEN D% = D% + IsLeap%(Y%)
   DaysInMonth% = D%
END FUNCTION
FUNCTION IsLeap% (Y%)
   IsLeap% = ((Y% MOD 100) = 0) - ((Y% MOD 4) = 0) - ((Y% MOD 400) = 0)
END FUNCTION
FUNCTION WhatDay$ (M%, D%, Y%)
   A& = 0: IF Y% < 1900 THEN WhatDay$ = "Unknown": EXIT FUNCTION
   FOR T% = 1900 TO Y% - 1: A& = A& + 1 + IsLeap(T%): NEXT
   FOR T% = 1 TO M% - 1: A& = A& + DaysInMonth%((T%), (Y%)): NEXT
   A& = A& + D%: WhatDay$ = DayNames$(A&)
END FUNCTION
'_|_|_|   DAYOWEEK.BAS
 
