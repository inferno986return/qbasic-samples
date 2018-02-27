'_|_|_|   LEAPDAYS.BAS
'_|_|_|   A program to determine if a given year was a leap year.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (10/16/96)
DECLARE FUNCTION LeapDay% (Y AS LONG)
DO
   INPUT "   Enter year => ", Year&: IF Year& <= 1 THEN END
   Days% = 365 + LeapDay(Year&)
   PRINT "The year"; Year&; "has"; Days% + D%; "days in it."
LOOP
FUNCTION LeapDay% (Y AS LONG)
   LeapDay% = ((Y MOD 100) = 0) - ((Y MOD 4) = 0) - ((Y MOD 400) = 0)
END FUNCTION
'_|_|_|   end   LEAPDAYS.BAS
