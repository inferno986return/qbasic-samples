'_|_|_|   ROUND2X.BAS   PUBLIC DOMAIN   Kurt Kuzba (3/2/1999)
'_|_|_|   Simple function demonstrating the rounding of a
'_|_|_|   single precision variable to varying decimal places.
'_|_|_|
DECLARE FUNCTION RoundToXPlaces! (N!, X%)
RANDOMIZE TIMER
DIM Fln AS SINGLE
PRINT "___ Test of Rounding Function ___"
Fln = (RND * 8000 + 8000) / 91: PRINT Fln, "Raw number"
PRINT RoundToXPlaces(Fln, 1), "Rounded to 1 place"
PRINT RoundToXPlaces(Fln, 2), "Rounded to 2 places"
PRINT RoundToXPlaces(Fln, 3), "Rounded to 3 places"
PRINT RoundToXPlaces(Fln, 4), "Rounded to 4 places"
PRINT "__________ End of Test __________"
PRINT
END

FUNCTION RoundToXPlaces! (N!, X%)
   Round! = N! - INT(N!)
   Round! = INT(Round! * (10 ^ X%) + .5)
   Round! = Round! * (10 ^ -X%)
   RoundToXPlaces! = INT(N!) + Round!
END FUNCTION

