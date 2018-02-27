'_|_|_|   PAUXZ.BAS
'_|_|_|   An example of a delay function with a .054931641
'_|_|_|   second resolution. No glitch at Midnight.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (2/28/97)
DECLARE SUB pauxz (seconds!)
COLOR 11, 0: CLS : LOCATE 12, 15
INPUT "Enter number of seconds to pause => ", s!
b! = TIMER: PRINT "START::::  "; b!
pauxz s!
f! = TIMER: PRINT "FINISH:::  "; f!
PRINT "Paused for"; f! - b!; "seconds."
'_|_|_|   end   PAUXZ.BAS
SUB pauxz (seconds!)
   DEF SEG = &H40: seconds! = seconds! * 18.20444#: add& = seconds!
   lo& = PEEK(&H6C) AND 255: hi& = PEEK(&H6D) AND 255
   go& = hi& * 256 + lo& + add&
   IF go& > 65535 THEN
      go& = go& - 65536
      DO
         lo& = PEEK(&H6E) AND 255
      LOOP WHILE lo& > 0
   END IF
   DO
      lo& = PEEK(&H6C) AND 255: hi& = PEEK(&H6D) AND 255
      halt& = hi& * 256 + lo&
   LOOP WHILE halt& < go&
   DEF SEG
END SUB
 
