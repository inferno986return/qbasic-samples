'_|_|_|   SHIFT.BAS
'_|_|_|   A simple bit-shifting routine for Qbasic
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (12/7/96)
DECLARE FUNCTION shift% (D%, V%, T%)
DO
   INPUT "number => ", N%
   sh% = N%
   PRINT shift%(0, sh%, 1), shift%(1, sh%, 1)
LOOP WHILE N% > 0
'_|_|_|   end   SHIFT.BAS

FUNCTION shift% (D%, V%, T%)
   S& = V%
   IF S& < 0 THEN S& = 65536 + S&
   FOR T% = T% TO 1 STEP -1
      IF D% = 0 THEN
         S& = S& \ 2
      ELSE
         S& = (S& * 2) AND 65535
      END IF
   NEXT
   IF S& > 32767 THEN S& = S& - 65536
   shift% = S&
END FUNCTION

