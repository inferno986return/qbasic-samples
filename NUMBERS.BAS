'_|_|_|   NUMBERS.BAS
'_|_|_|   Print text representations of a LONG integer.
'_|_|_|   No warrantee or guarantee given or implied.
'_|_|_|   PUBLIC DOMAIN   by Kurt Kuzba.  (8/7/1997)
DECLARE SUB Numbers (N#)
DECLARE FUNCTION Tens$ (N AS LONG)
PRINT
DO
   INPUT "Please enter a number => ", N#: L# = N#
   PRINT L#: Numbers L#: PRINT CHR$(13)
   IF N# = 0 THEN EXIT DO
LOOP
SYSTEM
SUB Numbers (T#)
   SELECT CASE T#
      CASE 0: PRINT "zero"
      CASE IS > 999999999
         PRINT Tens$(INT((T# \ 1000000000))); " billion";
         T# = T# MOD 1000000000
      CASE IS > 999999
         PRINT Tens$(INT((T# \ 1000000))); " million";
         T# = T# MOD 1000000
      CASE IS > 999
         PRINT Tens$(INT((T# \ 1000))); " thousand";
         T# = T# MOD 1000
      CASE ELSE
         PRINT Tens$((INT(T#))); : T# = 0
   END SELECT
   IF T# > 0 THEN Numbers T#
END SUB
FUNCTION Tens$ (N AS LONG)
   O$ = ""
   SELECT CASE N
      CASE 1 TO 12:
         T$ = "one   two   three four  five  six   seven eight "
         T$ = T$ + "nine  ten   eleventwelve"
         O$ = " " + RTRIM$(MID$(T$, N * 6 - 5, 6))
      CASE 13 TO 19:
         T$ = "thir four fif  six  seveneightnine"
         O$ = " " + RTRIM$(MID$(T$, N * 5 - 64, 5)) + "teen"
      CASE 20 TO 99:
         T$ = "twen thir for  fif  six  seveneigh nine"
         O$ = " " + RTRIM$(MID$(T$, (N \ 10) * 5 - 9, 5)) + "ty"
         IF (N MOD 10) > 0 THEN
            O$ = O$ + "-"
            O$ = O$ + MID$(Tens$(N MOD 10), 2)
         END IF
      CASE 100 TO 999:
         PRINT Tens$((N \ 100)); " hundred";
         IF (N MOD 100) > 0 THEN PRINT Tens$((N MOD 100));
   END SELECT
   Tens$ = O$
END FUNCTION
'_|_|_|   end   NUMBERS.BAS
 
