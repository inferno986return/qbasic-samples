'_|_|_|   RNDSTR.BAS
'_|_|_|   This program will randomize the characters in a string
'_|_|_|   Thanks to Bill White, ( of Miami :), for the shuffling
'_|_|_|   routine used here.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (2/2/96)
RANDOMIZE TIMER
DO
   INPUT "Enter a string to be scrambled => ", S$
   IF S$ <> "" THEN
      L% = LEN(S$) - 1
      FOR T% = 0 TO 999
         R% = RND * L% + 1
         T$ = MID$(S$, (T% MOD L%) + 1, 1)
         MID$(S$, (T% MOD L%) + 1) = MID$(S$, R%, 1)
         MID$(S$, R%) = T$
      NEXT
   END IF
   PRINT CHR$(34); S$; CHR$(34)
LOOP WHILE S$ <> ""
'_|_|_|   end  RNDSTR.BAS

