'_|_|_|   CONCAT.BAS
'_|_|_|   Restores files split using SPLITTER.BAS to one file.
'_|_|_|   No warrantee or guarantee given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (11/29/96)
ON ERROR GOTO OOps
INPUT "Files to concatenate => ", Pieces$
INPUT "Name of output file  => ", Whole$
W% = FREEFILE: OPEN Whole$ FOR OUTPUT AS #W%
IF BooBoo% <> 0 THEN CLOSE : END
P% = FREEFILE: Part% = 0
DO
   F$ = Pieces$ + LTRIM$(STR$(Part%)) + ".tmp"
   OPEN F$ FOR INPUT AS #P%
   IF BooBoo% <> 0 THEN CLOSE : END:  ELSE CLOSE P%
   OPEN F$ FOR BINARY AS #P%
   WHILE NOT EOF(P%)
      dat$ = INPUT$(1024, P%): PRINT #W%, dat$;
   WEND
   CLOSE P%: Part% = Part% + 1
LOOP: END
OOps:
   BooBoo% = ERR: RESUME NEXT
'_|_|_|   end   CONCAT.BAS
