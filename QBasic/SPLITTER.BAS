'_|_|_|   SPLITTER.BAS
'_|_|_|   Splits a file into 1MB segments.
'_|_|_|   No warrantee or guarantee given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (11/29/96)
ON ERROR GOTO OOps
CLOSE
INPUT "enter name of file to split => ", NFile$
PRINT "enter name of file sections"
INPUT "(EX. game > 'gamex.tmp')    => ", NTemp$
NoSeg% = 0: Source% = FREEFILE
OPEN NFile$ FOR BINARY AS #Source%
Destination% = FREEFILE
WHILE NOT EOF(Source%)
   F$ = NTemp$ + LTRIM$(STR$(NoSeg%)) + ".tmp"
   OPEN F$ FOR OUTPUT AS #Destination%
   accum& = 0
   WHILE (NOT EOF(Source%)) AND (accum& < 1048576)
      dat$ = INPUT$(1024, Source%): ln& = LEN(dat$)
      accum& = accum& + ln&: PRINT #Destination%, dat$;
   WEND
   CLOSE Destination%: NoSeg% = NoSeg% + 1
WEND: CLOSE Source%: END
OOps:
   CLOSE : PRINT ERR: END
'_|_|_|   end   SPLITTER.BAS
