'_|_|_|   READRAND.BAS   PUBLIC DOMAIN   Kurt Kuzba   (4/28/1998)
'_|_|_|   This program will open a standard text file and index
'_|_|_|   the lines, up to 10,000, choose one at random, and then
'_|_|_|   print it out to the screen.
'_|_|_|   No guarantee or warrantee given or implied.
ON ERROR GOTO Oops
'$DYNAMIC
RANDOMIZE TIMER
DIM Text AS STRING, CRLF AS STRING, FError AS STRING
DIM Fil AS INTEGER, Tpos AS LONG, Ptr AS INTEGER
DIM Index(1 TO 10000) AS LONG, Nlines AS LONG
INPUT "File to read => ", Text
IF Text = "" THEN SYSTEM
CRLF = CHR$(13) + CHR$(10)
FError = "ok": Fil = FREEFILE: OPEN Text FOR INPUT AS #Fil
IF FError <> "ok" THEN
   PRINT "Could not open "; CHR$(34); Text; CHR$(34)
   WHILE INKEY$ = "": WEND: SYSTEM
END IF
CLOSE Fil: OPEN Text FOR BINARY AS #Fil: Nlines = 0
WHILE (NOT EOF(Fil)) AND (Nlines < 10000)
   Text = INPUT$(1024, Fil): Ptr = 0
   Tpos = LOC(Fil) - LEN(Text)
   IF LEN(Text) > 0 THEN
      DO
         Nlines = Nlines + 1: Index(Nlines) = Tpos + Ptr
         IF (INSTR(Ptr + 1, Text, CRLF)) THEN
            Ptr = (INSTR(Ptr + 1, Text, CRLF)) + 2
         ELSE
            EXIT DO
         END IF
         IF Nlines = 10000 THEN EXIT DO
      LOOP
   END IF
WEND
PRINT "success"; Nlines; "lines read."
SEEK Fil, Index(RND * (Nlines - 1) + 1)
Text = INPUT$(1024, Fil)
CLOSE Fil
PRINT LEFT$(Text, INSTR(Text, CRLF));
CLOSE Fil: SYSTEM
Oops: FError = STR$(ERR): RESUME NEXT
'_|_|_|   end   READRAND.BAS   PUBLIC DOMAIN
 
