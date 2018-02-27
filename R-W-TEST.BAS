ON ERROR GOTO OOps
f$ = "filename.dat"
a$ = "Item one"
b$ = "Item two"
CLS
PRINT "test write"
PRINT a$
PRINT b$
Datafile% = FREEFILE
IF Datafile% = 0 THEN
   PRINT "Unable to obtain file handle."
ELSE
   FError$ = "ok"
   OPEN f$ FOR OUTPUT AS #Datafile%
   IF FError$ <> "ok" THEN
      PRINT "Disk Error: Unable to open "; f$; " for output.";
   ELSE
      WRITE #Datafile%, a$, b$  'WRITE Places output in quotes
      CLOSE Datafile%
   END IF
END IF
Datafile% = FREEFILE
a$ = ""
b$ = ""
PRINT "test read"
IF Datafile% = 0 THEN
   PRINT "Unable to obtain file handle."
ELSE
   FError$ = "ok"
   OPEN f$ FOR INPUT AS #Datafile%
   IF FError$ <> "ok" THEN
      PRINT "Disk Error: Unable to open "; f$; " for input.";
   ELSE
      INPUT #Datafile%, a$, b$
      CLOSE Datafile%
   END IF
END IF
PRINT a$
PRINT b$
OOps: FError$ = STR$(ERR): RESUME NEXT

