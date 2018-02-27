'_|_|_|   LABEL.BAS
'_|_|_|   A simple method to read disk volume labels.
'_|_|_|   No warrantee or guarantee given or implied.
'_|_|_|   PUBLIC DOMAIN   by Kurt Kuzba.  (7/22/1997)
ON ERROR GOTO BooBoo
COLOR 1, 7: INPUT "Enter drive label to read => ", Drive$
COLOR 14, 0
SHELL "dir " + Drive$ + ":\label.get > label.txt"
IF CSRLIN > 1 THEN LOCATE CSRLIN - 1: PRINT SPACE$(80); : LOCATE , 1
Ferror$ = "": lab% = FREEFILE: OPEN "label.txt" FOR INPUT AS #lab%
IF Ferror$ > "" THEN PRINT "Error reading "; Drive$: SYSTEM
txtline% = 0
WHILE NOT EOF(lab%)
   txtline% = txtline% + 1: LINE INPUT #lab%, a$
   IF txtline% = 2 THEN VolumeLabel$ = a$
WEND
CLOSE lab%
KILL "label.txt"
PRINT VolumeLabel$
SYSTEM
BooBoo: Ferror$ = STR$(ERR): RESUME NEXT
'_|_|_|   end   LABEL.BAS
 
