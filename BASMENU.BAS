'_|_|_|   BASMENU.BAS
'_|_|_|   A simple menuing system in QuickBasic.
'_|_|_|   No warrantee or guarantee given or implied.
'_|_|_|   PUBLIC DOMAIN   by  Kurt Kuzba.  (7/15/1997)
TYPE FileList: F AS STRING * 12: END TYPE
DECLARE SUB BoxBorder ()
DECLARE FUNCTION ChooseProgram$ (BF() AS FileList)
ON ERROR GOTO BooBoo
COLOR 12, 0
CLS
PRINT : PRINT "Creating a menu list..."
SHELL "dir /b *.bas > bas_list.txt"
DIM lst AS INTEGER, count AS INTEGER
DIM BasFile(5000) AS FileList, FError AS STRING
FError = "OK": lst = FREEFILE: OPEN "bas_list.txt" FOR INPUT AS #lst
IF FError <> "OK" THEN
   PRINT : PRINT " Could not open the directory list."
   WHILE INKEY$ > "": WEND: WHILE INKEY$ = "": WEND
   SYSTEM
END IF: count = 0: PRINT : PRINT "Reading menu list.";
WHILE (NOT EOF(lst)) AND (count < 5000)
   count = count + 1: PRINT "."; : LINE INPUT #lst, BasFile(count).F
WEND:  BasFile(0).F = STR$(count): KILL "bas_list.txt"
CALL BoxBorder: ToRun$ = ChooseProgram$(BasFile())
IF ToRun$ = "" THEN
   COLOR 9, 0: CLS : LOCATE 10, 1, 1
   PRINT " You have not chosen a program."
   PRINT " Hit any key to continue..."
   WHILE INKEY$ > "": WEND: WHILE INKEY$ = "": WEND
   SYSTEM
END IF
PRINT : PRINT "Running "; ToRun$
count = INSTR(ToRun$, ".")
IF count > 1 THEN ToRun$ = LEFT$(ToRun$, count - 1): RUN ToRun$
SYSTEM
BooBoo: FError = STR$(ERR): RESUME NEXT
SUB BoxBorder
   COLOR 14, 5
   LOCATE 7, 30
   PRINT " "; CHR$(218); CHR$(194); STRING$(14, 196);
   PRINT CHR$(194); CHR$(191); " ";
   FOR t% = 1 TO 10
      LOCATE 7 + t%, 30
      PRINT " "; STRING$(2, 179); SPACE$(14); STRING$(2, 179); " ";
   NEXT
   LOCATE 18, 30
   PRINT " "; CHR$(192); CHR$(193); STRING$(14, 196);
   PRINT CHR$(193); CHR$(217); " ";
   COLOR 13, 0: LOCATE 20
   LOCATE , 30: PRINT "Keys you may use:"
   LOCATE , 30: PRINT "DOWN, UP, HOME, END"
   LOCATE , 30: PRINT "PAGE UP, PAGE DOWN"
   LOCATE , 30: PRINT "ENTER  (select)"
   LOCATE , 30: PRINT "ESCAPE (abort)";
END SUB
FUNCTION ChooseProgram$ (BF() AS FileList)
   hi% = VAL(BF(0).F): choice% = 1: index% = 1
   DO
      FOR t% = 0 TO 9
         LOCATE 8 + t%, 33, 0
         COLOR 15, 1: IF choice% = (t% + index%) THEN COLOR 1, 7
         s$ = SPACE$(14)
         IF (t% + index%) <= hi% THEN MID$(s$, 2) = BF(t% + index%).F
         PRINT s$;
      NEXT
      DO: k$ = UCASE$(INKEY$): LOOP WHILE k$ = ""
      k% = ASC(k$): IF k% = 0 THEN k% = -ASC(MID$(k$, 2))
      SELECT CASE k%
         CASE 13: ChooseProgram$ = BF(choice%).F: EXIT DO
         CASE 27: ChooseProgram$ = "": EXIT DO
         CASE -71: choice% = 1: index% = 1
         CASE -72: choice% = choice% + (choice% > 1)
            IF choice% < index% THEN index% = choice%
         CASE -73: choice% = choice% - 10: index% = index% - 10
            IF choice% < 1 THEN choice% = 1
            IF index% < 1 THEN index% = 1
         CASE -79: choice% = hi%: index% = hi% - 9
            IF index% < 1 THEN index% = 1
         CASE -80: choice% = choice% - (choice% < hi%)
            IF index% < (choice% - 9) THEN index% = choice% - 9
         CASE -81: choice% = choice% + 10: index% = index% + 10
            IF choice% > hi% THEN choice% = hi%
            IF index% < (choice% - 9) THEN index% = choice% - 9
      END SELECT
   LOOP
END FUNCTION
'_|_|_|   end   BASMENU.BAS
