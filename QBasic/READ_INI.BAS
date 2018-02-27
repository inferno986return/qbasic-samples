'_|_|_|   READ_INI.BAS   PUBLIC DOMAIN   Kurt Kuzba   2/26/1998
'_|_|_|   No warrantee or Guarantee given or implied.
'_|_|_|   A simple program to illustrate the use of a .INI file
'_|_|_|   with programs in QBasic or Quick Basic.
DECLARE SUB ProgramINI (ini%)
DECLARE SUB StrReverse (a$)
ON ERROR GOTO OOps
DIM SHARED DefaultDir$, DefaultTxtClr%, DefaultBckClr%
DIM SHARED PWord$, FError$
FError$ = "ok"
Infile% = FREEFILE
OPEN "initest.ini" FOR INPUT AS #Infile%
ProgramINI Infile%
SHELL "dir " + DefaultDir$ + "*.exe"
BEEP: DO: k$ = INKEY$: LOOP WHILE k$ = ""
PRINT "The Password is... "; PWord$
PRINT "The Directory is... "; DefaultDir$
BEEP: DO: k$ = INKEY$: LOOP WHILE k$ = ""
OOps: FError$ = STR$(ERR): RESUME NEXT
SUB ProgramINI (ini%)
   DefaultDir$ = "C:\"
   DefaultTxtClr% = 15
   DefaultBckClr% = 1
   PWord$ = "friend"
   IF FError$ = "ok" THEN
      WHILE NOT EOF(ini%)
         LINE INPUT #ini%, text$
         parm$ = UCASE$(RTRIM$(LEFT$(text$, INSTR(text$, " "))))
         StrReverse text$
         cmnd$ = RTRIM$(LEFT$(text$, INSTR(text$, " ")))
         StrReverse cmnd$
         SELECT CASE parm$
            CASE "DIRECTORY": DefaultDir$ = cmnd$
            CASE "PASSWORD": PWord$ = cmnd$
            CASE "TEXTCOLOR": DefaultTxtClr% = VAL(cmnd$)
            CASE "BACKCOLOR": DefaultBckClr% = VAL(cmnd$)
         END SELECT
      WEND
      CLOSE ini%
   ELSE
      PRINT "No INI file"
   END IF
   COLOR DefaultTxtClr%, DefaultBckClr%
   CHDIR DefaultDir$
END SUB
SUB StrReverse (a$)
   L% = LEN(a$) + 1: Z% = (L% - 1) \ 2
   FOR C% = 1 TO Z%
      T$ = MID$(a$, C%, 1)
      MID$(a$, C%, 1) = MID$(a$, L% - C%, 1)
      MID$(a$, L% - C%, 1) = T$
   NEXT
END SUB
 
