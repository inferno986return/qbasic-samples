 '|======================  begin ANSIFUNC.BAS  =======================|
 '|  This is a QBasic program to convert an ANSI file to a QBasic     |
 '|  FUNCTION that will return the file as a string. In the program   |
 '|  this program produces, the function is called and the resulting  |
 '|  string is output to CONSOLE, which requires ANSI.SYS loaded.     |
 '|          Released to the Public Domain by Kurt Kuzba              |
 '|===================================================================|
 ' set error trap, set colors, clear screen, q$ = quote mark ==========
ON ERROR GOTO Done: COLOR 2, 0: CLS : q$ = CHR$(34)
 ' create a string for the input boxes  ===============================
i$ = STRING$(16, " ") + STRING$(15, CHR$(29))
 ' print message and get filename from user, open file for input ======
LOCATE 5, 10: PRINT "We will turn an ANSI file into a QBasic function"
LOCATE 6, 10: PRINT "which returns the entire ANSI file as a string."
LOCATE 8, 10: PRINT "Enter ANSI name [ EX. MYPIC.ANS ] ";
COLOR 1, 7: PRINT i$; : INPUT "", ans$: COLOR 2, 0
IF ans$ <> "" THEN OPEN ans$ FOR INPUT AS #1:  ELSE GOTO Done
 ' get name of function to create from user ===========================
LOCATE 10, 10: PRINT "Enter Function name [ EX. Pic1$ ] ";
COLOR 1, 7: PRINT i$; : INPUT "", func$: COLOR 2, 0
 ' make FUNCTION name and open file for input =========================
IF func$ = "" THEN GOTO Done
IF RIGHT$(func$, 1) <> "$" THEN func$ = func$ + "$"
name$ = func$ + ".bas": OPEN name$ FOR OUTPUT AS #2
 ' write QBasic FUNCTION to file ======================================
PRINT #2, "DECLARE FUNCTION "; func$; "()"
PRINT #2, "open "; q$; "CONS:"; q$; " FOR OUTPUT AS #1"
PRINT #2, "PRINT #1, "; func$
PRINT #2, "FUNCTION "; func$
PRINT #2, "t$ = "; q$; q$
 ' process ANSI to create QBasic routine to construct string ==========
WHILE NOT EOF(1)
   WHILE NOT EOF(1)
      PRINT #2, "t$=t$+"; q$; : C$ = ""
      WHILE C$ <> CHR$(10) AND NOT EOF(1)
         C$ = INPUT$(1, #1)
         IF C$ = CHR$(13) THEN
            C$ = q$ + " + CHR$(13) + CHR$(10)" + CHR$(13) + CHR$(10)
         END IF
         IF C$ <> CHR$(10) THEN PRINT #2, C$;
      WEND
   WEND
WEND
 ' finish writing QBasic FUNCTION to file =============================
PRINT #2, q$
PRINT #2, func$; "=t$"
PRINT #2, "END FUNCTION"
 ' done, tell user new program is ready, wait for key =================
LOCATE 12, 10: COLOR 10
PRINT "Your new program, "; name$; ", containing the FUNCTION"
LOCATE 13, 10: PRINT func$; ", to display "; ans$; ", is ready to test."
LOCATE 14, 10: PRINT "ANSI.SYS must be loaded for "; name$; " to work."
Done:  CLOSE 1: CLOSE 2: C% = 0: i$ = ""
WHILE i$ = ""
   C% = (C% + 1) AND 15: LOCATE 15, 10: COLOR C%: PRINT "Hit a Key"
   i$ = INKEY$
WEND: COLOR 2
 '|  ===============  end ANSIFUNC.BAS  ==============================|

