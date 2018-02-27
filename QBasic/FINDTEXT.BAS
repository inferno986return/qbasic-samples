'_|_|_|   FINDTEXT.BAS
'_|_|_|   This program performs a fast file search for any given
'_|_|_|   text, selectively allowing case sensitivity.
'_|_|_|   Full overlapping is implemented for instances where the
'_|_|_|   text might span the input buffer boundaries.
'_|_|_|   No guarantees or warrantees are given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (2/28/96)
ON ERROR GOTO OOps
FileToSearch$ = "": COLOR 2, 0: CLS : LOCATE 3, 1
INPUT " Enter file name => ", FileToSearch$
IF FileToSearch$ = "" THEN CLOSE 1: END
INPUT " Enter text to find => ", text$
PRINT " Search case sensitive (y/n)? ";
IF text$ = "" THEN END:  ELSE L% = LEN(text$): Overlap$ = ""
DO
   sensitive% = INSTR(" YN", UCASE$(INKEY$))
LOOP WHILE sensitive% < 2
PRINT MID$(" YN", sensitive%, 1)
IF sensitive% = 3 THEN text$ = UCASE$(text$)
OPEN FileToSearch$ FOR BINARY AS #1
PRINT TIME$
WHILE NOT EOF(1)
   test$ = Overlap$ + INPUT$(4096, 1): TL% = LEN(test$): strpos% = 0
   IF sensitive% = 3 THEN test$ = UCASE$(test$)
   WHILE strpos% < TL%
      found% = INSTR(strpos% + 1, test$, text$)
      IF found% THEN
         strpos% = found%
         PRINT "Text found at"; LOC(1) - LEN(test$) + found%;
         PRINT "   O>=* hit a key *=<O"
         WHILE INKEY$ <> "": WEND
         k$ = "": WHILE k$ = "": k$ = INKEY$: WEND
         IF k$ = CHR$(27) THEN CLOSE 1: END
      ELSE
         Overlap$ = RIGHT$(test$, L%): strpos% = TL%
      END IF
   WEND
WEND: CLOSE 1
PRINT "end of file": PRINT TIME$: END
OOps:
 PRINT " Disk error or bad file name ["; ERR; "]"
 CLOSE 1: END
'_|_|_|   end   FINDTEXT.BAS

