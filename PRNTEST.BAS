'_|_|_|   PRNTEST.BAS
'_|_|_|   This program will test your printer. First remove paper
'_|_|_|   and take printer offline. Run program. Add paper and
'_|_|_|   go online in individual steps. Program will locate the
'_|_|_|   printer staus port and test conditions. (updated 7/17)
'_|_|_|   No warrantees or guarantees are given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (7/17/96)
DEF SEG = &H40
PRN& = PEEK(9) AND 255
PRN& = PRN& * 256 + (PEEK(8) AND 255) + 1
'_|_|_|   Printer Status port is Printer I/O port + 1
DO
   t% = INP(PRN&): E% = 16
   IF (t% AND 16) = 16 THEN PRINT "Printer Online": E% = E% XOR 16
   IF (t% AND 32) = 32 THEN PRINT "Out of Paper": E% = E% OR 32
   IF (t% AND 128) = 0 THEN PRINT "Printer Busy": E% = E% OR 128
   IF E% > 0 THEN
      PRINT "Fix Printer and try again."
      DO: K$ = INKEY$: LOOP WHILE K$ = ""
   END IF
LOOP WHILE (E% > 0) AND (K$ <> CHR$(27))
'_|_|_|   end   PRNTEST.BAS

