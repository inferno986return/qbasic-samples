'_|_|_|   TESTDISK.BAS
'_|_|_|   This program demonstrates testing disk ready condition.
'_|_|_|   Tested with CD ROM, floppy, IDE fixed disk.
'_|_|_|   Tested under DOS 5.0, WIN'95, DOS 7 (WIN'95 DOS).
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (12/13/96)
ON ERROR GOTO Oops
DO
   COLOR 11, 0
   PRINT CHR$(13)
   PRINT "This routine will test presence of a disk drive."
   PRINT "Enter ESC to exit this program, or enter"
   PRINT "the letter of the drive to be tested => ";
   DO: k$ = UCASE$(INKEY$)
   LOOP WHILE (k$ <> CHR$(27)) AND (k$ < "A") OR (k$ > "Z")
   IF k$ <> CHR$(27) THEN
      PRINT k$
      Disk.Error.Data = 0
      tst% = FREEFILE
      IF tst% = 0 THEN
         PRINT "No file handles available."
         END
      END IF
      OPEN k$ + ":\TESTDISK.NUL" FOR INPUT AS tst%
      SELECT CASE Disk.Error.Data
         CASE 53: COLOR 15, 1: CLOSE tst%
            PRINT " Drive is ready for access. ";
         CASE ELSE: COLOR 14, 4: PRINT " Drive is not ready. ";
            PRINT "["; LTRIM$(STR$(Disk.Error.Data)); "]";
      END SELECT
   END IF
LOOP WHILE k$ <> CHR$(27)
SYSTEM
Oops:
   Disk.Error.Data = ERR: RESUME NEXT
'_|_|_|   end   TESTDISK.BAS
 
