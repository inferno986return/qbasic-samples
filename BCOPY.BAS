'_|_|_|   BCOPY.BAS
'_|_|_|   Performs a binary file copy.
'_|_|_|   No guarantee or warrantee is given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (12/7/96)
ON ERROR GOTO Oops
DO
   SCREEN 0
   WIDTH 80, 25
   COLOR 10, 0
   CLS
   DIM Disk.Error.Dat AS INTEGER
   INPUT "Enter old file path => ", SrcPath$
   SrcPath$ = RTRIM$(LTRIM$(SrcPath$))
   IF (SrcPath$ <> "") AND (RIGHT$(SrcPath$, 1) <> "\") THEN
      SrcPath$ = SrcPath$ + "\"
   END IF
   INPUT "Enter new file path => ", DstPath$
   DstPath$ = RTRIM$(LTRIM$(DstPath$))
   IF (DstPath$ <> "") AND (RIGHT$(DstPath$, 1) <> "\") THEN
      DstPath$ = DstPath$ + "\"
   END IF
   INPUT "Enter name of file  => ", FileToCopy$
   IF FileToCopy$ = "" THEN END
   Source$ = SrcPath$ + FileToCopy$
   Destination$ = DstPath$ + FileToCopy$
   src% = FREEFILE
   IF src% = 0 THEN
      PRINT "No handles available."
      END
   END IF
   Disk.Error.Dat = 0
   OPEN Source$ FOR INPUT AS #src%
   IF Disk.Error.Dat <> 0 THEN
      PRINT "Error opening "; Source$; "."
      END
   END IF
   CLOSE src%
   OPEN Source$ FOR BINARY AS #src%
   dst% = FREEFILE
   IF dst% = 0 THEN
      PRINT "No handles available."
      CLOSE src%
      END
   END IF
   Disk.Error.Dat = 0
   OPEN Destination$ FOR OUTPUT AS #dst%
   IF Disk.Error.Dat <> 0 THEN
      PRINT "Error opening "; Destination$; "."
      CLOSE src%
      END
   END IF
   COLOR 15, 1
   PRINT " Each . represents an 8K block. "
   WHILE NOT EOF(src%)
      dat$ = INPUT$(8192, src%)
      PRINT #dst%, dat$;
      PRINT ".";
   WEND
   COLOR 10, 0
   PRINT
   CLOSE src%
   CLOSE dst%
   IF DstPath$ = "" THEN
      DstPath$ = "current directory"
   END IF
   PRINT FileToCopy$; " copied to "; DstPath$; "."
   PRINT "Copy another file (Y/N) ";
   DO: k$ = INKEY$
   LOOP WHILE INSTR(" YN", UCASE$(k$)) < 2
LOOP WHILE UCASE$(k$) = "Y"
END
Oops:
   Disk.Error.Dat = ERR
   RESUME NEXT
'_|_|_|   end   BCOPY.BAS

