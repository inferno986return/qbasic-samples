============================================================================
     To: CHARLES GODARD                Number: 101      Refer: 0023    
   From: Chris Gunn                Conference: FidoQBasic   (139)
   Date: 11-24-96 13:34              BBS Name: Exec-PC
Subject: QB 4.5 Error Codes            BBS ID: EXECPC
----------------------------------------------------------------------------

 CG> I liked your program.  Like Chris Gunn, I too will give it an A+.
 CG> Not only was it very good, but we also got to see Chris Gunn in
 CG> action without his Libraries.  That's not a flame Chris, just
 CG> Redneck humor. <Grin>
Thanks Charles,
I use QuickBasic somewhat like the old Fortran.  I've added a whole bunch of 
subroutines and functions I use like they were part of QuickBasic.
 CG> ErrorMessages:
 CG> DATA 1 ," NEXT without FOR              "
 CG> DATA 2 ," Syntax error                  "
 CG> DATA 3 ," RETURN without GOSUB          "
Since DATA text still takes up program space and a FOR loop, I prefer using a 
faster subroutine or function with a SELECT CASE structure.
Whoop$ = ErrorMsg$(ERR)   or  PRINT ErrorMsg$(EFlag%)
or
CALL ErrorMsgs(EFlag%, Whoop$)
I've put my whole list at the foot since it's a big help debugging as well as 
telling the user what went astray.
Chris
======================================================================
FUNCTION ErrorMsg$(EFlag%)
IF EFlag% = 0 THEN EXIT FUNCTION
SELECT CASE EFlag%
        CASE 1:  VErr$ = "SHARE not active"
        CASE 2:  VErr$ = "Syntax error"
        CASE 3:  VErr$ = "RETURN without GOSUB"
        CASE 4:  VErr$ = "Out of Data"
        CASE 5:  VErr$ = "Illegal Function Call"
        CASE 6:  VErr$ = "Overflow"
        CASE 7:  VErr$ = "Out of Memory"
        CASE 8:  VErr$ = "Label not defined"
        CASE 9:  VErr$ = "Subscript out of range"
        CASE 10: VErr$ = "Duplicate definition"
        CASE 11: VErr$ = "Division by Zero"
        CASE 12: VErr$ = "Illegal in direct mode"
        CASE 13: VErr$ = "Type mismatch"
        CASE 14: VErr$ = "Out of string space"
        CASE 15: VErr$ = "Fossil not available"
        CASE 16: VErr$ = "String formula too complex"
        CASE 17: VErr$ = "Carrier Dropped!"
        CASE 18: VErr$ = "Function not defined"
        CASE 19: VErr$ = "No RESUME"
        CASE 20: VErr$ = "RESUME without error"
        CASE 24: VErr$ = "Modem or Device Timeout"
        CASE 25: VErr$ = "Modem or Device fault"
        CASE 26: VErr$ = "FOR without NEXT"
        CASE 27: VErr$ = "Out of paper"
        CASE 29: VErr$ = "WHILE without WEND"
        CASE 30: VErr$ = "WEND without WHILE"
        CASE 33: VErr$ = "Duplicate label"
        CASE 35: VErr$ = "Subprogram NOT defined"
        CASE 37: VErr$ = "Argument-count mismatch"
        CASE 38: VErr$ = "Array not defined"
        CASE 40: VErr$ = "Variable required"
        CASE 50: VErr$ = "Buffer or Field overflow"
        CASE 51: VErr$ = "Internal System error"
        CASE 52: VErr$ = "Bad file name or number"
        CASE 53: VErr$ = "File not found"
        CASE 54: VErr$ = "Bad file mode"
        CASE 55: VErr$ = "File already open"
        CASE 56: VErr$ = "FIELD statement active"
        CASE 57: VErr$ = "Modem or Device I/O error"
        CASE 58: VErr$ = "File already exists"
        CASE 59: VErr$ = "Bad record length"
        CASE 61: VErr$ = "Disk full"
        CASE 62: VErr$ = "Input past end of file"
        CASE 63: VErr$ = "Bad record number"
        CASE 64: VErr$ = "Invalid filename"
        CASE 67: VErr$ = "Too many files"
        CASE 68: VErr$ = "Drive/Device unavailable"
        CASE 69: VErr$ = "Comm buffer overflow"
        CASE 70: VErr$ = "Permission denied"
        CASE 71: VErr$ = "Disk/Printer not ready"
        CASE 72: VErr$ = "Disk media error"
        CASE 73: VErr$ = "Feature not available"
        CASE 74: VErr$ = "Rename across disks"
        CASE 75: VErr$ = "Path/File access error"
        CASE 76: VErr$ = "Path not found"
        CASE 80: VErr$ = "Zero length file"
        CASE 81: VErr$ = "Invalid name"
        CASE 82: VErr$ = "Table not found"
        CASE 83: VErr$ = "Index not found"
        CASE 84: VErr$ = "Invalid column"
        CASE 85: VErr$ = "No current record"
        CASE 86: VErr$ = "Duplicate value for unique index"
        CASE 87: VErr$ = "Invalid operation on null index"
        CASE 88: VErr$ = "Database needs repair"
        CASE ELSE: VErr$ = "UNIDENTIFIED ERROR"
END SELECT
ErrorMsg$ = VErr$ + "!"
END FUNCTION
--- FMail 0.96â
 * Origin: BIZynet BBS - Worldwide business via the E-Ways (1:15/55.1)
                                
