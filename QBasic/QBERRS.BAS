'============================================================================
'     To: ALL                           Number: 58       Refer: 0   
'   From: JEAN-SEBASTIEN CARLE      Conference: QUIK_BAS -Qui (298)
'   Date: 05-11-99 03:04              BBS Name: YE OLDE PAWN SHOPPE ]I[
'Subject: On Error                      BBS ID: YOPSIII
'----------------------------------------------------------------------------
'
' I took the time to type out all of the documented qbasic
' error codes... thought this could be usefull to someone...
'***  Updated to use a string function on 11/23/1999 by Kurt Kuzba

DECLARE FUNCTION ErrorString$ (programerror AS INTEGER)
REM //////////////////////////////////////////////////////////
REM /// Error trapping                                     ///
REM //////////////////////////////////////////////////////////

ON ERROR GOTO ErrorRoutine
CLS
OPEN "nonfile" FOR INPUT AS #1 '/// generates error 53
SYSTEM

ErrorRoutine:
  PRINT "Error"; STR$(ERR); ": "; ErrorString$(ERR)
  RESUME NEXT

' -=<Captain Red
' -=<Head of Coding Department
' -=<Syndikate 13
'... Open mouth, insert foot, echo internationally.
'--- Blue Wave/DOS v2.30
' * Origin: The BandMaster, Vancouver, BC (604-266-7754) (1:153/7715)          
                                  

FUNCTION ErrorString$ (programerror AS INTEGER)
  SELECT CASE programerror
    CASE 1: ErrorString$ = "NEXT without FOR"
    CASE 2: ErrorString$ = "Syntax Error"
    CASE 3: ErrorString$ = "RETURN wihout GOSUB"
    CASE 4: ErrorString$ = "Out of DATA"
    CASE 5: ErrorString$ = "Illegal function call"
    CASE 6: ErrorString$ = "Overflow"
    CASE 7: ErrorString$ = "Out of memory"
    CASE 8: ErrorString$ = "Label not defined"
    CASE 9: ErrorString$ = "Subcript out of range"
    CASE 10: ErrorString$ = "Duplicate definition"
    CASE 11: ErrorString$ = "Division by zero"
    CASE 12: ErrorString$ = "Illegal in direct mode"
    CASE 13: ErrorString$ = "Type mismatch"
    CASE 14: ErrorString$ = "Out of string space"
    CASE 15: ErrorString$ = "Undefined error"
    CASE 16: ErrorString$ = "String formula too complex"
    CASE 17: ErrorString$ = "Cannot continue"
    CASE 18: ErrorString$ = "Function not defined"
    CASE 19: ErrorString$ = "No RESUME"
    CASE 20: ErrorString$ = "RESUME without error"
    CASE 24: ErrorString$ = "Device timeout"
    CASE 25: ErrorString$ = "Device fault"
    CASE 26: ErrorString$ = "FOR without NEXT"
    CASE 27: ErrorString$ = "Out of paper"
    CASE 28: ErrorString$ = "Undefined error"
    CASE 29: ErrorString$ = "WHILE without WEND"
    CASE 30: ErrorString$ = "WEND without WHILE"
    CASE 35: ErrorString$ = "Subprogram not defined"
    CASE 36: ErrorString$ = "Undefined error"
    CASE 37: ErrorString$ = "Argument-count mismatch"
    CASE 38: ErrorString$ = "Array not defined"
    CASE 39: ErrorString$ = "CASE ELSE expected"
    CASE 40: ErrorString$ = "Variable required"
    CASE 50: ErrorString$ = "FIELD overflow"
    CASE 51: ErrorString$ = "Internal error"
    CASE 52: ErrorString$ = "Bad file name or number"
    CASE 53: ErrorString$ = "File not found"
    CASE 54: ErrorString$ = "Bad file mode"
    CASE 55: ErrorString$ = "File already open"
    CASE 56: ErrorString$ = "FIELD statement active"
    CASE 57: ErrorString$ = "Device I/O error"
    CASE 58: ErrorString$ = "File already exists"
    CASE 59: ErrorString$ = "Bad record length"
    CASE 60: ErrorString$ = "Undefined error"
    CASE 61: ErrorString$ = "Disk full"
    CASE 62: ErrorString$ = "Input past end of file"
    CASE 63: ErrorString$ = "Bad record number"
    CASE 64: ErrorString$ = "Bad file name"
    CASE 67: ErrorString$ = "Too many files"
    CASE 68: ErrorString$ = "Device unavailable"
    CASE 69: ErrorString$ = "Communication-buffer overflow"
    CASE 70: ErrorString$ = "Permission denied"
    CASE 71: ErrorString$ = "Disk not ready"
    CASE 72: ErrorString$ = "Disk-media error"
    CASE 73: ErrorString$ = "Advanced feature unavailable"
    CASE 74: ErrorString$ = "Rename across disks"
    CASE 75: ErrorString$ = "Path/File access error"
    CASE 76: ErrorString$ = "Path not found"
    CASE 97: ErrorString$ = "Unknown error"
    CASE ELSE: ErrorString$ = "Undefined error"
  END SELECT
END FUNCTION

