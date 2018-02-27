'_|_|_|   NULSHELL.BAS
'_|_|_|   This program demonstrates the use of DOS redirection of
'_|_|_|   program output to a NUL device.
'_|_|_|   Released to the   Public Domain   by Kurt Kuzba
'_|_|_|
COLOR 7, 0
CLS
SCREEN , , 1, 1
CLS
LOCATE 10
COLOR 14, 2
FOR T% = 1 TO 3: LOCATE , 30: PRINT STRING$(20, CHR$(176)): NEXT
COLOR 31, 1: LOCATE 11, 32: PRINT "  Reading File  "
FOR T% = 1 TO 30: SHELL "type c:\autoexec.bat > nul": NEXT
COLOR 15, 1: LOCATE 11, 32: PRINT " [ Hit RETURN ] "
WHILE INKEY$ = "": WEND
SCREEN , , 0, 0
'_|_|_|   end NULSHELL.BAS

