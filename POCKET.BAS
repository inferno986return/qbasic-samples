'_|_|_|   POCKET.BAS   PUBLIC DOMAIN   Kurt Kuzba  (11/23/1999)
'_|_|_|   A demonstration of the usefulness of fixed length
'_|_|_|   strings in developing text apps in QBasic.
TYPE pocket
   L AS STRING * 80
   fore AS INTEGER   'Use of this data type allows a doubly linked list
   back AS INTEGER
END TYPE
CLS
CLEAR
'$DYNAMIC
PRINT "  This is prior to array creation"
PRINT "  Non-String Arrays ::"; FRE(-1)
PRINT "  Stack Space       ::"; FRE(-2)
PRINT "  String Space      ::"; FRE(0)
DIM TXT$(100)              'This is working space for the editor
DIM TEXT(1 TO 700) AS pocket
FOR t% = 1 TO 700: TEXT(t%).L = STRING$(80, "x"): NEXT
COLOR 4, 0: PRINT TEXT(1).L;
COLOR 15, 1: PRINT
PRINT "  Non-String Arrays ::"; FRE(-1)
PRINT "  Stack Space       ::"; FRE(-2)
PRINT "  String Space      ::"; FRE(0)
PRINT "  You can see how this allows more space in your editor."
PRINT "  We have 700 lines of fixed length text strings available."
COLOR 4, 0: PRINT TEXT(1).L

