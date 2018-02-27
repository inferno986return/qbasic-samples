DECLARE FUNCTION Scr2Str$ (x%, y%, l%)
TYPE MyDatFile
   NumPeople AS INTEGER: NumDogs AS INTEGER: END TYPE
CLS
DO
   DIM Mystuff AS MyDatFile, RetrieveStuff AS MyDatFile
   Mystuff.NumPeople = 15: Mystuff.NumDogs = 11
   LOCATE 1, 1: PRINT "Enter Q on Record line to exit program."
   LOCATE 2, 10: PRINT "Record :                ";
   LOCATE 2, 19: INPUT "", a$
   R% = VAL(Scr2Str$(2, 19, 15)): IF R% < 1 THEN EXIT DO
   LOCATE 3, 10: PRINT "People :                ";
   LOCATE 4, 10: PRINT "Dogs   :                ";
   LOCATE 3, 19: INPUT "", a$
   LOCATE 4, 19: INPUT "", b$
   Mystuff.NumPeople = VAL(Scr2Str$(3, 19, 15))
   Mystuff.NumDogs = VAL(Scr2Str$(4, 19, 15))
   OPEN "THINGS.DAT" FOR RANDOM AS #5 LEN = LEN(Mystuff)
   PUT #5, R%, Mystuff: GET #5, R%, RetrieveStuff: CLOSE #5
   PRINT "Number of People :"; RetrieveStuff.NumPeople
   PRINT "Number of Dogs   :"; RetrieveStuff.NumDogs
LOOP: SYSTEM

FUNCTION Scr2Str$ (x%, y%, l%)
   FOR t% = 0 TO l% - 1: a$ = a$ + CHR$(SCREEN(x%, y% + t%)): NEXT
   Scr2Str$ = a$
END FUNCTION

