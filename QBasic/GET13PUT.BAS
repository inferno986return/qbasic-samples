DECLARE FUNCTION Info$ (l%)
SCREEN 13
FOR t% = 200 TO 1 STEP -1
   CIRCLE (160, 99), t%, t% + 16
   CIRCLE (159, 99), t%, t% + 16
NEXT
LOCATE 25, 15: COLOR 10: PRINT " Hit a Key ";
WHILE INKEY$ = "": WEND
'$DYNAMIC
DIM BufferPage(1 TO 16001) AS LONG
GET (0, 0)-(319, 199), BufferPage
SCREEN 0: WIDTH 80, 43: COLOR 15, 1: CLS : LOCATE 10
FOR t% = 0 TO 9: PRINT , Info$(t%): PRINT : NEXT
WHILE INKEY$ = "": WEND
SCREEN 13
PUT (0, 0), BufferPage, PSET
ERASE BufferPage
WHILE INKEY$ = "": WEND
SCREEN 0: WIDTH 80, 25: SYSTEM

REM $STATIC
FUNCTION Info$ (l%) : SELECT CASE l%
   CASE 0
      Info$ = "First, we dimension an array to hold our data."
   CASE 1
      Info$ = "Our array must be FOUR BYTES LARGER than the data"
   CASE 2
      Info$ = "which we will be placing within it. This is to allow"
   CASE 3
      Info$ = "the GET and PUT read and write the picture dimension"
   CASE 4
      Info$ = "data for height and width, each requiring one integer."
   CASE 5
      Info$ = "Then we GET the entire screen, reading it into our RAM"
   CASE 6
      Info$ = "buffer, which we allocated with our DIM statement"
   CASE 7
      Info$ = "Now we can make any changes to the screen we like, and"
   CASE 8
      Info$ = "still restore it to what it was before."
   CASE 9
      Info$ = "Hit a key to restore the screen."
END SELECT: END FUNCTION

