'From: CARL GORRINGE             Conference: FidoQBasic   (139)
'Date: 03-26-96 16:21              BBS Name: Exec-PC
'Subject: Scroll Bar routine            BBS ID: EXECPC
'---------------------------- CUT HERE -----------------------------
DECLARE SUB TextVertBar (X%, Y1%, Y2%, ColFor%, ColBak%, NumLines%, First%, Last%)
SCREEN 0
CLS
COLOR 11
PRINT "Press the Up/Down Arrow Keys, ESC to Quit"
NumLines% = 20
First% = 1
Last% = 4
DO
   CALL TextVertBar(50, 2, 22, 0, 7, NumLines%, First%, Last%)
   DO
      I$ = INKEY$
   LOOP UNTIL I$ <> ""
   IF (I$ = CHR$(0) + "P") AND (Last% < NumLines%) THEN
      '-- Down Arrow Key Pressed --
      LOCATE 2, 1: PRINT " Down ";
      First% = First% + 1
      Last% = Last% + 1
   ELSEIF (I$ = CHR$(0) + "H") AND (First% > 1) THEN
      '-- Up Arrow Key Pressed --
      LOCATE 2, 1: PRINT "  Up  ";
      First% = First% - 1
      Last% = Last% - 1
   END IF
LOOP UNTIL I$ = CHR$(27)
END
'---------------------------- CUT HERE -----------------------------
'* Origin: CAXIUS BBS - San Leandro, CA [510-895-6961] (1:215/42)
   

SUB TextVertBar (X%, Y1%, Y2%, ColFor%, ColBak%, NumLines%, First%, Last%)
'::: (c) Carl Gorringe 7/11/95 :::
'-----------------------------------------------------------------------
'  Draws a Vertical Scroll Bar in Text Mode
'-----------------------------------------------------------------------
'  (X%, Y1% to Y2%) is Position of Scroll Bar.
'  ColFor%, ColBak% are Colors used.
'  NumLines% = Total Number of Lines in Text Box. (must be >= Length of Bar!)
'  First% = First or Top line number in Text Box. (starts at 1!)
'  Last% = Last or Bottom line number in Text Box. (must be <= NumLines% !)
'-----------------------------------------------------------------------
CursorV% = CSRLIN
CursorH% = POS(0)
'--- Display Arrow Symbols ---
   COLOR ColFor%, ColBak%
   LOCATE Y1%, X%
      PRINT CHR$(24);
   LOCATE Y2%, X%
      PRINT CHR$(25);
'--- Display Inner Scroll Bar ---
   LenBar% = Y2% - Y1% - 1
   IF LenBar% > 1 THEN
      '-- Display Blank Bar --
      FOR a% = (Y1% + 1) TO (Y2% - 1)
         LOCATE a%, X%
         PRINT CHR$(176);
      NEXT a%
      '-- Calculate Moving Bar --
      Ratio1! = First% / NumLines%
      Ratio2! = Last% / NumLines%
         IF Ratio2! > 1 THEN Ratio2! = 1
      BarY1% = INT(Ratio1! * LenBar%) + Y1% + 1
      BarY2% = INT(Ratio2! * LenBar%) + Y1%
         IF BarY2% < BarY1% THEN BarY2% = BarY1%
      '-- Display Moving Bar --
      FOR a% = BarY1% TO BarY2%
         LOCATE a%, X%
         PRINT CHR$(178);
      NEXT a%
   END IF
LOCATE CursorV%, CursorH%
END SUB

