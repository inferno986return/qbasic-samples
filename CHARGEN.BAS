'CHARGEN.BAS
'______  Released to the Public domain 11-6-2004 by Kurt Kuzba.
'______  Character generator for 8x12 character set.  Characters may be
'______  read into array and used with pset to place character on top of
'______  graphics without destroying background data in graphics modes.
'______  When used with mode 12h (SCREEN 12), screen is then 80x40 chars.
'______  In my test run, I produced 7x7 characters with true descenders
'______  and a one pixel buffer between characters, top and right.
'______  With modest editing, a character set of any size may be made
'______  using this code.

DIM charmap(256) AS STRING * 12
DIM bufarray(1 TO 12) AS INTEGER
DIM bitmask(8) AS INTEGER
DIM bitval(8) AS INTEGER

ulc$ = CHR$(201)  '____  These are the double line characters
urc$ = CHR$(187)  '____  for drawing a border around the
llc$ = CHR$(200)  '____  editing area
lrc$ = CHR$(188)
hzl$ = CHR$(205)
vtl$ = CHR$(186)

FOR B% = 0 TO 7         '____  Create bit AND OR values
	bitval(B%) = 2 ^ B%
	bitmask(B%) = 255 - bitval(B%)
NEXT

Char% = 0   '____  Set up basic status variables
edits% = 0

SCREEN 12   '____  Input file name to use for save file
LOCATE 18
COLOR 10
PRINT , "File being edited ";
COLOR 15
INPUT ""; Chars$
LOCATE 18, 33
PRINT " -=*[ "; Chars$; " ]*=-"

COLOR 10    '____  Display user instructions on screen
PRINT , "Use the cursors to move cursor around the editor."
PRINT , "The space key toggles space under cursor ON/OFF."
PRINT , "The ENTER key saves your character to the array."
PRINT , "Use PAGE UP and PAGE DOWN to change characters."
PRINT , "Use Delete to clear all pixels in a character."
PRINT , "Use Insert to turn on all pixels in a character."
PRINT , "Use 'r' to read from the chardump.dat file."
PRINT , "Use 's' to save to the chardump.dat file."
PRINT , "Hitting the escape key will allow you to exit."

LINE (159, 16)-(168, 29), 3, B   '____  Draw character boxes
LINE (159, 31)-(168, 48), 2, B

LOCATE 2, 23                     '____  Label character boxes
COLOR 3
PRINT "Custom character"
LOCATE 3, 23
COLOR 2
PRINT "Standard character"

DO                          '____  Editor process loop
	crsby% = 1
	crsbi% = 7               '____  Set default cursor position

	FOR B% = 1 TO 12         '____  Move character data to buffer
		bufarray(B%) = ASC(MID$(charmap(Char%), B%))
	NEXT

	DO                       '____  Editor actions loop
	  
		LOCATE 1, 1           '____  Display character data
		COLOR 15
		PRINT ulc$; STRING$(16, hzl$); urc$
		FOR byte% = 1 TO 12
			PRINT vtl$;
			bytevalue% = bufarray(byte%)
			FOR bit% = 7 TO 0 STEP -1
				bcolor% = 1
				IF (byte% = crsby%) AND (bit% = crsbi%) THEN bcolor% = 14
				COLOR bcolor%
				IF bytevalue% AND bitval(bit%) THEN
					PRINT STRING$(2, CHR$(219));
					PSET (167 - bit%, 16 + byte%), 15
				ELSE
					PRINT STRING$(2, CHR$(176));
					PSET (167 - bit%, 16 + byte%), 0
				END IF
			NEXT
			COLOR 15
			PRINT vtl$
		NEXT
		PRINT llc$; STRING$(16, hzl$); lrc$
		PRINT "Character #"; Char%; "  ";
		LOCATE 3, 21
		SELECT CASE Char%
			CASE 7, 9 TO 13:  PRINT " "
			CASE ELSE: PRINT CHR$(Char%)
		END SELECT

		DO: k$ = INKEY$: LOOP WHILE k$ = ""
		k% = INP(98)          '____  Get a keypress from user

		SELECT CASE k%        '____  Process keypresses

			CASE IS = 1:       '____  Exit on Escape key
				EXIT DO
			CASE IS = 72:      '____  Move cursor up on up arrow
				crsby% = crsby% - 1 - 12 * (crsby% = 1)
			CASE IS = 75:      '____  Move cursor left on left arrow
				crsbi% = (crsbi% + 1) MOD 8
			CASE IS = 77:      '____  Move cursor right on right arrow
				crsbi% = crsbi% - 1 - 8 * (crsbi% = 0)
			CASE IS = 80:      '____  Move cursor down on down arrow
				crsby% = crsby% MOD 12 + 1
			CASE IS = 49, 73:  '____  Next character. Exit actions loop
				Char% = (Char% + 1) MOD 256: EXIT DO
			CASE IS = 25, 81:  '____  Previous character. Exit actions loop
				Char% = Char% - 1 - 256 * (Char% = 0): EXIT DO
			CASE IS = 19:      '____  Read character data from file
				fh% = FREEFILE
				OPEN Chars$ FOR BINARY AS fh%
				FOR B% = 0 TO 255
					charmap(B%) = INPUT$(12, fh%)
				NEXT
				CLOSE fh%
				EXIT DO         '____  Exit actions loop
			CASE IS = 31:      '____  Save character data to file
				edits% = 0      '____  Don't remind user to save file
				fh% = FREEFILE
				OPEN Chars$ FOR OUTPUT AS fh%
				FOR B% = 0 TO 255
					PRINT #fh%, charmap(B%);
				NEXT
				CLOSE fh%
				EXIT DO         '____  Exit actions loop
			CASE IS = 57:      '____  Toggle pixel under cursor ON/OFF
				IF bufarray(crsby%) AND bitval(crsbi%) THEN
					bufarray(crsby%) = bufarray(crsby%) AND bitmask(crsbi%)
				ELSE
					bufarray(crsby%) = bufarray(crsby%) OR bitval(crsbi%)
				END IF
			CASE IS = 28:      '____  Save edited character to data array
				edits% = -1     '____  Remind user to save file
				FOR B% = 1 TO 12
					MID$(charmap(Char%), B%, 1) = CHR$(bufarray(B%))
				NEXT
			CASE IS = 82:      '____  Toggle all bits ON. Exit actions loop
				FOR B% = 1 TO 12
					MID$(charmap(Char%), B%, 1) = CHR$(255)
				NEXT
				EXIT DO
			CASE IS = 83:      '____  Toggle all bits OFF. Exit actions loop
				FOR B% = 1 TO 12
					MID$(charmap(Char%), B%, 1) = CHR$(0)
				NEXT
				EXIT DO
		END SELECT

	LOOP                     '____  End actions loop

	IF k$ = CHR$(27) THEN EXIT DO
									 '____  Exit process loop if Escape key pressed
LOOP                        '____  End process loop

IF edits% THEN              '____  If there are unsaved edits, save file
	CLS
	LOCATE 15
	COLOR 10
	PRINT , "Save your character map? (Y/N) ";
	DO
		DO: k$ = LCASE$(INKEY$): LOOP WHILE k$ = ""
	LOOP WHILE INSTR("yn " + CHR$(27), k$) < 1
	PRINT k$
	IF k$ = "y" THEN
		fh% = FREEFILE
		OPEN Chars$ FOR OUTPUT AS fh%
		FOR B% = 0 TO 255
			PRINT #fh%, charmap(B%);
		NEXT
		CLOSE fh%
	END IF
END IF

SCREEN 0                    '____  Return to text screen and end program
SYSTEM

