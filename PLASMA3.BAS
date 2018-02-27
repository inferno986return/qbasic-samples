'============================================================================
'     To: Rob Scott                     Number: 80       Refer: 0004  
'   From: Rodney Taylor             Conference: FidoQBasic   (139)
'   Date: 06-22-96 15:46              BBS Name: Exec-PC
'Subject: Re: Plasma [1/2]              BBS ID: EXECPC
'----------------------------------------------------------------------------
'
' >>> Part 1 of 2...
' JM> Plasma screen displays? What, exactly, do you mean?
' RS> I am looking for still plasma that drawing prog's can make..
'  I got one.  Pulled it out of an ABC packet.
'  It does a bit of animation, but you can cut off that part before
'  it starts to rotate the palettes

'PLASMA GENERATOR By: Ian Remmler
'------ ---------
DECLARE FUNCTION Rand% ()
DECLARE FUNCTION PntClr% (x%, y%)
DECLARE SUB Dot (x%, y%, c%)
DECLARE SUB Adjust (xa%, ya%, x%, y%, xb%, yb%)
DECLARE SUB SubDivide (x1%, y1%, x2%, y2%)
DECLARE SUB MakePal (col%(), x%, y%)
DEFINT A-Z
DIM r(-8 TO 264), g(-8 TO 264), b(-8 TO 264)  'For the palette values.
COMMON SHARED F  'Graininess factor
IF INSTR(COMMAND$, "/?") OR INSTR(COMMAND$, "-?") THEN GOTO PrntHelp
'REM above line if you are using QBasic.
RANDOMIZE TIMER
DEF SEG = &HA000
F = 10: ra = 1: rb = 255: d = 1: s = 1
SCREEN 13
Start:
GOSUB PalSet  'Get a random palette to start with.
Dot 0, 0, RND * 255 + 1
Dot 319, 0, RND * 255 + 1
Dot 319, 199, RND * 255 + 1
Dot 0, 199, RND * 255 + 1
SubDivide 0, 0, 319, 199
BEEP
rotate:
	DO
		a$ = INKEY$
		IF LEN(a$) THEN GOSUB keys  'Check to see if an option key was pushed.
		FOR a = 1 TO 9
			'This loop is needed to keep the palette from screwing up when
			'palette rotation speed is > 1.
			r(255 + a) = r(a): g(255 + a) = g(a): b(255 + a) = b(a)
			r(1 - a) = r(255 - a): g(1 - a) = g(255 - a): b(1 - a) = b(255 - a)
		NEXT
		FOR t = ra TO rb STEP d  'This loop rotates the palette.
			SWAP r(t), r(t + d * s)
			SWAP g(t), g(t + d * s)
			SWAP b(t), b(t + d * s)
		NEXT
		GOSUB Set  'Output the new palette values to the VGA adapter.
	LOOP
PalSet:
	'This routine uses a technique similar to the plasma generator
	'itself to create a smooth palette.
	FOR a = 1 TO 255
		r(a) = -1: g(a) = -1: b(a) = -1
	NEXT
	r(1) = INP(64) \ 4: g(1) = INP(64) \ 4: b(1) = INP(64) \ 4
	r(255) = r(1) + Rand: g(255) = g(1) + Rand: b(255) = b(1) + Rand
	MakePal r(), 1, 255
	MakePal g(), 1, 255
	MakePal b(), 1, 255
	GOSUB Set
RETURN
Set:
	'Dump all the palette values to the VGA adapter.
	OUT &H3C8, 1
	WAIT &H3DA, 8
	FOR c = 1 TO 255
		OUT &H3C9, r(c)
		OUT &H3C9, g(c)
		OUT &H3C9, b(c)
	NEXT
RETURN
keys:
	SELECT CASE UCASE$(a$)
		CASE " "  'Reverse direction of palette rotation.
			SWAP ra, rb: d = -d
		CASE CHR$(13)  'Make a new random palette.
			GOSUB PalSet
		CASE "F"  'Change the "graininess" (Amount of randomness).
			SCREEN 0: WIDTH 80, 25
			PRINT "Current = "; F
			PRINT : INPUT "Enter new graininess factor. - ", F
			SCREEN 13: GOTO Start
		CASE CHR$(49) TO CHR$(57)  'Change palette rotation speed.
			s = VAL(a$)
		CASE CHR$(27)  'Exit the program.
			SCREEN 0: WIDTH 80, 25: END
	END SELECT
RETURN
PrntHelp:
	PRINT "PLASMA GENERATOR - By: Ian Remmler"
	PRINT "------ ---------"
	PRINT "[ENTER] : Create a random palette."
	PRINT "[SPACE] : Reverse direction of palette rotation."
	PRINT "1 - 9   : Change palette rotation speed."
	PRINT "F       : Enter new graininess factor."
	PRINT "[ESC]   : Exit the program."
	PRINT "/? or -? on command line to display help."
	END
'----------------------------Cut Here---------------------------
'Give it a go, see what you think.
'        Rod Taylor
'... My other computer is a Commodore 64.
'~~~ TGWave v1.12 [NR]
'--- GEcho 1.11+
' * Origin: The Other Domain * Hawkestone ON * (705) 487-6520 (1:252/107)
'       

SUB Adjust (xa, ya, x, y, xb, yb)
	IF PntClr(x, y) <> 0 THEN EXIT SUB
	q = ABS(xa - xb) + ABS(ya - yb)
	v = (PntClr(xa, ya) + PntClr(xb, yb)) \ 2 + (Rand * q * F) \ 10
	IF v < 1 THEN v = 1
	IF v > 255 THEN v = 255
	Dot x, y, v
END SUB

SUB Dot (x, y, c)  'Fast replacement for PSET.
	POKE y * 320& + x, c
END SUB

SUB MakePal (col(), x, y)
	z = (x + y) \ 2
	IF col(z) <> -1 THEN EXIT SUB
	q = ABS(x - y) \ 8
	c = (col(x) + col(y)) \ 2 + Rand * q
	IF c < 0 THEN c = 0
	IF c > 63 THEN c = 63
	col(z) = c
	MakePal col(), x, z
	MakePal col(), y, z
END SUB

FUNCTION PntClr (x, y)  'Fast replacement for POINT.
	PntClr = PEEK(y * 320& + x)
END FUNCTION

FUNCTION Rand
	'This returns a value of -1, 0, or 1 and is much faster than RND.
	Rand = INP(64) MOD 3 - 1
END FUNCTION

SUB SubDivide (x1, y1, x2, y2)
	IF (x2 - x1 < 2) AND (y2 - y1 < 2) THEN EXIT SUB
	x = (x1 + x2) \ 2
	y = (y1 + y2) \ 2
	Adjust x1, y1, x, y1, x2, y1
	Adjust x1, y2, x, y2, x2, y2
	Adjust x2, y1, x2, y, x2, y2
	Adjust x1, y1, x1, y, x1, y2
	IF PntClr(x, y) = 0 THEN
		v = PntClr(x1, y1) + PntClr(x2, y1) + PntClr(x2, y2)
		v = v + PntClr(x1, y2) + PntClr(x1, y) + PntClr(x, y1)
		v = v + PntClr(x2, y) + PntClr(x, y2)
		v = v \ 8
		Dot x, y, v
		IF INKEY$ = CHR$(27) THEN END
	END IF
	SubDivide x1, y1, x, y
	SubDivide x, y, x2, y2
	SubDivide x, y1, x2, y
	SubDivide x1, y, x, y2
END SUB

