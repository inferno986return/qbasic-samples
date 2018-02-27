'_|_|_|   VESATEST.BAS
'_|_|_|   A demo of line drawing and palette manipulation in
'_|_|_|   VESA mode 101h (640x480x256) and 103h (800x600x256)
'_|_|_|   for QB45. Use QB /L
'_|_|_|   Includes putpixel() and Bline() SUBs.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (3/7/1998)
'$INCLUDE: 'qb.bi'
DECLARE SUB Bline (x%, y%, x2%, y2%, c%)
DECLARE SUB putpixel (x%, y%, c%)
DECLARE SUB vesamode (mode&)
DECLARE SUB vesatest (mode&)
TYPE PaletteData: r AS INTEGER: g AS INTEGER: b AS INTEGER: END TYPE
RANDOMIZE TIMER
DIM SHARED High AS INTEGER, Wide AS INTEGER
DO
	COLOR 14, 6: CLS
	LOCATE 7, 10: PRINT CHR$(214); STRING$(40, 196); CHR$(183);
	FOR t% = 8 TO 14: LOCATE t%, 10
		PRINT CHR$(186); STRING$(40, 32); CHR$(186); : NEXT
	LOCATE 15, 10: PRINT CHR$(211); STRING$(40, 196); CHR$(189);
	COLOR 15, 3
	LOCATE 8, 12: PRINT SPACE$(38);
	LOCATE 9, 12: PRINT " Press 1 for Mode &H101 - 640x480x256 ";
	LOCATE 10, 12: PRINT SPACE$(38);
	LOCATE 11, 12: PRINT " Press 2 for Mode &H103 - 800x600x256 ";
	LOCATE 12, 12: PRINT SPACE$(38);
	LOCATE 13, 12: PRINT " Press Escape to exit vesa mode demo  ";
	LOCATE 14, 12: PRINT SPACE$(38);
	DO
		DO: k$ = INKEY$: LOOP WHILE k$ = ""
		k% = INSTR(" 12" + CHR$(27), k$)
	LOOP WHILE k% < 2
	SELECT CASE k%
		CASE 2: High = 480: Wide = 640: vesatest &H101
			'vesamode &H101   640x480x256
		CASE 3: High = 600: Wide = 800: vesatest &H103
			'vesamode &H103   800x6000x256
		CASE 4: EXIT DO
		CASE ELSE: BEEP
	END SELECT
LOOP: COLOR 2, 0: CLS : SYSTEM
'_|_|_|   end   SVGA_XTB.BAS

SUB Bline (x%, y%, x2%, y2%, c%)
'_|_|_|   Bresenham Line Drawing Algorithm
'_|_|_|   Adapted from BRESNHAM.C in Bob Stout's SNIPPETS.
	IF x% = x2% THEN
		b% = y%: e% = y2%: IF y% > y2% THEN SWAP b%, e%
		FOR b% = b% TO e%: putpixel x%, b%, c%: NEXT: EXIT SUB
	END IF
	IF y% = y2% THEN
		b% = x%: e% = x2%: IF x% > x2% THEN SWAP b%, e%
		FOR b% = b% TO e%: putpixel b%, y%, c%: NEXT: EXIT SUB
	END IF
	i% = 0: steep% = 0: e% = 0
	IF (x2% - x%) > 0 THEN sx% = 1:  ELSE sx% = -1
	dx% = ABS(x2% - x%)
	IF (y2% - y%) > 0 THEN sy% = 1:  ELSE sy% = -1
	dy% = ABS(y2% - y%)
	IF (dy% > dx%) THEN
		steep% = 1: SWAP x%, y%: SWAP dx%, dy%: SWAP sx%, sy%
	END IF: e% = 2 * dy% - dx%: ex% = dx% * 2: ey% = dy% * 2
	FOR i% = 0 TO dx% - 1
		IF steep% = 1 THEN putpixel y%, x%, c%:  ELSE putpixel x%, y%, c%
		WHILE e% >= 0: y% = y% + sy%: e% = e% - ex%: WEND
		x% = x% + sx%: e% = e% + ey%
	NEXT: putpixel x2%, y2%, c%
END SUB

SUB putpixel (x%, y%, c%)
	STATIC a%
	px& = x%: py& = y%
	IF (x% < 0) OR (x% > Wide) OR (y% < 0) OR (y% > High) THEN EXIT SUB
	po& = py& * Wide + px&: pa& = po& \ 65536: po& = po& AND 65535
	IF a% <> INT(pa&) THEN
		DIM r AS RegType       '_|_|_|   If not in the proper frame
		a% = INT(pa&)          '_|_|_|   of memory for the putpixel,
		r.ax = &H4F05          '_|_|_|   switch to the right frame.
		r.bx = 0
		r.dx = a%
		CALL INTERRUPT(&H10, r, r)
	END IF
	DEF SEG = &HA000          '_|_|_|   store the color data in
	POKE po&, c%              '_|_|_|   the video memory
	DEF SEG
END SUB

SUB vesamode (mode&)
	DIM r AS RegType    '_|_|_|   mode% should be &H101 to access
	r.ax = &H4F02       '_|_|_|   the 640x480x256 VESA video mode
	r.bx = mode&
	CALL INTERRUPT(&H10, r, r)
END SUB

SUB vesatest (mode&)
	'$DYNAMIC
	DIM pal(256) AS PaletteData
	r% = 7: rd% = 3: g% = 11: gd% = 4: b% = 17: bd% = 5
	FOR t% = 1 TO 127
		IF (r% > 60) OR (r% < 3) THEN rd% = -rd%
		IF (g% > 59) OR (g% < 4) THEN gd% = -gd%
		IF (b% > 58) OR (b% < 5) THEN bd% = -bd%
		r% = r% + rd%: g% = g% + gd%: b% = b% + bd%
		pal(t%).r = r%: pal(t%).g = g%: pal(t%).b = b%
		pal(256 - t%) = pal(t%): NEXT: pal(128) = pal(127)
	SCREEN 13   '_|_|_|   make QB think it is not in text mode
	vesamode mode&
	x% = 1: x1% = 1: y% = 1: y1% = 1: c% = 0
	yd% = 2: yd1% = 2: xd% = 2: xd1% = 2
	Hg% = High - 2: Wd% = Wide - 2
	Hmid% = High \ 4: Wmid% = Wide \ 4
	FOR t% = Hmid% TO High - Hmid%
		Bline Wmid%, t%, Wide - Wmid%, t%, t% AND 255: NEXT
	Hmid% = High \ 3: Wmid% = Wide \ 3
	FOR t% = Wmid% TO Wide - Wmid%
		Bline t%, Hmid%, t%, High - Hmid%, t% AND 255: NEXT
	BEEP
	DO
		c% = (c% MOD 255) + 1
		pal(256) = pal(255)
		FOR t% = 255 TO 1 STEP -1: pal(t%) = pal(t% - 1)
			OUT &H3C8, t%: OUT &H3C9, pal(t%).r
			OUT &H3C9, pal(t%).g: OUT &H3C9, pal(t%).b
		NEXT: pal(0) = pal(256)
		IF (x% < 1) OR (x% > Hg%) THEN xd% = RND * 3 + 2
		IF x% > Hg% THEN xd% = -xd%
		IF (y% < 1) OR (y% > Wd%) THEN yd% = RND * 3 + 2
		IF y% > Wd% THEN yd% = -yd%
		IF (x1% < 1) OR (x1% > Hg%) THEN xd1% = RND * 3 + 2
		IF x1% > Hg% THEN xd1% = -xd1%
		IF (y1% < 1) OR (y1% > Wd%) THEN yd1% = RND * 3 + 2
		IF y1% > Wd% THEN yd1% = -yd1%
		x% = x% + xd%: x1% = x1% + xd1%: y% = y% + yd%
		y1% = y1% + yd1%: Bline (y%), (x%), (y1%), (x1%), c%
	LOOP WHILE INKEY$ = ""
	SCREEN 0: WIDTH 80, 25
END SUB

