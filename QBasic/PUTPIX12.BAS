'_|_|_|   PUTPIX12.BAS
'_|_|_|   An example of writing pixels in video mode 12h.
'_|_|_|   No warrantees or guarantees are given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (8/15/98)
DEFINT A-Z
DECLARE SUB pix12 (vertical%, horizontal%, c%)
DIM SHARED bitfield(7) AS INTEGER
   bitfield(0) = 128: bitfield(1) = 64: bitfield(2) = 32
   bitfield(3) = 16: bitfield(4) = 8: bitfield(5) = 4
   bitfield(6) = 2: bitfield(7) = 1
SCREEN 12
PRINT "Use QBasic Graphics Commands ( Y/N )? ";
DO
   DO: K$ = UCASE$(INKEY$): LOOP WHILE K$ = ""
LOOP WHILE INSTR("YN", K$) < 1: PRINT K$
IF K$ = "Y" THEN
   LINE (0, 0)-(639, 19), 10, BF
   LINE (0, 0)-(639, 479), 5
   LINE (639, 0)-(0, 479), 14
   LINE (0, 0)-(639, 479), 14
   LINE (639, 0)-(0, 479), 5
   FOR c = 15 TO 0 STEP -1
      y1% = 308 - 5 * c: y2% = 316 + 5 * c
      x1% = 234 - 5 * c: x2% = 242 + 5 * c
      LINE (y1%, x1%)-(y2%, x2%), c, BF
      IF INKEY$ > "" THEN c = 0
   NEXT
ELSE
   FOR h = 0 TO 639: FOR v = 0 TO 19: pix12 v, h, 10: NEXT: NEXT
   FOR y = 0 TO 639: pix12 y / 1.3, y, 5: NEXT
   FOR y = 0 TO 639: pix12 479 - (y / 1.3), y, 14: NEXT
   FOR y = 0 TO 639: pix12 y / 1.3, y, 14: NEXT
   FOR y = 0 TO 639: pix12 479 - (y / 1.3), y, 5: NEXT
   FOR c = 15 TO 0 STEP -1
      FOR y = 308 - 5 * c TO 316 + 5 * c
         FOR x = 234 - 5 * c TO 242 + 5 * c
            pix12 x, y, c
            IF INKEY$ <> "" THEN c = -1
         NEXT
      NEXT
   NEXT
END IF
WHILE INKEY$ = "": WEND: SCREEN 0
SUB pix12 (vertical%, horizontal%, c%)
   IF (vertical% < 0) OR (horizontal% < 0) THEN EXIT SUB
   IF (vertical% > 479) OR (horizontal% > 639) THEN EXIT SUB
   DEF SEG = &HA000: OUT &H3CE, 4: OUT &H3C4, 2
   p& = vertical%: p& = p& * 80 + horizontal% \ 8
   bit% = bitfield(horizontal% AND 7): bitmask% = 255 XOR bit%
   OUT &H3CF, 0: OUT &H3C5, 1: B% = PEEK(p&) AND bitmask%
   IF (c% AND 1) <> 0 THEN B% = B% OR bit%
   POKE p&, B%
   OUT &H3CF, 1: OUT &H3C5, 2: B% = PEEK(p&) AND bitmask%
   IF (c% AND 2) <> 0 THEN B% = B% OR bit%
   POKE p&, B%
   OUT &H3CF, 2: OUT &H3C5, 4: B% = PEEK(p&) AND bitmask%
   IF (c% AND 4) <> 0 THEN B% = B% OR bit%
   POKE p&, B%
   OUT &H3CF, 3: OUT &H3C5, 8: B% = PEEK(p&) AND bitmask%
   IF (c% AND 8) <> 0 THEN B% = B% OR bit%
   POKE p&, B%
END SUB
'_|_|_|   end   PUTPIX12.BAS
 
