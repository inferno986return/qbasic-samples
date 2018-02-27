'_|_|_|   BIGPALET.BAS
'_|_|_|   This is a simple demonstration of a technique rotating
'_|_|_|   a big palette smoothly in graphics mode 13h.
'_|_|_|   No warrantees or guarantees are given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (4/7/96)
DECLARE SUB fade13 ()
DECLARE SUB RotatePalette (t%)
DECLARE SUB PalPocket (save%)
DECLARE SUB MakePalette ()
'$DYNAMIC
DIM SHARED PAL(384) AS INTEGER
DIM SHARED RGB(16050) AS LONG
SCREEN 13: PalPocket 1: SOUND 999, 3: MakePalette
FOR t% = 200 TO 1 STEP -1
   CIRCLE (60 + t% \ 2, 50 + t% \ 4), t%, 201 - t%
   PAINT (60 + t% \ 2, 50 + t% \ 4), 201 - t%
NEXT: t% = 0
WHILE INKEY$ = ""
   WHILE (INP(&H3DA) AND 8) = 0: WEND    ' Uncomment these lines
   WHILE (INP(&H3DA) AND 8) <> 0: WEND   ' if compiling to .EXE
   WHILE (INP(&H3DA) AND 8) = 0: WEND    ' They are for smooth
   WHILE (INP(&H3DA) AND 8) <> 0: WEND   ' rotation if compiled
   RotatePalette t%: t% = (t% + 1) MOD 21400
WEND
PalPocket 0
SCREEN 0: WIDTH 80, 25
END
'_|_|_|   end   BIGPALET.BAS

REM $STATIC
SUB MakePalette
   DEF SEG = VARSEG(RGB(0)): O& = VARPTR(RGB(0))
   r% = 0: g% = 0: b% = 0: rd% = 1: gd% = 1: bd% = 1
   FOR t& = 0 TO 21399
      p& = O& + t& * 3
      IF (r% < 64) AND (r% >= 0) THEN
         POKE p&, r%
      ELSE
         IF r% < 0 THEN POKE p&, 0
         IF r% > 63 THEN POKE p&, 63
      END IF
      IF (g% < 64) AND (g% >= 0) THEN
         POKE p& + 1, g%
      ELSE
         IF g% < 0 THEN POKE p& + 1, 0
         IF g% > 63 THEN POKE p& + 1, 63
      END IF
      IF (b% < 64) AND (b% >= 0) THEN
         POKE p& + 2, b%
      ELSE
         IF b% < 0 THEN POKE p& + 2, 0
         IF b% > 63 THEN POKE p& + 2, 63
      END IF
      IF r% = 70 THEN rd% = -1:  ELSE IF r% = -8 THEN rd% = 1
      IF g% = 73 THEN gd% = -1:  ELSE IF g% = -11 THEN gd% = 1
      IF b% = 76 THEN bd% = -1:  ELSE IF b% = -14 THEN bd% = 1
      r% = r% + rd%: g% = g% + gd%: b% = b% + bd%
   NEXT
END SUB

SUB PalPocket (save%)
   DEF SEG = VARSEG(PAL(0)): O& = VARPTR(PAL(0))
   IF save% <> 0 THEN
      FOR t% = 0 TO 255
         OUT &H3C7, t%
         POKE O& + t% * 3 + 0, INP(&H3C9)
         POKE O& + t% * 3 + 1, INP(&H3C9)
         POKE O& + t% * 3 + 2, INP(&H3C9)
      NEXT
   ELSE
      FOR t% = 0 TO 255
         OUT &H3C8, t%
         OUT &H3C9, PEEK(O& + t% * 3 + 0)
         OUT &H3C9, PEEK(O& + t% * 3 + 1)
         OUT &H3C9, PEEK(O& + t% * 3 + 2)
      NEXT
   END IF
END SUB

SUB RotatePalette (t%)
   DEF SEG = VARSEG(RGB(0)): O& = VARPTR(RGB(0))
   FOR att% = 1 TO 255
      C& = O& + ((att% + t%) MOD 21400) * 3
      OUT &H3C8, att%
      OUT &H3C9, PEEK(C&)
      OUT &H3C9, PEEK(C& + 1)
      OUT &H3C9, PEEK(C& + 2)
   NEXT
END SUB

