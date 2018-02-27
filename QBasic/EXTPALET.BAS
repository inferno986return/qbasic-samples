'_|_|_|   PALROT13.BAS
'_|_|_|   This is a simple demonstration of a technique ratating
'_|_|_|   a graphics display smoothly in graphics mode 13h.
'_|_|_|   No warrantees or guarantees are given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (3/22/96)
DECLARE SUB fade13 ()
DECLARE SUB RotatePalette (t%)
DECLARE SUB PalPocket (save%)
DECLARE SUB MakePalette ()
DIM SHARED PAL(384) AS INTEGER
DIM SHARED RGB(4032) AS INTEGER
SCREEN 13: PalPocket 1: SOUND 999, 3: MakePalette
FOR t% = 200 TO 1 STEP -1
   CIRCLE (160, 100), t%, 201 - t%
   PAINT (160, 100), 201 - t%
NEXT
SOUND 999, 3: t% = 0
WHILE INKEY$ = ""
'   WHILE (INP(&H3DA) AND 8) = 0: WEND    ' Uncomment these two
'   WHILE (INP(&H3DA) AND 8) <> 0: WEND   ' if compiling to .EXE
   RotatePalette t%: t% = (t% + 1) MOD 2688
WEND
PalPocket 0
SOUND 999, 3
WHILE INKEY$ = "": WEND
SCREEN 0: WIDTH 80, 25
END
'_|_|_|   end   PALROT13.BAS

SUB MakePalette
   DEF SEG = VARSEG(RGB(0)): O& = VARPTR(RGB(0))
   r% = 0: g% = 0: b% = 0: rd% = 1: gd% = 1: bd% = 1
   FOR t% = 0 TO 2687
      p& = O& + t% * 3
      POKE p&, r%: POKE p& + 1, g%: POKE p& + 2, b%
      IF r% = 63 THEN rd% = -1:  ELSE IF r% = 0 THEN rd% = 1
      IF g% = 55 THEN gd% = -1:  ELSE IF g% = 0 THEN gd% = 1
      IF b% = 47 THEN bd% = -1:  ELSE IF b% = 0 THEN bd% = 1
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
      C& = O& + ((att% + t%) MOD 2688) * 3
      OUT &H3C8, att%
      OUT &H3C9, PEEK(C&)
      OUT &H3C9, PEEK(C& + 1)
      OUT &H3C9, PEEK(C& + 2)
   NEXT
END SUB

