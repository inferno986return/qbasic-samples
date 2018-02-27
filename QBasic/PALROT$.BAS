'PALROT$.BAS   PUBLIC DOMAIN   by Kurt Kuzba.  (4/27/1997)
DECLARE SUB PaletteRotate (lo%, hi%, D$)
SCREEN 13
CLS
FOR r% = 30 TO 125
   CIRCLE (159, 99), r%, r%
   CIRCLE (160, 99), r%, r%
NEXT
WHILE INKEY$ = "": PaletteRotate 30, 125, "f": WEND
WHILE INKEY$ = "": PaletteRotate 30, 125, "b": WEND
SCREEN 0: SYSTEM

SUB PaletteRotate (lo%, hi%, D$)
   L% = (hi% - lo% + 1) * 3: pal$ = ""
   '   set up your data string and length parameter
   OUT &H3C7, lo%
   '   Set color register to read palette registers
   '   After each read, the palette register will
   '   increment to the next color in the RGB sequence.
   '   After all three reads, the color itself increments
   FOR rgb% = 1 TO L%: pal$ = pal$ + CHR$(INP(&H3C9)): NEXT
   '   Read the color values from the palette registers
   IF UCASE$(D$) = "F" THEN
      pal$ = LEFT$(RIGHT$(pal$, 3) + pal$, L%)
   END IF
   IF UCASE$(D$) = "B" THEN
      pal$ = MID$(pal$, 4) + LEFT$(pal$, 3)
   END IF
   '   Rotate the palette data in our data string
   OUT &H3C8, lo%
   '   Set color register to set palette registers
   '   After each set, the palette register will
   '   increment to the next color in the RGB sequence.
   '   After all three sets, the color itself increments
   FOR rgb% = 1 TO L%
      OUT &H3C9, ASC(MID$(pal$, rgb%))
      WAIT (&H3DA), 1: WAIT (&H3DA), 1, 1
   NEXT
   '   Set the palette registers to the rotated colors
   '   If snow results, introduce a delay into the write loop
   '   This delay waits for the horizontal retrace
END SUB

