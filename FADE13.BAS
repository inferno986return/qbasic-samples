'_|_|_|   FADE13.BAS
'_|_|_|   This is a simple demonstration of a technique of fading
'_|_|_|   a graphics display smoothly in graphics mode 13h.
'_|_|_|   No warrantees or guarantees are given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (3/22/96)
DECLARE SUB fade13 ()
SCREEN 13
DEF SEG = &HA000
FOR t& = 1 TO 63999
   POKE t&, (t& AND 511) \ 2
NEXT
SOUND 999, 3
fade13
SOUND 999, 3
WHILE INKEY$ = "": WEND
PALETTE
SOUND 999, 3
WHILE INKEY$ = "": WEND
SCREEN 0
END
'_|_|_|   end   FADE13.BAS

SUB fade13
   DO
      visible% = 0
      WAIT &H3DA, 8, 8
      WAIT &H3DA, 8, 0
      FOR attrib% = 0 TO 255
         OUT &H3C7, attrib%     ' Set color to read
         red% = INP(&H3C9)      ' read red value
         green% = INP(&H3C9)    ' read green value
         blue% = INP(&H3C9)     ' read blue value
         ' Test the color values, decrementing if necessary.
         ' Set loop variable if colors are still visible.
         IF red% > 0 THEN red% = red% - 1: visible% = 1
         IF green% > 0 THEN green% = green% - 1: visible% = 1
         IF blue% > 0 THEN blue% = blue% - 1: visible% = 1
         OUT &H3C8, attrib%     ' Set color to write
         OUT &H3C9, red%        ' write red value
         OUT &H3C9, green%      ' write green value
         OUT &H3C9, blue%       ' write blue value
      NEXT
   LOOP WHILE visible% <> 0
END SUB

