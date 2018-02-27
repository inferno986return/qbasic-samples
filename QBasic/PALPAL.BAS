'_|_|_|   PALPAL.BAS
'_|_|_|   This is a simple demonstration of methods of palette
'_|_|_|   manipulation. Demonstration uses graphics mode 13h.
'_|_|_|   No warrantees or guarantees are given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (6/2/96)
DECLARE SUB pal (act$)
SCREEN 13: DEF SEG = &HA000: DIM SHARED red(256) AS INTEGER
DIM SHARED grn(256) AS INTEGER: DIM SHARED blu(256) AS INTEGER
FOR t& = 1 TO 63999: POKE t&, (t& AND 511) \ 2: NEXT
SOUND 999, 3: pal "save"
pal "fadeout": SOUND 999, 3: WHILE INKEY$ = "": WEND
pal "fadein": SOUND 999, 3: WHILE INKEY$ = "": WEND
pal "blackout": SOUND 999, 3: WHILE INKEY$ = "": WEND
pal "restore": SOUND 999, 3: WHILE INKEY$ = "": WEND
SCREEN 0: WIDTH 80, 25
END
'_|_|_|   end   PALPAL.BAS

SUB pal (act$)
   SELECT CASE act$
   ' "save","fadein","fadeout","restore","blackout"
      CASE "save"
         FOR colour% = 0 TO 255
            OUT &H3C7, colour%          ' Set color to read
            red(colour%) = INP(&H3C9)   ' read red value
            grn(colour%) = INP(&H3C9)   ' read green value
            blu(colour%) = INP(&H3C9)   ' read blue value
         NEXT
      CASE "fadein"
         DO
            done% = 0
            FOR colour% = 0 TO 255
               OUT &H3C7, colour%       ' Set color to read
               red% = INP(&H3C9)        ' read red value
               grn% = INP(&H3C9)        ' read green value
               blu% = INP(&H3C9)        ' read blue value
               ' Test the color values, decrementing if necessary.
               ' Set loop variable if saved palette not in use.
               IF red% < red(colour%) THEN red% = red% + 1: done% = 1
               IF grn% < grn(colour%) THEN grn% = grn% + 1: done% = 1
               IF blu% < blu(colour%) THEN blu% = blu% + 1: done% = 1
               WAIT &H3DA, 8, 8
               OUT &H3C8, colour%       ' Set color to write
               OUT &H3C9, red%          ' write red value
               OUT &H3C9, grn%          ' write green value
               OUT &H3C9, blu%          ' write blue value
            NEXT
         LOOP WHILE done% <> 0
      CASE "fadeout"
         DO
            visible% = 0
            FOR colour% = 0 TO 255
               OUT &H3C7, colour%       ' Set color to read
               red% = INP(&H3C9)        ' read red value
               grn% = INP(&H3C9)        ' read green value
               blu% = INP(&H3C9)        ' read blue value
               ' Test the color values, decrementing if necessary.
               ' Set loop variable if colors are still visible.
               IF red% > 0 THEN red% = red% - 1: visible% = 1
               IF grn% > 0 THEN grn% = grn% - 1: visible% = 1
               IF blu% > 0 THEN blu% = blu% - 1: visible% = 1
               WAIT &H3DA, 8, 8
               OUT &H3C8, colour%       ' Set color to write
               OUT &H3C9, red%          ' write red value
               OUT &H3C9, grn%          ' write green value
               OUT &H3C9, blu%          ' write blue value
            NEXT
         LOOP WHILE visible% <> 0
      CASE "restore"
         FOR colour% = 0 TO 255
            OUT &H3C8, colour%          ' Set color to write
            OUT &H3C9, red(colour%)     ' write red value
            OUT &H3C9, grn(colour%)     ' write green value
            OUT &H3C9, blu(colour%)     ' write blue value
         NEXT
      CASE "blackout"
         FOR colour% = 0 TO 255
            OUT &H3C8, colour%          ' Set color to write
            OUT &H3C9, 0                ' write red value
            OUT &H3C9, 0                ' write green value
            OUT &H3C9, 0                ' write blue value
         NEXT
   END SELECT
END SUB

