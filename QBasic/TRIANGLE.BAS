'--------------- TRIANGLE.BAS -------- Cut Here ---------------
SCREEN 12
Xres% = 639
Yres% = 479
CLS
X% = RND * Xres%
Y% = RND * Yres%
DO
   N! = RND
   IF N! <= 1 / 3 THEN
      X% = X% * .5
      Y% = Y% * .5
   ELSEIF N! > 1 / 3 AND N! <= 2 / 3 THEN
      X% = (Xres% / 2 + X%) * .5
      Y% = (Yres% + Y%) * .5
   ELSEIF N! > 2 / 3 THEN
      X% = (Xres% + X%) * .5
      Y% = Y% * .5
   END IF
   PSET (X%, Yres% - Y%), 15
LOOP UNTIL INKEY$ <> ""
END
'------------------------ Cut Here --------------------------
'Please let me know if you like it!
'°±²Û  Carl Gorringe  Û²±°
