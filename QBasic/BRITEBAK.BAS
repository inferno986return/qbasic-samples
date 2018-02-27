'_|_|_|   BRITEBAK.BAS
'_|_|_|   This program uses palette manipulation to provide a
'_|_|_|   bright colored background for text programs.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (8/18/96)
DECLARE SUB pal7Set (R%, G%, B%)
OUT &H3C7, 7           'This section just reads in the default
rDef% = INP(&H3C9)     'definition for color 7. We will use the
gDef% = INP(&H3C9)     'values we get from this to restore the
bDef% = INP(&H3C9)     'default colors when appropriate.
COLOR 1, 0: CLS
pal7Set 63, 63, 41: COLOR 0, 7
LOCATE 10, 20: PRINT " As you can see, the background is      ";
LOCATE 11, 20: PRINT " now a rather odd shade of yellow.      ";
LOCATE 12, 20: PRINT " The actual color used may be anything. ";
LOCATE 13, 20: PRINT " Hit a key to see another color.        ";
WHILE INKEY$ = "": WEND
pal7Set 47, 63, 63: PRINT
LOCATE 10, 20: PRINT " Here we have changed the values in our ";
LOCATE 11, 20: PRINT " palette to provide us with a bright    ";
LOCATE 12, 20: PRINT " cyan background color.                 ";
LOCATE 13, 20: PRINT " Hit a key to see another color.        ";
WHILE INKEY$ = "": WEND
pal7Set 63, 51, 63: PRINT
LOCATE 10, 20: PRINT " Here is a pale rose background.        ";
LOCATE 11, 20: PRINT " We have actually changed nothing but   ";
LOCATE 12, 20: PRINT " our palette definition for color 7.    ";
LOCATE 13, 20: PRINT " Hit a key to see another color.        ";
WHILE INKEY$ = "": WEND
pal7Set 63, 63, 63: PRINT
LOCATE 10, 20: PRINT " Here is the bright white background.   ";
LOCATE 11, 20: PRINT " In full-screen applications, however,  ";
LOCATE 12, 20: PRINT " there is no appreciable difference.    ";
LOCATE 13, 20: PRINT " Hit a key to see another color.        ";
WHILE INKEY$ = "": WEND
pal7Set rDef%, gDef%, bDef%: PRINT
LOCATE 10, 20: PRINT " This is our default background color.  ";
LOCATE 11, 20: PRINT " Since we saved the data, we need only  ";
LOCATE 12, 20: PRINT " sent it to the same SUB to change to   ";
LOCATE 13, 20: PRINT " our original color. Hit any key...     ";
WHILE INKEY$ = "": WEND
COLOR 10, 0: CLS : LOCATE 10
PRINT , "   There is a means to set the video to ignore the"
PRINT , "blink attribute and use the high intensity colors"
PRINT , "instead.  I prefer to use the palette manipulation,"
PRINT , "myself.  Unless you have the need to display more"
PRINT , "than 8 background colors at one time, you have a"
PRINT , "greater range of colors available to you using a"
PRINT , "routine like the one found here."
'_|_|_|   end   BRITEBAK.BAS

SUB pal7Set (R%, G%, B%)
   OUT &H3C8, 7
   OUT &H3C9, R%
   OUT &H3C9, G%
   OUT &H3C9, B%
END SUB

