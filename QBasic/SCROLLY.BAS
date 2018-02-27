DECLARE SUB delay (hold!)
DECLARE FUNCTION msg$ ()
COLOR 14, 1
CLS
LOCATE 3, 1
s$ = STRING$(11, " ")
PRINT s$ + "This program demonstrates application of scrolling text"
PRINT s$ + "on a single line of the screen. Speed may be controlled"
PRINT s$ + "by adjusting the value you are sending to the delay SUB."
PRINT s$ + "Incidentally, this delay function works perfectly, and"
PRINT s$ + "for any number of seconds that fit in single precision."
a$ = msg$
COLOR 15, 4
WHILE INKEY$ = ""
   LOCATE 1, 1
   PRINT LEFT$(a$, 80);
   a$ = MID$(a$, 2) + LEFT$(a$, 1)
   delay 0                      'I used the minimum delay
WEND

SUB delay (hold!)
' pause program in .05 second intervals.
DEF SEG = 0
IF hold! < (3600 / 65536) THEN hold! = (3600 / 65536)
WHILE hold! > 14.0625
   hold% = PEEK(&H46C)
   WHILE PEEK(&H46C) = hold%: WEND
   WHILE PEEK(&H46C) <> hold%: WEND
   hold! = hold! - 14.0625
WEND
hold% = hold! * 18.20444444#
hold% = (PEEK(&H46C) + hold%) AND 255
   WHILE PEEK(&H46C) <> hold%: WEND
END SUB

FUNCTION msg$
a$ = "     This is how we scroll our text, scroll our text, "
a$ = a$ + "scroll our text. This is how we scroll our text, "
a$ = a$ + "early Monday morning. In fact, we seem to do it "
a$ = a$ + "this way most all the time, but who is really "
a$ = a$ + "keeping track? :)"
msg$ = a$
END FUNCTION

