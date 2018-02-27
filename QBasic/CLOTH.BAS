'From: RAY CATHODE               Conference: QUIK_BAS -Qui (298)
'Date: 06-26-97 19:33              BBS Name: YE OLDE PAWN SHOPPE ]I[
'Subject: Cloth texture background      BBS ID: YOPSIII
'Curtain.bas    02/25/1997  by Andrew K. Dart  --= PUBLIC DOMAIN =--
'Background for full-screen message
SCREEN 12
RANDOMIZE TIMER
LINE (0, 0)-(640, 480), 15, BF' This line is optional
FOR x = 1 TO 640
   Fox = x + SIN(x / 9) * 8 - COS(x / 5) * 5
   Golf = x + SIN(x / 6) * 7 + SIN(x / 11) * 9
   LINE (Fox, 0)-(Golf, 480), 6 + x MOD 2, , INP(64) IMP NOT INP(64)
NEXT x
FOR y = 1 TO 480 STEP 3
   Able = y + SIN(y / 3) * 18
   Baker = y + SIN(y / 2) * 28
   LINE (0, Able)-(640, Baker), 0, , INP(64) AND INP(64)
NEXT y
'Optional second layer of horizontal lines:
FOR y = 1 TO 480 STEP 7
   Charlie = y + SIN(y / 7) * 4
   Dog = y + SIN((y + 1) / 5) * 5
   LINE (0, Charlie)-(640, Dog), 6, , INT(RND * 32765) * SGN(RND - .5)
NEXT y
a$ = "  Does this look like cloth or wood?  "
VPosition = 15
LOCATE VPosition, 40 - LEN(a$) \ 2
PRINT SPACE$(LEN(a$));
LOCATE VPosition + 1, 40 - LEN(a$) \ 2
COLOR 14
PRINT a$;
LOCATE VPosition + 2, 40 - LEN(a$) \ 2
PRINT SPACE$(LEN(a$));
SLEEP
END
'* Origin: The Blues Cafe (214) 638-1186  8 Nodes  (1:124/8014.0)             
                                         

