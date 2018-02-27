DECLARE SUB message (delay%)
DECLARE SUB SlowPrint (Strg$, freq%, dur!, x%, y%)
DECLARE FUNCTION slwdata$ (l%)
'From: Ray Turner                Conference: FidoQBasic   (139)
'Date: 12-11-96 18:10              BBS Name: Exec-PC
'|--|--| SLWPRNT.BAS
'|--|--| A example program to display the uses of SlowPrint routine
'|--|--| Public Domain 1996 Ray Turner Jr.
CLS : COLOR 7
FOR t% = 1 TO 24
   LOCATE t%, 1, 0
   WAIT &H3DA, 8
   WAIT &H3DA, 8, 8
   PRINT STRING$(80, 219);
NEXT
COLOR 0, 7
a$ = "SlowPrint Subroutine : Public Domain 1996 By Ray Turner Jr."
LOCATE 5, 40 - LEN(a$) \ 2
PRINT a$
COLOR 15, 1
LOCATE 9
LOCATE , 14: PRINT slwdata$(1)
FOR t% = 1 TO 5: LOCATE , 14: PRINT slwdata$(2): NEXT
LOCATE , 14: PRINT slwdata$(3): message -1
SYSTEM

SUB message (delay%)
   SHARED SLOW%
   COLOR 10: IF delay% THEN SLOW% = -1
   SlowPrint slwdata$(4), 100, .1, 10, 16
   SlowPrint slwdata$(5), 145, .1, 11, 16
   SlowPrint slwdata$(6), 200, .1, 12, 16
   SlowPrint slwdata$(7), 135, .1, 13, 16
   SlowPrint slwdata$(8), 77, .1, 14, 16
   COLOR 7, 0
END SUB

SUB SlowPrint (Strg$, freq%, dur!, x%, y%)
   SHARED SLOW%
   LOCATE x%, y%, 0
   FOR lent = 1 TO LEN(Strg$)
      IF INKEY$ = " " THEN SLOW% = 0
      Strgg$ = MID$(Strg$, lent, 1)
         'Get the Letter to print
      PRINT Strgg$;
         'Print the current letter
      IF SLOW% THEN
         IF Strgg$ <> " " THEN SOUND freq%, dur!
         'If its not a space, make a sound
         hold! = TIMER: release! = hold! + ((RND * 5 + 3) / 100)
         DO: temp! = TIMER
            LOOP WHILE (hold! <= temp!) AND (release! > temp!)
            'TIMER based random delay
      END IF
   NEXT
END SUB

FUNCTION slwdata$ (l%)
   SELECT CASE l%
      CASE 1
         slwdata$ = "ÚÄ´ SlowPrint Sample ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
      CASE 2
         slwdata$ = "³                                             ³"
      CASE 3
         slwdata$ = "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
      CASE 4
         slwdata$ = "This is a sample program for the SlowPrint"
      CASE 5
         slwdata$ = "routine for QuickBasic 4.5. It was written"
      CASE 6
         slwdata$ = "  By Ray Turner for a game he is writing."
      CASE 7
         slwdata$ = "   Kurt Kuzba modified the delay routine"
      CASE 8
         slwdata$ = "    and made a few other minor changes."
   END SELECT
END FUNCTION

