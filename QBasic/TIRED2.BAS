'From: Mandie N.                 Conference: FidoQBasic   (139)
'Date: 05-28-96 22:43            Subject: tired.bas
'tired.bas                       Rewrites: Kurt Kuzba
'the result of too many waking hours
SCREEN 13: COLOR 100: LOCATE 12, 6, 0: RANDOMIZE (TIMER * 100)
OUT &H3C8, 100: OUT &H3C9, 63: OUT &H3C9, 0: OUT &H3C9, 0
PRINT "TIRED   -< the lesser one >-"
c% = 16: cc% = 1: r% = 1: y% = 100: x% = 160: xx% = 1: yy% = 1
FOR L% = 16 TO 79
   OUT &H3C8, L%: OUT &H3C9, 15
   OUT &H3C9, L% - 16: OUT &H3C9, 63 - (L% - 16)
   FOR T% = 1 TO 6: WAIT &H3DA, 8: WAIT &H3DA, 8, 8: NEXT
   OUT &H3C8, 100: OUT &H3C9, 79 - L%: OUT &H3C9, 0: OUT &H3C9, 0
NEXT
WHILE INKEY$ <> "": WEND: CLS : COLOR 4
DO
   IF INKEY$ <> "" THEN EXIT DO
   WAIT &H3DA, 8: WAIT &H3DA, 8, 8: WAIT &H3DA, 8: WAIT &H3DA, 8, 8
   CIRCLE (x%, y%), r%, c%: PAINT (x%, y%), c%
   x% = x% + xx%: y% = y% + yy%: c% = c% + cc%: b% = r% \ 2
   IF (x% < b%) OR (x% > (319 - b%)) THEN
      xx% = (-1 + (RND < .5)) * SGN(xx%)
   END IF
   IF x% < 0 THEN x% = 0
   IF x% > 319 THEN x% = 319
   IF (y% < b%) OR (y% > (199 - b%)) THEN
      yy% = (-1 + (RND < .5)) * SGN(yy%)
   END IF
   IF y% < 0 THEN y% = 0
   IF y% > 199 THEN y% = 199
   IF c% < 17 OR c% > 78 THEN cc% = -cc%
   SOUND 21 + c%, .03
   IF r% = 1 THEN rr% = 1
   IF r% = 40 THEN rr% = -1
   r% = r% + rr%
LOOP: SCREEN 0: WIDTH 80, 25: PRINT "Meow?": END
