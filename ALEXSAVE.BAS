'From: Alex Wellerstein          Conference: FidoQBasic   (139)
'Date: 06-09-96 12:48               Subject: This is AMAZING i think  
'this is a little screensaver type thingy i made... when there are a lot
'of lines, it looks real trippy... if it's real slow (especially in
'QBasic v.1.1) make sure to adjust the delay variable to like 0 or
'something...
'COOLSAVE.BAS  by Alex Wellerstein (General Electrik)
DEFINT A-Z: SCREEN 13: CLS : s = 10: c = 32: delay = 2
RANDOMIZE (TIMER * 1000)
WHILE k$ <> CHR$(27)
   REDIM x(s) AS INTEGER, y(s) AS INTEGER, xd(s) AS INTEGER
   REDIM yd(s) AS INTEGER, xs(s) AS INTEGER, ys(s) AS INTEGER
   FOR i = 1 TO s
      x(i) = RND * 320: y(i) = RND * 200
      xd(i) = RND * 2: yd(i) = RND * 2
      xs(i) = RND * 5: ys(i) = RND * 5
   NEXT
   DO
      FOR i = 1 TO s - 1
         LINE (x(i), y(i))-(x(i + 1), y(i + 1)), 0
      NEXT: LINE (x(s), y(s))-(x(1), y(1)), 0
      FOR i = 1 TO s
         IF x(i) > 320 THEN xd(i) = 0: xs(i) = RND * 5
         IF x(i) < 1 THEN xd(i) = -1: xs(i) = RND * 5
         IF y(i) > 200 THEN yd(i) = 0: ys(i) = RND * 5
         IF y(i) < 1 THEN yd(i) = -1: ys(i) = RND * 5
         IF yd(i) THEN y(i) = y(i) + ys(i) ELSE y(i) = y(i) - ys(i)
         IF xd(i) THEN x(i) = x(i) + xs(i) ELSE x(i) = x(i) - xs(i)
      NEXT: c = (c + 1) AND 127: IF c = 0 THEN c = rand * 90 + 32
      FOR i = 1 TO s - 1
         LINE (x(i), y(i))-(x(i + 1), y(i + 1)), c
      NEXT: LINE (x(s), y(s))-(x(1), y(1)), c: k$ = INKEY$
      FOR D = 0 TO delay: WAIT &H3DA, 8: WAIT &H3DA, 8, 8: NEXT
      SELECT CASE k$
         CASE CHR$(0) + CHR$(72): IF s < 30 THEN s = s + 1: EXIT DO
         CASE CHR$(0) + CHR$(80): IF s > 3 THEN s = s - 1: EXIT DO
         CASE CHR$(0) + CHR$(75): delay = delay + (delay >= 0)
         CASE CHR$(0) + CHR$(77): delay = delay - (delay <= 4)
         CASE " ": GOSUB Spaced
         CASE CHR$(27): EXIT DO
      END SELECT
   LOOP
WEND: SCREEN 0: WIDTH 80, 25: END
Spaced:
   CLS : LOCATE 5
   M$ = "Alex's Screen Saver": GOSUB Center
   M$ = "ESC quits": GOSUB Center
   M$ = "Left Arrow = Slower": GOSUB Center
   M$ = "Right Arrow = faster": GOSUB Center
   M$ = "Down Arrow = More Lines": GOSUB Center
   M$ = "Up Arrow = Fewer Lines": GOSUB Center
   SLEEP 3: WHILE INKEY$ <> "": WEND: RETURN
Center:
   M$ = SPACE$(20 - ((LEN(M$) + 1) \ 2)) + M$
   PRINT : PRINT M$: RETURN
'if ya dig it, tell me!

