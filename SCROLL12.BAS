'_|_|_|   SCROLL12.BAS
'_|_|_|   This is a simple demonstration of a technique of causing
'_|_|_|   text to scroll smoothly in graphics mode 12h.
'_|_|_|   No warrantees or guarantees are given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (8/14/98)
DECLARE SUB smoothscroll (screenline%)
SCREEN 12
RANDOMIZE TIMER
FOR t% = 1 TO 40
   y% = RND * 639: x% = RND * 479: k% = RND * 10
   FOR c% = 1 TO 10
      CIRCLE (y%, x%), RND * 100 + 30, k%
   NEXT
NEXT
text$ = " ... ... ...   This is a simple scrolling message. It"
text$ = text$ + " doesn't mean anything and really doesn't say"
text$ = text$ + " anything. It just scrolls across the screen "
text$ = text$ + "until somebody gets sick of it and hits a key"
text$ = text$ + ". I hope you like the way it works... ... ..."
COLOR 15: Kolor% = 0
WHILE INKEY$ = ""
   Kolor% = (Kolor% MOD 15) + 1
   LOCATE 15, 1, 0
   COLOR 8
   PRINT LEFT$(text$, 10);
   COLOR 7
   PRINT MID$(text$, 11, 10);
   COLOR 15
   PRINT MID$(text$, 21, 40);
   COLOR 7
   PRINT MID$(text$, 61, 10);
   COLOR 8
   PRINT MID$(text$, 71, 10);
   freq% = 800 - ((ASC(text$) AND 28) \ 3) * 48
   dur! = (ASC(text$) AND 7) * .3 + 1
   SOUND freq%, dur!
   text$ = MID$(text$ + LEFT$(text$, 1), 2)
   smoothscroll 15
   IF MID$(text$, 77, 3) = "..." THEN BEEP
WEND
SCREEN 0: END
SUB smoothscroll (screenline%)
   DIM buf(2561) AS INTEGER
   '_|_|_|   We want a buffer which is 639x16, so we need to have
   '_|_|_|   640(width) * 16(height) * 4(bitplanes) * 1(bits/plane)
   '_|_|_|   which is equal to 40,960 bits. We add 7 to this to be
   '_|_|_|   certain we don't get stuck on a partial byte, and take
   '_|_|_|   the integer portion of that number, divided by 8, which
   '_|_|_|   is 5,120. Since we are using an integer array, we may
   '_|_|_|   now divide by 2, which gives us 2560. To this, we add
   '_|_|_|   our dimensions data space, which is two integers, and
   '_|_|_|   get a result of 2,562 for our array elements. In actual
   '_|_|_|   practice, 2,561 will work. The calculation is set up to
   '_|_|_|   provide adequate space for all instances, and one extra
   '_|_|_|   integer is not much of a waste. We could also use a
   '_|_|_|   STATIC or SHAREd array to save the time to dimension
   '_|_|_|   it every iteration, but if our program has many such
   '_|_|_|   arrays, then they may all use the same space in turn
   '_|_|_|   if they are automatic variables or ERASEd.
   sl% = (screenline% - 1) * 16
   FOR t% = 1 TO 8
   '_|_|_|   We want to move the full 8 pixels, but one at a time.
      GET (1, sl%)-(639, sl% + 15), buf
   '_|_|_|   Grab the line, minus the first two pixels
      WAIT &H3DA, 1: WAIT &H3DA, 1, 1
   '_|_|_|   Wait for a horizontal retrace for smooth display
      PUT (0, sl%), buf, PSET
   '_|_|_|   Put the line back, overwriting the first pixel
   NEXT
   ERASE buf
   '_|_|_|   Might as well free up the memory when not in use.
   '_|_|_|   In this particular instance the ERASE command is
   '_|_|_|   really unnecessary, since buf() is an automatic
   '_|_|_|   variable and goes away when we exit the SUB anyway.
END SUB
'_|_|_|   end   SCROLL12.BAS
 
