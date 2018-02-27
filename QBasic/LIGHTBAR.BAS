'_|_|_|   LIGHTBAR.BAS
'_|_|_|   Program to demonstrate scrolling text with lightbar.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (8/20/96)
DECLARE FUNCTION TagBar$ (r%)
RANDOMIZE (TIMER * INP(64) + INP(64))
DIM SHARED tags$(100)
FOR t% = 0 TO 99: tags$(t%) = "": NEXT
tags$(0) = "Th-th-th-that's all, folks": tags$(1) = "End of message"
tags$(2) = "This is not a tagline": tags$(3) = "Buy a vowel"
tags$(4) = "I'll take QuickBasic 4.5 for $100, Alex"
tags$(5) = "I got one hand in my pocket"
tags$(6) = "9 of 10 people say the population is 90% idiots"
tags$(7) = "That's funny, I had a topic when I came in...?!?"
tags$(8) = "But can you sing underwater, standing on your head?"
tags$(9) = "I had an idea once, but it got lonely and left"
tags$(10) = "Pay no attention to the man behind the keyboard"
tags$(11) = "Elvis is living in my basement"
tags$(12) = "The future's uncertain and The End is always near"
tags$(13) = "There are EELS in my hovercraft"
tags$(14) = "Dammit Spock! You can't argue with a tagline!"
tags$(15) = "This tagline unregistered for 65,536 days"
tags$(16) = "You've just won a trip for 2 on my Twit Filter!"
tags$(17) = "I think my mouse just crawled off my desk"
tags$(18) = "Is that a QWK packet or are you just happy to see me?"
tags$(19) = "Sufficiently sophisticated technology is magic"
COLOR 15, 1: CLS : LOCATE 5, 10: p$ = " RS" + CHR$(13)
PRINT "Print RANDOM tagline or SELECT [ENTER] (R:S)? ";
p% = 0: WHILE p% < 2: p% = INSTR(p$, UCASE$(INKEY$)): WEND
p$ = TagBar$(p% - 2): LOCATE 10, 5: COLOR 12, 1: PRINT p$
END
 
FUNCTION TagBar$ (r%)
  PCOPY 0, 1: SCREEN , , 1, 1: lim% = 0
  IF tags$(lim%) <> "" THEN
     WHILE tags$(lim%) <> "": lim% = lim% + 1: WEND
     IF r% > 0 THEN
        sel% = 0: top% = 0: ex% = lim%: COLOR 10, 0: LOCATE 1, 1, 0
        PRINT STRING$(560, CHR$(176)): s$ = STRING$(76, " ")
        WHILE lim% = ex%
           FOR t% = 0 TO 4
              LOCATE 2 + t%, 3: COLOR 12, 0
              IF t% + top% = sel% THEN COLOR 15, 5
              PRINT LEFT$(tags$(top% + t%) + s$, 76)
           NEXT: p% = 0: p$ = " HPIQGO" + CHR$(13)
           WHILE p% < 2: p% = INSTR(p$, RIGHT$(INKEY$, 1)): WEND
           SELECT CASE p%
              CASE IS = 2   'CURSOR UP
                 IF sel% > 0 THEN sel% = sel% - 1
                 IF sel% < top% THEN top% = sel%
              CASE IS = 3   'CURSOR DOWN
                 IF sel% < (lim% - 1) THEN sel% = sel% + 1
                 IF sel% > (top% + 4) THEN top% = sel% - 4
              CASE IS = 4   'PAGE UP
                 top% = top% - 5: sel% = sel% - 5
                 IF top% < 0 THEN top% = 0
                 IF sel% < 0 THEN sel% = 0
              CASE IS = 5   'PAGE DOWN
                 top% = top% + 5: sel% = sel% + 5
                 IF top% >= lim% - 5 THEN top% = lim% - 5
                 IF top% < 0 THEN top% = 0
                 IF sel% >= lim% THEN sel% = lim% - 1
              CASE IS = 6   'HOME
                 top% = 0: sel% = 0
              CASE IS = 7   'END
                 sel% = lim% - 1: top% = sel% - 4
                 IF top% < 0 THEN top% = 0
              CASE IS = 8   'ENTER
                 lim% = sel%
           END SELECT
        WEND
     ELSE
        lim% = (RND * 9999) MOD lim%
     END IF
  END IF
  SCREEN , , 0, 0
  TagBar$ = tags$(lim%)
END FUNCTION
'_|_|_|   end   LIGHTBAR.BAS
