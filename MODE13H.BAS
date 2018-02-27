DECLARE SUB GetSprite (S AS ANY)
DECLARE SUB MakeSprites (S() AS ANY)
DECLARE SUB PutSprite (S AS ANY)
TYPE SpriteDef
   top AS INTEGER
   left AS INTEGER
   right AS INTEGER
   bottom AS INTEGER
   sprite AS STRING * 100
   buffer AS STRING * 100
END TYPE
DIM SHARED YMult(0 TO 199) AS LONG
FOR t& = 0 TO 199: YMult(t&) = 320 * t&: NEXT
DIM Figure(3) AS SpriteDef, anim(3) AS INTEGER, animate AS INTEGER
anim(0) = 0: anim(1) = 1: anim(2) = 2: anim(3) = 1: animate = 0
MakeSprites Figure()
SCREEN 13
GetSprite Figure(3)
WHILE INKEY$ = ""
   Figure(anim(animate)).buffer = Figure(3).sprite
   FOR t% = 1 TO 9: WAIT (&H3DA), 8: WAIT (&H3DA), 8, 8: NEXT
   '_|_|_|  To insure smooth animation. 1 to 9 to slow it down
   PutSprite Figure(anim(animate))
   animate = (animate + 1) MOD 4
WEND
SCREEN 0: WIDTH 80, 25: SYSTEM

SUB BlitSprite (S AS SpriteDef)
   t& = S.top: B& = S.bottom: L& = S.left: R& = S.right: P& = 1
   DEF SEG = &HA000
   FOR Y& = t& TO B&
      FOR X& = L& TO R&
         MID$(S.buffer, P&) = CHR$(PEEK(YMult(Y&) + X&))
         C% = ASC(MID$(S.sprite, P&))
         K% = ASC(MID$(S.buffer, P&))
         P& = P& + 1
         IF C% > 0 THEN
            POKE YMult(Y&) + X&, C%
         ELSE
            POKE YMult(Y&) + X&, K%
         END IF
      NEXT
   NEXT
   DEF SEG
END SUB

SUB GetSprite (S AS SpriteDef)
   t& = S.top: B& = S.bottom: L& = S.left: R& = S.right: P& = 1
   DEF SEG = &HA000
   FOR Y& = t& TO B&
      FOR X& = L& TO R&
         MID$(S.sprite, P&) = CHR$(PEEK(YMult(Y&) + X&))
         P& = P& + 1
      NEXT
   NEXT
   DEF SEG
END SUB

SUB MakeSprites (S() AS SpriteDef)
   DIM sp(9) AS STRING * 10
   sp(0) = "   ....   "
   sp(1) = "  ..!!..  "
   sp(2) = " ...!!... "
   sp(3) = ".........."
   sp(4) = ".........."
   sp(5) = "...$$$$$$$"
   sp(6) = ".........."
   sp(7) = " ........ "
   sp(8) = "  ......  "
   sp(9) = "   ....   "
   FOR X% = 0 TO 9: MID$(S(0).sprite, X% * 10 + 1) = sp(X%): NEXT
   FOR X% = 1 TO 100
      MID$(S(0).sprite, X%) = CHR$(ASC(MID$(S(0).sprite, X%)) - 32)
   NEXT
   S(0).top = 95: S(0).left = 155
   S(0).bottom = 104: S(0).right = 164
   sp(0) = "   ....   "
   sp(1) = "  ..!!..  "
   sp(2) = " ...!!... "
   sp(3) = ".........."
   sp(4) = ".....$$$$$"
   sp(5) = "..$$$$////"
   sp(6) = "....$$$$$$"
   sp(7) = " ........ "
   sp(8) = "  ......  "
   sp(9) = "   ....   "
   FOR X% = 0 TO 9: MID$(S(1).sprite, X% * 10 + 1) = sp(X%): NEXT
   FOR X% = 1 TO 100
      MID$(S(1).sprite, X%) = CHR$(ASC(MID$(S(1).sprite, X%)) - 32)
   NEXT
   S(1).top = 95: S(1).left = 155
   S(1).bottom = 104: S(1).right = 164
   sp(0) = "   ....   "
   sp(1) = "  ..!!.$  "
   sp(2) = " ...!!$/  "
   sp(3) = ".....$/   "
   sp(4) = "...$$/    "
   sp(5) = ".$$$/     "
   sp(6) = "...$$/    "
   sp(7) = " ...$/    "
   sp(8) = "  ...$/   "
   sp(9) = "   ...$   "
   FOR X% = 0 TO 9: MID$(S(2).sprite, X% * 10 + 1) = sp(X%): NEXT
   FOR X% = 1 TO 100
      MID$(S(2).sprite, X%) = CHR$(ASC(MID$(S(2).sprite, X%)) - 32)
   NEXT
   S(2).top = 95: S(2).left = 155
   S(2).bottom = 104: S(2).right = 164
END SUB

SUB PutPixel (X%, Y%, C%) : POKE YMult(X%) + Y%, C%: END SUB

SUB PutSprite (S AS SpriteDef)
   t& = S.top: B& = S.bottom: L& = S.left: R& = S.right: P& = 1
   DEF SEG = &HA000
   FOR Y& = t& TO B&
      FOR X& = L& TO R&
         POKE YMult(Y&) + X&, ASC(MID$(S.sprite, P&))
         P& = P& + 1
      NEXT
   NEXT
   DEF SEG
END SUB

