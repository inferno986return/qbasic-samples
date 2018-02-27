DECLARE SUB testpattern (d%)
SCREEN 12
DO
  k$ = INKEY$
  IF k$ > "" THEN EXIT DO
  testpattern INT(RND * 30 + 1)
  SLEEP 2
LOOP
SCREEN 0: WIDTH 80, 25

SUB testpattern (d%)
  DIM Lft AS INTEGER, Rgt AS INTEGER
  DIM Top AS INTEGER, Bot AS INTEGER
  Lft = 160: Rgt = 479: Top = 120: Bot = 360
  DIM x(Lft TO Rgt) AS LONG, y(Top TO Bot) AS LONG
  FOR t% = Lft TO Rgt: x(t%) = t%: NEXT
  FOR t% = Top TO Bot: y(t%) = t%: NEXT
  FOR t% = 0 TO 3000
    SWAP x((t% MOD 320) + Lft), x(RND * 200 + Lft)
    SWAP y((t% MOD 240) + Top), y(RND * 200 + Top)
  NEXT
  FOR v% = Top TO Bot
    FOR h% = Lft TO Rgt
      PSET (x(h%), y(v%)), ((x(h%) * y(v%) \ d%) AND 15)
      IF INKEY$ > "" THEN EXIT SUB
    NEXT
  NEXT
END SUB

