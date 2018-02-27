'_|_|_|   RGB-FLOW.BAS
'_|_|_|   A graphics display program based on a FIDO ECHO message
'_|_|_|   From: Mike Castelli   ...   Date: 04-05-96 10:48
'_|_|_|   Subject: Circle Burn
'_|_|_|   No guarantees or warrantees are given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (4/18/96)
DECLARE SUB PalPocket (save%)
DECLARE SUB CircDraw ()
TYPE BallsColorDef
   R AS INTEGER: G AS INTEGER: B AS INTEGER
END TYPE
TYPE GreatBallsOfFire
   X AS INTEGER: Y AS INTEGER: XD AS INTEGER: YD AS INTEGER
   H AS INTEGER: C AS INTEGER
END TYPE
DIM SHARED cr(1 TO 3) AS GreatBallsOfFire
DIM SHARED rgb(1 TO 3, 6) AS BallsColorDef
FOR t% = 0 TO 3
   rgb(1, t%).R = t% * 10 + 22: rgb(1, t%).G = t% + 1: rgb(1, t%).B = 0
   rgb(2, t%).R = 0: rgb(2, t%).G = t% * 10 + 22: rgb(2, t%).B = t% + 1
   rgb(3, t%).R = t% + 1: rgb(3, t%).G = 0: rgb(3, t%).B = t% * 10 + 22
NEXT
FOR t% = 1 TO 2
   rgb(1, 6 - t%) = rgb(1, t%)
   rgb(2, 6 - t%) = rgb(2, t%)
   rgb(3, 6 - t%) = rgb(3, t%)
NEXT
SCREEN 12: RANDOMIZE (TIMER * 100)
PalPocket 1
cr(1).X = RND * 640: cr(1).Y = RND * 480
cr(1).XD = 2: cr(1).YD = 5: cr(1).H = 0
cr(2).X = RND * 640: cr(2).Y = RND * 480
cr(2).XD = 3: cr(2).YD = 5: cr(1).H = 3
cr(3).X = RND * 640: cr(3).Y = RND * 480
cr(3).XD = 4: cr(3).YD = 5: cr(1).H = 6
WHILE INKEY$ = ""
   CircDraw
   FOR t% = 1 TO 3
      IF cr(t%).X > 615 THEN cr(t%).XD = -(RND * 5 + 2)
      IF cr(t%).X < 25 THEN cr(t%).XD = RND * 5 + 2
      IF cr(t%).Y > 455 THEN cr(t%).YD = -(RND * 5 + 2)
      IF cr(t%).Y < 25 THEN cr(t%).YD = RND * 5 + 2
      cr(t%).X = cr(t%).X + cr(t%).XD
      cr(t%).Y = cr(t%).Y + cr(t%).YD
   NEXT
WEND: WHILE INKEY$ = "": WEND
PalPocket 0: SCREEN 0: WIDTH 80, 25

SUB CircDraw
   FOR C% = 1 TO 3
      cr(C%).H = (cr(C%).H + 1) MOD 5
      cr(C%).C = (cr(C%).C + 4) MOD 5
      X% = cr(C%).X: Y% = cr(C%).Y: H% = cr(C%).H
      FOR l% = 0 TO 29
         CIRCLE (X%, Y%), l% + 1, ((H% + l% \ 4) MOD 5) + C% * 5 - 4
      NEXT
      FOR t% = 1 TO 5
         OUT &H3C8, t% + (C% - 1) * 5: H% = (cr(C%).C + t%) MOD 5
         OUT &H3C9, rgb(C%, H%).R
         OUT &H3C9, rgb(C%, H%).G
         OUT &H3C9, rgb(C%, H%).B
      NEXT
   NEXT
END SUB

SUB PalPocket (save%) STATIC
   DIM pal(384) AS INTEGER
   DEF SEG = VARSEG(pal(0)): O& = VARPTR(pal(0))
   IF save% <> 0 THEN
      FOR t% = 0 TO 255
         OUT &H3C7, t%
         POKE O& + t% * 3 + 0, INP(&H3C9)
         POKE O& + t% * 3 + 1, INP(&H3C9)
         POKE O& + t% * 3 + 2, INP(&H3C9)
      NEXT
   ELSE
      FOR t% = 0 TO 255
         OUT &H3C8, t%
         OUT &H3C9, PEEK(O& + t% * 3 + 0)
         OUT &H3C9, PEEK(O& + t% * 3 + 1)
         OUT &H3C9, PEEK(O& + t% * 3 + 2)
      NEXT
   END IF
END SUB

