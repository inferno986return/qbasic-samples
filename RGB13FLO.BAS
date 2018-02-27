DECLARE SUB ScrPocket (gt%)
DECLARE SUB PalPocket (save%)
DECLARE SUB CircDraw ()
'_|_|_|   RGB13FLO.BAS
'_|_|_|   A graphics display program based on a FIDO ECHO message
'_|_|_|   From: Mike Castelli   ...   Date: 04-05-96 10:48
'_|_|_|   Subject: Circle Burn
'_|_|_|   No guarantees or warrantees are given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (4/18/96)
CRSX% = CSRLIN: CRSY% = POS(0): ScrPocket 1
TYPE BallsColorDef
   R AS INTEGER: G AS INTEGER: B AS INTEGER: END TYPE
TYPE GreatBallsOfFire
   X AS INTEGER: XD AS INTEGER: Y AS INTEGER: YD AS INTEGER
   H AS INTEGER: C AS INTEGER: END TYPE
DIM SHARED cr(1 TO 6) AS GreatBallsOfFire
DIM SHARED rgb(1 TO 6, 16) AS BallsColorDef
FOR t% = 0 TO 8
   C% = t% * 5 + 22: B% = (t% + 1) * 2
   rgb(1, t%).R = C%: rgb(1, t%).G = B%: rgb(1, t%).B = t%
   rgb(2, t%).R = t%: rgb(2, t%).G = C%: rgb(2, t%).B = B%
   rgb(3, t%).R = B%: rgb(3, t%).G = t%: rgb(3, t%).B = C%
   rgb(4, t%).R = C%: rgb(4, t%).G = 0: rgb(4, t%).B = C%
   rgb(5, t%).R = 0: rgb(5, t%).G = C%: rgb(5, t%).B = C%
   rgb(6, t%).R = C%: rgb(6, t%).G = C%: rgb(6, t%).B = 0: NEXT
FOR t% = 1 TO 7
   FOR C% = 1 TO 6
      rgb(C%, 16 - t%) = rgb(C%, t%): NEXT: NEXT
SCREEN 13: RANDOMIZE (TIMER * 100): PalPocket 1
FOR t% = 1 TO 6
   cr(t%).X = RND * 320: cr(t%).Y = RND * 200
   cr(t%).XD = (RND * 2 + 1): cr(t%).YD = (RND * 2 + 1)
   cr(t%).H = t% * 2: NEXT
WHILE INKEY$ = ""
   CircDraw
   FOR t% = 1 TO 6
      IF cr(t%).X < 10 THEN cr(t%).XD = ((RND * 999) AND 1) + 1
      IF cr(t%).Y < 10 THEN cr(t%).YD = ((RND * 999) AND 1) + 1
      IF cr(t%).X > 309 THEN cr(t%).XD = -(((RND * 999) AND 1) + 1)
      IF cr(t%).Y > 189 THEN cr(t%).YD = -(((RND * 999) AND 1) + 1)
      cr(t%).X = cr(t%).X + cr(t%).XD
      cr(t%).Y = cr(t%).Y + cr(t%).YD: NEXT
WEND: PalPocket 0: SCREEN 0: WIDTH 80, 25
LOCATE CRSX%, CRSY%: ScrPocket 0
'_|_|_|   end   RGB13FLO.BAS

SUB CircDraw
   FOR C% = 1 TO 6
      WAIT &H3DA, 8: WAIT &H3DA, 8, 8
      cr(C%).H = (cr(C%).H + 1) AND 15
      cr(C%).C = (cr(C%).C + 14) AND 15
      X% = cr(C%).X: Y% = cr(C%).Y: H% = cr(C%).H
      FOR l% = 0 TO 11
         CIRCLE (X%, Y%), l% + 1, ((H% + l%) AND 15) + C% * 16 - 15
      NEXT
      FOR t% = 1 TO 16
         OUT &H3C8, t% + (C% - 1) * 16: H% = (cr(C%).C + t%) AND 15
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

SUB ScrPocket (gt%) STATIC
   DIM scr(4000)  AS STRING * 1
   DEF SEG = &HB800
   IF gt% <> 0 THEN
      FOR t& = 0 TO 3999: scr(t&) = CHR$(PEEK(t&) AND 255): NEXT
   ELSE
      FOR t& = 0 TO 3999: POKE t&, ASC(scr(t&)): NEXT
   END IF
END SUB

