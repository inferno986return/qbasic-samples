'_|_|_|   CIRCBURN.BAS
'_|_|_|   A graphics display program based on a FIDO ECHO message
'_|_|_|   From: Mike Castelli
'_|_|_|   Date: 04-05-96 10:48
'_|_|_|   Subject: Circle Burn
'_|_|_|   No guarantees or warrantees are given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (4/14/96)
DECLARE SUB PalPocket (save%)
DECLARE SUB CircPal (x%)
SCREEN 13: RANDOMIZE (TIMER * 100)
Xcoord% = 100: Xdirection% = 1: Ycoord% = 75: Ydirection% = -1
BeginColour% = 100: EndColour% = BeginColour% + 15: PalPocket 1
OriginSize% = 33: SizeOfCirc% = OriginSize%:
DO
   PalChange% = (PalChange% + 1) AND 511
   IF PalChange% = 1 THEN CircPal (RND * 100)
   IF Xcoord% < 20 THEN Xdirection% = RND * 1 + 1
   IF Xcoord% > 300 THEN Xdirection% = RND * -1 - 1
   IF Ycoord% < 20 THEN Ydirection% = RND * 1 + 1
   IF Ycoord% > 180 THEN Ydirection% = RND * -1 - 1
   Xcoord% = Xcoord% + Xdirection%: Ycoord% = Ycoord% + Ydirection%
   FOR CurrentCirc% = BeginColour% TO EndColour%
      SizeOfCirc% = SizeOfCirc% - 1
      IF SizeOfCirc% <= 0 THEN SizeOfCirc% = OriginSize%
      CIRCLE (Xcoord%, Ycoord%), SizeOfCirc%, CurrentCirc%
   NEXT
   FOR TC% = 1 TO 16
      CurrentCirc% = CurrentCirc% - 1: SizeOfCirc% = SizeOfCirc% - 1
      IF SizeOfCirc% <= 0 THEN SizeOfCirc% = OriginSize%
      CIRCLE (Xcoord%, Ycoord%), SizeOfCirc%, CurrentCirc%
   NEXT
LOOP WHILE INKEY$ = ""
PalPocket 0
SCREEN 0: WIDTH 80, 25

SUB CircPal (x%)
   FOR t% = 0 TO 33
      OUT &H3C8, t% + 100
      SELECT CASE x% AND 3
         CASE 0
            OUT &H3C9, t% + 29: OUT &H3C9, t% \ .6: OUT &H3C9, 0
         CASE 1
            OUT &H3C9, 0: OUT &H3C9, t% + 29: OUT &H3C9, t% \ .6
         CASE 2
            OUT &H3C9, t% \ .6: OUT &H3C9, 0: OUT &H3C9, t% + 29
         CASE 3
            OUT &H3C9, t% \ .6: OUT &H3C9, t% \ .6
            OUT &H3C9, t% \ .6
      END SELECT
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

