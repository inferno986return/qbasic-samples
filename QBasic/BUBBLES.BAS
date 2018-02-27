'_|_|_|   BUBBLES.BAS
'_|_|_|   This program uses the CIRCLE function to create the
'_|_|_|   illusion of bubbles being blown from a bubble machine
'_|_|_|   and blowing past the viewer.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (1/31/96)
TYPE CircObj
   LX AS INTEGER: LY AS INTEGER: R AS INTEGER
   CX AS INTEGER: CY AS INTEGER: C AS INTEGER
END TYPE: DIM CO(25) AS CircObj: W% = 320: H% = 240
CO(0).LX = W% * 2: CO(0).CX = 1: CO(0).LY = H%: CO(0).CY = 1
XL% = W% * 2: YL% = H% * 2: FOR T% = 0 TO MAX%: CO(T%) = CO(0): NEXT
RANDOMIZE (TIMER + INP(64)): MAX% = VAL(COMMAND$)
IF MAX% < 10 THEN MAX% = 10
IF MAX% > 24 THEN MAX% = 24
FOR T% = 0 TO MAX%: CO(T%) = CO(0): NEXT
WHILE INKEY$ <> "": WEND: k$ = "": C% = 0: SCREEN 12
WHILE k$ = ""
   k$ = INKEY$
   IF k$ = "" THEN
      CIRCLE (CO(C%).LX, CO(C%).LY), CO(C%).R, 0
      X% = CO(C%).LX + CO(C%).CX: Y% = CO(C%).LY + CO(C%).CY
      IF (X% >= XL%) OR (X% < 0) OR (Y% < 0) OR (Y% >= YL%) THEN
          X% = RND * 9: Y% = 9 - X%
          IF RND > .49 THEN X% = -X%
          IF RND > .49 THEN Y% = -Y%
          CO(C%).CX = X%: CO(C%).CY = Y%
          CO(C%).C = 14 * RND + 1: X% = W%: Y% = H%
      END IF: CO(C%).LX = X%: CO(C%).LY = Y%
      X% = ABS(W% - CO(C%).LX) + ABS(H% - CO(C%).LY)
      CO(C%).R = (100 + X%) \ 5
      CIRCLE (CO(C%).LX, CO(C%).LY), CO(C%).R, CO(C%).C
      C% = (C% + 1) MOD MAX%
   END IF
WEND
'_|_|_|   end  BUBBLES.BAS
