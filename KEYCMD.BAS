'_|_|_|   KEYCMD.BAS
'_|_|_|   Demonstrates of keyboard array and dynamic screen control
'_|_|_|   in Quick Basic. Keyboard routine of limited value - ASM.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (8/2/96)
DECLARE SUB Keyboard ()
DECLARE FUNCTION noise% ()
DIM SHARED Keys(128) AS INTEGER, dla AS LONG
DIM SHARED siren AS INTEGER, sirendirection AS INTEGER
COLOR 15, 1: CLS : siren = 480
PRINT " Use cursor keys to move. ESC exits."
PRINT " The ALT and CTRL key make sounds."
dla = 0: DEF SEG = &H40: t% = PEEK(&H6C)
WHILE t% = PEEK(&H6C): WEND: t% = PEEK(&H6C)
WHILE t% = PEEK(&H6C): dla = dla + 1: WEND: t% = PEEK(&H6C)
dla = dla \ 50
COLOR 14: underchar% = 32: undercolor% = 16
x% = 12: y% = 40: c$ = CHR$(219): LOCATE x%, y%: PRINT c$;
DO
   IF Keys(1) > 0 THEN EXIT DO:  ELSE Keyboard
   ox% = x%: oy% = y%
   IF (Keys(72) > 0) AND (x% > 1) THEN x% = x% - 1
   IF (Keys(75) > 0) AND (y% > 1) THEN y% = y% - 1
   IF (Keys(77) > 0) AND (y% < 80) THEN y% = y% + 1
   IF (Keys(80) > 0) AND (x% < 25) THEN x% = x% + 1
   IF (ox% <> x%) OR (oy% <> y%) THEN
      t% = 0: WHILE t% < dla: t% = t% + noise%: WEND
      DEF SEG = &HB800: p% = ((ox% * 80 + oy%) - 81) * 2
      POKE p%, underchar%: POKE p% + 1, undercolor%
      p% = ((x% * 80 + y%) - 81) * 2
      underchar% = PEEK(p%): undercolor% = PEEK(p% + 1)
      LOCATE x%, y%: PRINT c$;
   ELSE toss% = noise%
   END IF
LOOP: DEF SEG = &H40: POKE &H17, PEEK(&H17) AND 240
'_|_|_|   end   KEYCMD.BAS

SUB Keyboard
   K% = INP(96): Keys(K% AND 127) = 1 + (K% > 127)
   toss$ = INKEY$
END SUB

FUNCTION noise%
   STATIC zap%
   result% = 1
   IF Keys(29) > 0 THEN
      IF siren < 500 THEN sirendirection = 20: Keyboard
      IF siren > 1000 THEN sirendirection = -20: Keyboard
      siren = siren + sirendirection: SOUND siren, .07: Keyboard
      result% = result% + dla * .3: Keyboard
   END IF: Keyboard
   IF Keys(56) > 0 THEN
      zap% = (zap% MOD 1000) + 100: SOUND zap%, .03: Keyboard
      SOUND 1200 - zap%, .03: Keyboard
      result% = result% + dla * .2: Keyboard
   END IF: Keyboard: noise% = result%
END FUNCTION

