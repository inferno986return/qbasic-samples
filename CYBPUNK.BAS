SCREEN 13: CLS
logo$ = "CYBERPUNK"
L% = LEN(logo$)
Center% = 20 - (L% + 1) \ 2
Block& = Center% * 8 - 8
sizeofbuf% = L% * 64
DIM buf(2 + L% * 32) AS INTEGER
wide% = L% * 8 - 1
colour% = 1
LOCATE 1, Center%, 0
COLOR colour%
PRINT logo$;
X% = Block& MOD 320: Y% = Block& \ 320
GET (X%, Y%)-(X% + wide%, Y% + 7), buf
WHILE (Block& \ 320) < 191
   DO
      DEF SEG = VARSEG(buf(0))
      FOR T% = 1 TO sizeofbuf%
         C% = PEEK(T% + 3)
         IF C% > 0 THEN POKE T% + 3, colour%
      NEXT
      X% = Block& MOD 320: Y% = Block& \ 320
      PUT (X%, Y%), buf, PSET
      WAIT &H3DA, 8: WAIT &H3DA, 8, 8
      colour% = (colour% MOD 200) + 1
   LOOP WHILE (colour% MOD 3) <> 0
   IF (Block& \ 320) < 190 THEN
      DEF SEG = &HA000
      FOR T& = Block& TO Block& + wide%: POKE T&, 0: NEXT
   END IF
   Block& = Block& + 320
WEND
WHILE INKEY$ = "": WEND
SCREEN 0: WIDTH 80, 25

