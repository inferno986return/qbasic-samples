'_|_|_|   POKE12.BAS   PUBLIC DOMAIN   Kurt Kuzba  (11/23/1999)
'_|_|_|   Generate random graphic patterns in mode 12h.
RANDOMIZE TIMER
SCREEN 12
PRINT "Display will change at intervals."
PRINT "ESC exits program."
PRINT "Any other key alters display."
PRINT "Hit any key to begin."
WHILE INKEY$ > "": WEND
WHILE INKEY$ = "": WEND
DEF SEG = &HA000
OUT &H3C4, 2
DO
  m% = RND * 253 + 1
  OUT &H3C5, ((RND * 999) MOD 15) + 1
  FOR t& = 0 TO 38399
  POKE t&, z%
  z% = ((z% * 7) MOD m%) + 1
  NEXT
  FOR m% = 1 TO 700
    WHILE ((INP(&H3DA) AND 8) = 0): WEND
    WHILE ((INP(&H3DA) AND 8) <> 0): WEND
    p$ = INKEY$
    IF p$ > "" THEN EXIT FOR
  NEXT
  IF p$ = CHR$(27) THEN EXIT DO
LOOP
SCREEN 0
WIDTH 80, 25
SYSTEM

