DECLARE SUB CreateDeck (d() AS INTEGER, Cards AS INTEGER)
DECLARE SUB FanDeck (d() AS INTEGER)
DECLARE SUB ShuffleDeck (d() AS INTEGER)
CLS
RANDOMIZE TIMER
REDIM Deck(0) AS INTEGER
DO
  PRINT "Select (T)arot or (P)oker deck => ";
  DO
    DO: k$ = UCASE$(INKEY$): LOOP WHILE k$ = ""
  LOOP WHILE INSTR("TP", k$) < 1
  PRINT k$
  SELECT CASE k$
    CASE "P": CreateDeck Deck(), 52
    CASE "T": CreateDeck Deck(), 78
  END SELECT
  CLS : FanDeck Deck(): ShuffleDeck Deck(): FanDeck Deck()
  PRINT "Would you like to try it again  ( Y/N )? ";
  DO
    DO: k$ = UCASE$(INKEY$): LOOP WHILE k$ = ""
  LOOP WHILE INSTR("YN", k$) < 1
  PRINT k$
LOOP WHILE k$ <> "N"
SYSTEM

SUB CreateDeck (d() AS INTEGER, Cards AS INTEGER)
  REDIM d(1 TO Cards)
  FOR t% = 1 TO Cards: d(t%) = t%: NEXT
END SUB

SUB FanDeck (d() AS INTEGER)
  SELECT CASE UBOUND(d)
    CASE 52: PRINT : dk$ = "1234567890JQK"
      FOR t% = 1 TO 52
        v% = (d(t%) - 1) MOD 13 + 1
        c$ = " ": IF v% = 10 THEN c$ = "1"
        c$ = c$ + MID$(dk$, v%, 1)
        c$ = c$ + CHR$((d(t%) - 1) \ 13 + 3) + " "
        PRINT c$;
        IF t% MOD 13 = 0 THEN PRINT
      NEXT
    CASE 78: PRINT : dk$ = "1234567890PJQK"
      FOR t% = 1 TO 78
        IF d(t%) < 57 THEN
          v% = (d(t%) - 1) MOD 14 + 1
          c$ = " ": IF v% = 10 THEN c$ = "1"
          c$ = c$ + MID$(dk$, v%, 1)
          c$ = c$ + CHR$((d(t%) - 1) \ 14 + 3) + " "
          PRINT c$;
        ELSE
          PRINT RIGHT$(STR$(d(t%) - 57), 2) + "M ";
        END IF
        IF t% MOD 14 = 0 THEN PRINT
      NEXT
  END SELECT
  PRINT : PRINT
END SUB

DEFINT A-Z
SUB ShuffleDeck (d() AS INTEGER)
  Cards = UBOUND(d)
  FOR t = 1 TO Cards
    SWAP d(((RND * 32767) MOD Cards) + 1), d(t)
  NEXT
END SUB

