DECLARE SUB ScreenPocket (c$)
ScreenPocket "put"
Mn$ = "  JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC"
fill$ = STRING$(6, CHR$(178))
SCREEN 12
RANDOMIZE (TIMER + INP(64))
DO
   key$ = INKEY$
   IF key$ = "" THEN
      key$ = INKEY$
      o% = c%: WHILE o% = c%: c% = RND * 15: WEND
      e% = RND * 20 + 20: tx% = RND * 21 + 2: ty% = RND * 70 + 2
      FOR d% = 1 TO e%
         x% = RND * 639: y% = RND * 479
         i% = RND * 3 + 1: l% = RND * 150 + 50
         FOR t% = i% TO l% STEP i%
            IF key$ = "" THEN
               CIRCLE (x%, y%), t%, c%: key$ = INKEY$
               WAIT &H3DA, 8: WAIT &H3DA, 8, 8
               COLOR 15
               LOCATE tx%, ty%
               PRINT MID$(Mn$, VAL(DATE$) * 3, 3); " ";
               PRINT MID$(DATE$, 4, 2);
               LOCATE tx% + 1, ty%
               tm% = VAL(TIME$) + 12 * (VAL(TIME$) > 12)
               PRINT RIGHT$(STR$(tm%), 2);
               PRINT MID$(TIME$, 3, 3);
               PRINT MID$("ap", 1 - (VAL(TIME$) > 12), 1)
            END IF
         NEXT
      NEXT
      COLOR c%: LOCATE tx%, ty%: PRINT fill$;
      LOCATE tx% + 1, ty%: PRINT fill$; : COLOR 14
   END IF
LOOP WHILE key$ = ""
SCREEN 0
ScreenPocket "get"


SUB ScreenPocket (c$) STATIC
   DIM buf(4000) AS STRING * 1
   DEF SEG = &HB800
   IF c$ = "put" THEN
      CX% = CSRLIN: CY% = POS(0)
      FOR t% = 0 TO 3999: buf(t%) = CHR$(PEEK(t%)): NEXT
   END IF
   IF c$ = "get" THEN
      LOCATE CX%, CY%
      FOR t% = 0 TO 3999: POKE t%, ASC(buf(t%)): NEXT
   END IF
END SUB

