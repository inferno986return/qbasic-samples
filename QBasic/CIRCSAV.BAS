DECLARE SUB circlesaver ()
DECLARE SUB scrpocket (T$)
DECLARE SUB HoldForKey ()
'_|_|_|   CIRCSAV.BAS
'_|_|_|   This is an example of a simple graphics screensaver
'_|_|_|   which stores the text screen in a STATIC array and
'_|_|_|   provides a constantly changing screen afterward,
'_|_|_|   restoring the text screen when a key is pressed.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (2/7/96)
T$ = "~-.__.<([ This is a Test Screen ])>.__-~"
FOR T% = 1 TO 25
   LOCATE T%, 1: COLOR (T% MOD 14) + 1, T% MOD 8: PRINT T$; T$;
NEXT: LOCATE 1, 1: COLOR 15, 0
PRINT " hit any key to begin ": HoldForKey: : circlesaver



SUB circlesaver
   scrpocket "take": SCREEN 12
   RANDOMIZE (TIMER + INP(64)): fill$ = STRING$(5, CHR$(178))
   DO
      key$ = INKEY$
      IF key$ = "" THEN
         key$ = INKEY$: o% = c%
         WHILE o% = c%: c% = RND * 15: WEND
         e% = RND * 20 + 20: tx% = RND * 21 + 2: ty% = RND * 72 + 2
         FOR d% = 1 TO e%
            x% = RND * 639: y% = RND * 479
            i% = RND * 3 + 1: l% = RND * 150 + 50
            FOR T% = i% TO l% STEP i%
               IF key$ = "" THEN
                  CIRCLE (x%, y%), T%, c%: key$ = INKEY$
                  WAIT &H3DA, 8: WAIT &H3DA, 8, 8
                  LOCATE tx%, ty%, 0: PRINT LEFT$(TIME$, 5);
                  LOCATE tx% + 1, ty%: PRINT LEFT$(DATE$, 5);
               END IF
            NEXT
         NEXT: COLOR c%: LOCATE tx%, ty%: PRINT fill$;
         LOCATE tx% + 1, ty%: PRINT fill$; : COLOR 14
      END IF
   LOOP WHILE key$ = ""
   SCREEN 0: : scrpocket "put": HoldForKey
END SUB

SUB HoldForKey
   WHILE INKEY$ <> "": WEND: WHILE INKEY$ = "": WEND
END SUB

SUB scrpocket (T$) STATIC
   DIM buf(4000)  AS STRING * 1
   DEF SEG = &HB800
   IF T$ = "put" THEN
      FOR T% = 0 TO 3999: POKE T%, ASC(buf(T%)): NEXT
      LOCATE CX%, CY%, 1
   END IF
   IF T$ = "take" THEN
      FOR T% = 0 TO 3999: buf(T%) = CHR$(PEEK(T%)): NEXT
      CX% = CSRLIN: CY% = POS(0)
   END IF
END SUB

