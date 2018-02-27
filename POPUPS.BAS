'_|_|_|   POPUPS.BAS
'_|_|_|   Example of popup menus in QBasic.
'_|_|_|   No warrantee, guarantee, or manatee given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (2/9/97)
DECLARE FUNCTION MenuS$ (M%)
DECLARE FUNCTION MenuWin% (x%, y%, Menu$)
p% = POS(0): c% = CSRLIN: Menu% = 1: poplin% = 1: popcol% = 1: CLS
DO
   act% = MenuWin%(poplin%, popcol%, MenuS$(Menu%))
   SELECT CASE Menu%
      CASE 1
         IF (act% = -1) OR (act% = 3) THEN
            Menu% = 3: act% = -1: poplin% = 6: popcol% = 60
         END IF
         IF (act% = -2) OR (act% = 2) THEN
            Menu% = 2: act% = -2: poplin% = 3: popcol% = 30
         END IF
      CASE 2
         IF (act% = -1) OR (act% = 11) THEN
            Menu% = 1: act% = -1: poplin% = 1: popcol% = 1
         END IF
         IF (act% = -2) OR (act% = 10) THEN
            Menu% = 3: act% = -2: poplin% = 6: popcol% = 60
         END IF
      CASE 3
         IF (act% = -1) OR (act% = 6) THEN
            Menu% = 2: act% = -1: poplin% = 3: popcol% = 30
         END IF
         IF (act% = -2) OR (act% = 5) THEN
            Menu% = 1: act% = -2: poplin% = 1: popcol% = 1
         END IF
   END SELECT
LOOP WHILE act% < 0
COLOR 2, 0:  LOCATE c%, p%:  PRINT "Your Choice = "; act%: SYSTEM
FUNCTION MenuS$ (M%)
   SELECT CASE M%
      CASE 1: M$ = "Choice Window;Item One;Window Two;Window Three;"
         M$ = M$ + "Item Four;Item Five;Item Six;Item Seven;"
         M$ = M$ + "Item Eight;Item Nine;Item Ten;"
      CASE 2: M$ = "Menu Items;Delete;Remove;Add;Concatenate;Itemize"
         M$ = M$ + ";Sort By Name;Sort By Size;Sort By Date;Sort By "
         M$ = M$ + " Extension;Next Directory;Previous Directory;"
      CASE 3: M$ = "Test Menu Items;Lawnmower;Sprinkler;Leaf Rake;Ga"
         M$ = M$ + "rden Hose;Next Menu;Previous Menu;Refrigerator;S"
         M$ = M$ + "tove;Kitchen Sink;"
      CASE ELSE: M$ = ""
   END SELECT: MenuS$ = M$
END FUNCTION
FUNCTION MenuWin% (x%, y%, Menu$)
   ox% = CSRLIN: oy% = POS(0): bs% = 160 * x% - 160 + y% * 2 - 2
   DIM M$(23): i% = -1: M% = 0: ndx% = 1: V& = &H3DA
   WHILE ndx% <> 0: nxt% = INSTR(ndx%, Menu$, ";")
      IF nxt% <> 0 THEN
         i% = i% + 1: M$(i%) = MID$(Menu$, ndx%, nxt% - ndx%)
         nxt% = nxt% + 1: IF LEN(M$(i%)) > M% THEN M% = LEN(M$(i%))
      END IF: ndx% = nxt%: es% = (x% + i% + 2) * 160 - 1
   WEND: M% = M% + 2: fil$ = STRING$(M%, " "): k% = 0: p% = 0: c% = 0
   FOR F% = 0 TO i%: M$(F%) = LEFT$(" " + M$(F%) + fil$, M%): NEXT
   DIM buf(4000) AS STRING * 1: s% = (M% + 2) * 2: DEF SEG = &HB800
   FOR t% = 0 TO es% - bs%
      IF (t% MOD 160) < s% THEN
         buf(t%) = CHR$(PEEK(t% + bs%))
      ELSE t% = 160 * (t% \ 160 + 1)
         WHILE (INP(V&) AND 8) = 0: WEND: WHILE (INP(V&) AND 8) <> 0: WEND
      END IF
   NEXT: LOCATE x%, y%, 0: COLOR 8, 1: x% = x% + 1
   PRINT CHR$(213); STRING$(M%, CHR$(205)); CHR$(184);
   FOR p% = 0 TO i%:
      WHILE (INP(V&) AND 8) = 0: WEND: WHILE (INP(V&) AND 8) <> 0: WEND
      LOCATE x% + p%, y%: COLOR 8: PRINT CHR$(179);
      COLOR 15 + 3 * (p% = 0): PRINT M$(p%); : COLOR 8: PRINT CHR$(179)
   NEXT: LOCATE x% + p%, y%: y% = y% + 1: p% = 1
   PRINT CHR$(212); STRING$(M%, CHR$(205)); CHR$(190);
   WHILE k% <> 13 AND k% <> 27
      LOCATE x% + p%, y%: COLOR 1, 7: PRINT M$(p%);
      k$ = "": WHILE k$ = "": k$ = INKEY$: WEND
      k% = ASC(k$): IF k% = 0 THEN k% = -ASC(MID$(k$, 2))
      LOCATE x% + p%, y%: COLOR 15, 1
      SELECT CASE k%
         CASE -72: PRINT M$(p%): p% = p% - 1 - i% * (p% = 1)
         CASE -80: PRINT M$(p%): p% = p% + 1 + i% * (p% = i%)
         CASE -75: p% = -1: k% = 13
         CASE -77: p% = -2: k% = 13
         CASE 27: p% = 0
      END SELECT
   WEND
   FOR t% = es% - bs% TO 0 STEP -1
      IF (t% MOD 160) < s% THEN
         POKE t% + bs%, ASC(buf(t%))
      ELSE t% = 160 * (t% \ 160) + s%
         WHILE (INP(V&) AND 8) = 0: WEND: WHILE (INP(V&) AND 8) <> 0: WEND
      END IF: NEXT: ERASE buf: LOCATE ox%, oy%: MenuWin% = p%
END FUNCTION
'_|_|_|   end   POPUPS.BAS
