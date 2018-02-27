DECLARE SUB Help ()
DECLARE SUB alarm ()
DECLARE SUB noise ()
DECLARE SUB Clipboard ()
DECLARE SUB LoadDoc (d%)
DECLARE SUB SaveDoc (d%)
DECLARE SUB ShowDoc (d%)
DECLARE SUB InfoBar (d%)
DECLARE SUB DocInit (d%)
DECLARE SUB SaveQry (d%)
DECLARE SUB CutText (d%)
DECLARE SUB PasteText (d%)
DECLARE SUB DeleteText (d%)
DECLARE SUB parmsinit (d%)
DECLARE SUB InsLine (d%, s$)
DECLARE SUB PutLine (d%, t$)
DECLARE SUB LineCat (d%, l%)
DECLARE SUB DocOpn (F$, buf%)
DECLARE SUB SetMark (d%, sh%)
DECLARE SUB CheckMark (d%, sh%)
DECLARE SUB ShowLine (d%, l%, s%)
DECLARE SUB OrderMark (d%, la%, lz%, ca%, cz%)
DECLARE FUNCTION GetKey% ()
DECLARE FUNCTION EdDoc% (d%)
DECLARE FUNCTION GetLine$ (d%)
DECLARE FUNCTION DelLine$ (d%)
DECLARE FUNCTION Msg% (m$, p$)
DECLARE FUNCTION GetStr$ (p$, d$, m%)
CLEAR ' $DYNAMIC
TYPE txt: l AS STRING * 80: END TYPE
TYPE parms
   ins AS INTEGER: top AS INTEGER: lin AS INTEGER: crs AS INTEGER
   tot AS INTEGER: mla AS INTEGER: mca AS INTEGER: mlz AS INTEGER
   mcz AS INTEGER: wid AS INTEGER: END TYPE
DIM SHARED D0(800) AS txt: DIM SHARED D1(800) AS txt
DIM SHARED D2(800) AS txt: DIM SHARED cfg(3) AS parms
DIM SHARED Doc$(6): Doc$(1) = COMMAND$: Doc$(2) = ""
DocInit 0: DocInit 1: DocInit 2: Doc$(4) = "": Doc$(5) = "INS"
IF Doc$(1) <> "" THEN DocOpn Doc$(1), 1:  ELSE Doc$(1) = "text1.txt"
ed% = 1: WHILE ed% <> 0: ed% = EdDoc%(ed%): WEND:
COLOR 2, 0: CLS : SYSTEM
WhatThe: Doc$(3) = STR$(ERR): RESUME NEXT

REM $STATIC
SUB alarm
   FOR t% = 0 TO 25: SOUND 2200 + 2 * t%, .1: SOUND 2225 + t%, .05: NEXT
END SUB

SUB CheckMark (d%, sh%)
   IF sh% THEN
      IF cfg(d%).mca = 0 THEN
         cfg(d%).mla = cfg(d%).lin: cfg(d%).mlz = cfg(d%).lin
         cfg(d%).mca = cfg(d%).crs: cfg(d%).mcz = cfg(d%).crs
      END IF
   ELSE cfg(d%).mca = 0
   END IF
END SUB

SUB Clipboard
   cfg(0).lin = 0: cfg(0).crs = 1: k% = 0
   Doc$(0) = "CLIPBOARD    [ESC] TO EXIT"
   WHILE k% <> 27
      l% = cfg(0).lin: cfg(0).mca = 1: cfg(0).mcz = 80: cfg(0).mla = l%
      cfg(0).mlz = l%: ShowDoc 0: InfoBar 0: k% = GetKey%
      SELECT CASE k%
         CASE -71: cfg(0).lin = 0: cfg(0).top = 0                 '__HOME*
         CASE -79: l% = cfg(0).tot - 23: IF l% < 0 THEN l% = 0     '__END*
            cfg(0).top = l%: cfg(0).lin = cfg(0).tot - 1
         CASE -73:                                             '__PAGE UP*
            IF l% <> 0 THEN
               c% = l% - 24: cfg(0).lin = -c% * (c% >= 0)
               c% = cfg(0).top - 24: cfg(0).top = -c% * (c% >= 0)
            END IF
         CASE -81                                            '__PAGE DOWN*
            t% = cfg(d%).tot - 1: p% = t% - 23: l% = l% + 24
            IF l% <= (t% + 23) THEN
               IF l% > t% THEN l% = t%
               cfg(0).lin = l%: l% = cfg(0).top + 24
               IF l% > p% THEN l% = p%
               cfg(0).top = -l% * (l% >= 0)
            END IF
         CASE -72: l% = l% - 1                                 '__CRSR UP*
            IF l% >= 0 THEN
               cfg(0).lin = l%: IF cfg(0).top > l% THEN cfg(0).top = l%
            END IF
         CASE -80: l% = l% + 1                             '__CURSOR DOWN*
            IF l% < cfg(0).tot THEN
               cfg(0).lin = l%: t% = cfg(0).top
               IF l% > (t% + 23) THEN cfg(0).top = t% + 1
            END IF
      END SELECT
   WEND
END SUB

SUB CutText (d%)
   IF cfg(d%).mca = 0 THEN t% = Msg%("No Text Marked", ""): EXIT SUB
   OrderMark d%, la%, lz%, ca%, cz%: DocInit 0
   r% = cfg(d%).lin: i% = lz% - la%: cfg(0).tot = i% + 1
   FOR l% = 0 TO i%
      cfg(0).lin = l%: cfg(d%).lin = la% + l%: t$ = GetLine$(d%)
      IF l% = i% THEN t$ = LEFT$(t$, cz%)
      IF l% = 0 THEN t$ = MID$(t$, ca%)
      D0(l%).l = t$
   NEXT: cfg(d%).lin = r%: noise
END SUB

SUB DeleteText (d%)
   IF cfg(d%).mca = 0 THEN t% = Msg%("No Text Marked", ""): EXIT SUB
   OrderMark d%, la%, lz%, ca%, cz%: l% = lz% - la%
   r% = la%: cfg(d%).crs = ca%: cfg(d%).mca = 0: cfg(d%).lin = la%
   t$ = MID$(LEFT$(" " + GetLine$(d%), ca%), 2): cfg(d%).lin = lz%
   p$ = RTRIM$(MID$(GetLine$(d%), cz% + 1)): b$ = t$ + p$
   IF la% = lz% THEN
      IF b$ = "" THEN b$ = DelLine$(d%):  ELSE PutLine d%, b$
   END IF
   IF lz% > la% THEN
      IF p$ = "" THEN lz% = lz% + 1:  ELSE PutLine d%, p$
      IF t$ <> "" THEN cfg(d%).lin = la%: la% = la% + 1: PutLine d%, t$
   END IF
   IF la% < lz% THEN
      l% = lz% - la%: z% = cfg(d%).tot - l%: cfg(d%).tot = z%
      FOR t% = la% TO z%
         IF d% = 1 THEN D1(t%).l = D1(t% + l%).l
         IF d% = 2 THEN D2(t%).l = D2(t% + l%).l
      NEXT
      FOR t% = z% TO 799
         IF d% = 1 THEN D1(t%).l = ""
         IF d% = 2 THEN D2(t%).l = ""
      NEXT
   END IF: cfg(d%).lin = r%: r% = r% - 1
   IF cfg(d%).tot < 0 THEN cfg(d%).tot = 1
   IF r% + 25 > cfg(d%).tot THEN r% = cfg(d%).tot - 24
   cfg(d%).top = r% * -(r% >= 0): noise
END SUB

FUNCTION DelLine$ (d%)
   l% = cfg(d%).lin: e% = cfg(d%).tot - 1
   IF d% = 1 THEN
      DelLine$ = RTRIM$(D1(cfg(1).lin).l)
      FOR t% = l% TO e% - 1: D1(t%).l = D1(t% + 1).l: NEXT
      D1(e%).l = SPACE$(80)
   END IF
   IF d% = 2 THEN
      DelLine$ = RTRIM$(D2(cfg(2).lin).l)
      FOR t% = l% TO e% - 1: D2(t%).l = D2(t% + 1).l: NEXT
      D2(e%).l = SPACE$(80)
   END IF: cfg(d%).tot = e% - (e% = l%): ShowDoc d%
END FUNCTION

SUB DocInit (d%)
   FOR t% = 0 TO 799
      IF d% = 0 THEN D0(t%).l = ""
      IF d% = 1 THEN D1(t%).l = ""
      IF d% = 2 THEN D2(t%).l = ""
   NEXT: parmsinit d%
END SUB

SUB DocOpn (F$, d%)
   ON ERROR GOTO WhatThe
   t% = 1: Doc$(3) = "ok": IF F$ <> "" THEN OPEN F$ FOR INPUT AS #1
   t$ = "File Error :: " + F$ + " :: Hit a Key"
   IF Doc$(3) <> "ok" THEN t% = Msg%(t$, ""): EXIT SUB
   WHILE t% = 1: l% = 0: DocInit d%
      WHILE ((l% < 800) AND (NOT EOF(1)))
         LINE INPUT #1, p$: p$ = RTRIM$(p$): cfg(d%).lin = l%
         WHILE INSTR(p$, CHR$(9)) <> 0
            p% = INSTR(p$, CHR$(9)): t$ = MID$(p$, p% + 1)
            t$ = "   " + t$: IF p% > 1 THEN t$ = LEFT$(p$, p% - 1) + t$
            p$ = t$
         WEND
         IF LEN(p$) > 79 THEN
            IF l% < 799 THEN
               PutLine d%, LEFT$(p$, 79): p$ = MID$(p$, 80)
               l% = l% + 1: cfg(d%).lin = l%
            END IF: p$ = LEFT$(p$, 79)
         END IF: PutLine d%, p$: l% = l% + 1
      WEND: t$ = "File too large. Load next section (Y/N)?"
      IF (NOT EOF(1)) THEN t% = Msg%(t$, "yn"):  ELSE t% = 0
   WEND: CLOSE 1
   cfg(d%).tot = l%: cfg(d%).lin = 0: alarm: Doc$(d%) = UCASE$(F$)
END SUB

FUNCTION EdDoc% (d%)
   DEF SEG = 0: EdDoc% = d%: IF d% = 0 THEN GOTO CYCLE
   ShowDoc d%: EDIT$ = "YES": s$ = SPACE$(80)
   WHILE EDIT$ = "YES"
      t$ = GetLine$(d%) + s$: l% = cfg(d%).lin: c% = cfg(d%).crs
      ShowDoc d%: InfoBar d%
      LOCATE l% - cfg(d%).top + 2, c%, 1, 13 * cfg(d%).ins, 13
      k% = GetKey%: sh% = ((PEEK(&H417) AND 3) <> 0): w% = cfg(d%).wid
      IF k% = 9 THEN
         t$ = LEFT$(t$, c% - 1) + "  " + MID$(t$, c%): k% = 32
         c% = c% + 2
      END IF
      SELECT CASE k%
         CASE 32 TO 254:                                          '__TEXT*
            IF cfg(d%).ins <> 0 THEN MID$(t$, c% + 1) = MID$(t$, c%)
            MID$(t$, c%) = CHR$(k%): t$ = RTRIM$(t$)
            IF (LEN(t$) > w%) THEN
               p$ = MID$(t$, w% + 1): t$ = LEFT$(t$, w%): t% = 0
               WHILE t% = 0: r$ = RIGHT$(t$, 1): l% = LEN(t$)
                  t% = INSTR("*+\]}:)/- ", r$) - (l% < (w% * .75))
                  IF t% = 0 THEN p$ = r$ + p$: t$ = LEFT$(t$, l% - 1)
               WEND: PutLine d%, t$: l% = cfg(d%).lin: t% = LEN(t$)
               IF l% < 799 THEN cfg(d%).lin = l% + 1: InsLine d%, p$
               IF c% > w% AND l% < 799 THEN c% = c% - t%: l% = l% + 1
               cfg(d%).lin = l%: IF c% > w% AND l% > 788 THEN c% = t%
            ELSE PutLine d%, t$
            END IF: cfg(d%).crs = c% + 1
         CASE 8: l% = cfg(d%).lin: t% = LEN(RTRIM$(t$))      '__BACKSPACE*
         IF (c% = 1) AND (l% = 0) THEN GOTO CYCLE
            IF c% > 1 THEN
               IF t% <> 0 THEN MID$(t$, c% - 1) = MID$(t$, c%) + " "
               cfg(d%).crs = c% - 1: PutLine d%, t$
            ELSE LineCat d%, l% - 1
            END IF
         CASE -83: t% = LEN(RTRIM$(t$))                         '__DELETE*
            IF c% <= t% THEN
               MID$(t$, c%) = MID$(t$, c% + 1) + " ": PutLine d%, t$
            ELSE LineCat d%, cfg(d%).lin
            END IF
         CASE -82: t$ = Doc$(6)                                 '__INSERT*
             IF sh% THEN
                IF t$ <> "" THEN InsLine d%, t$: cfg(d%).crs = 1
             ELSE
                cfg(d%).ins = ABS(cfg(d%).ins - 1): noise
             END IF
         CASE 13: l% = cfg(d%).lin: IF l% >= 799 THEN GOTO CYCLE '__ENTER*
            IF c% = 1 THEN p$ = "":  ELSE p$ = LEFT$(t$, c% - 1)
            PutLine d%, p$: cfg(d%).lin = l% + 1
            InsLine d%, MID$(t$, c%): cfg(d%).crs = 1
         CASE -61, -38: SaveQry d%: LoadDoc d%: GOTO CYCLE '__ALT/L or F3*
         CASE -60, -31: SaveDoc d%                         '__ALT/S or F2*
         CASE 27, -45: EDIT$ = "NO"     '__ESCAPE or CTRL/X : exit editor*
         CASE -94                       '__CTRL/F1 : switch to Document 1*
            IF Doc$(1) = "" THEN Doc$(1) = "text1.txt"
            EdDoc% = 1: GOTO CYCLE
         CASE -95                       '__CTRL/F2 : switch to Document 2*
            IF Doc$(2) = "" THEN Doc$(2) = "text2.txt"
            EdDoc% = 2: GOTO CYCLE
         CASE -96: Clipboard                                   '__CTRL/F3*
         CASE 25: Doc$(6) = DelLine$(d%): cfg(d%).crs = 1       '__CTRL/Y*
         CASE -71: CheckMark d%, sh%: cfg(d%).crs = 1             '__HOME*
            SetMark d%, sh%
         CASE -79: CheckMark d%, sh%: c% = LEN(RTRIM$(t$)) + 1     '__END*
            cfg(d%).crs = -c% * (c% < 81) - 80 * (c% > 80)
            SetMark d%, sh%
         CASE -119: CheckMark d%, sh%: cfg(d%).crs = 1       '__CTRL/HOME*
            cfg(d%).lin = cfg(d%).top: SetMark d%, sh%
         CASE -117: CheckMark d%, sh%: c% = cfg(d%).top + 23  '__CTRL/END*
            t% = cfg(d%).tot - 1: IF c% >= t% THEN c% = t%
            cfg(d%).lin = c%: t$ = RTRIM$(GetLine$(d%)): t% = LEN(t$)
            cfg(d%).crs = t% - (t% < 80): SetMark d%, sh%
         CASE -132: CheckMark d%, sh%: cfg(d%).top = 0    '__CTRL/PAGE UP*
            cfg(d%).lin = 0: cfg(d%).crs = 1: SetMark d%, sh%
         CASE -118: CheckMark d%, sh%: c% = cfg(d%).tot '__CTRL/PAGE DOWN*
            cfg(d%).top = -(c% - 24) * (c% > 23)
            cfg(d%).lin = c% - 1: cfg(d%).crs = 1: SetMark d%, sh%
         CASE -73: CheckMark d%, sh%                           '__PAGE UP*
            IF l% = 0 THEN GOTO CYCLE
            c% = l% - 24: cfg(d%).lin = -c% * (c% >= 0)
            c% = cfg(d%).top - 24: cfg(d%).top = -c% * (c% >= 0)
            SetMark d%, sh%
         CASE -81: CheckMark d%, sh%                         '__PAGE DOWN*
            t% = cfg(d%).tot - 1: p% = t% - 23
            l% = cfg(d%).lin + 24: IF l% > (t% + 23) THEN GOTO CYCLE
            IF l% > t% THEN l% = t%
            cfg(d%).lin = l%: l% = cfg(d%).top + 24
            IF l% > p% THEN l% = p%
            cfg(d%).top = -l% * (l% >= 0): SetMark d%, sh%
         CASE -72:  CheckMark d%, sh%                          '__CRSR UP*
            l% = l% - 1: IF l% < 0 THEN GOTO CYCLE
            cfg(d%).lin = l%: IF cfg(d%).top > l% THEN cfg(d%).top = l%
            SetMark d%, sh%
         CASE -80: CheckMark d%, sh%                       '__CURSOR DOWN*
            l% = l% + 1: IF l% >= cfg(d%).tot THEN GOTO CYCLE
            cfg(d%).lin = l%: t% = cfg(d%).top
            IF l% > (t% + 23) THEN cfg(d%).top = t% + 1
            SetMark d%, sh%
         CASE -75: CheckMark d%, sh%                       '__CURSOR LEFT*
            IF c% = 1 THEN GOTO CYCLE
            cfg(d%).crs = c% - 1: SetMark d%, sh%
         CASE -77: CheckMark d%, sh%                      '__CURSOR RIGHT*
            IF c% > 79 THEN GOTO CYCLE
            cfg(d%).crs = c% + 1: SetMark d%, sh%
         CASE -46: CutText d%                        '__ALT/C :: CUT TEXT*
         CASE -32: DeleteText d%                  '__ALT/D :: DELETE TEXT*
         CASE -25: PasteText d%                    '__ALT/P :: PASTE TEXT*
         CASE -59, -35: Help                       '__ALT/H OR F1 :: HELP*
         CASE -115: cfg(d%).wid = w% + (w% > 10)             '__CTRL/LEFT*
         CASE -116: cfg(d%).wid = w% - (w% < 79)            '__CTRL/RIGHT*
         CASE -62: SHELL "command.com"                 '__F4 :: DOS SHELL*
      END SELECT
   WEND: t$ = CHR$(13) + "yn " + CHR$(27): SaveQry d%
   EdDoc% = -d% * (Msg("Exit KISSED (Y/N)?", t$) > 2)
CYCLE: noise: DEF SEG
END FUNCTION

FUNCTION GetKey%
   t$ = "": WHILE t$ = "": t$ = INKEY$: WEND: GetKey% = ASC(t$)
   IF ASC(t$) = 0 THEN GetKey% = -ASC(MID$(t$, 2))
END FUNCTION

FUNCTION GetLine$ (d%)
   GetLine$ = "": IF d% = 1 THEN GetLine$ = D1(cfg(1).lin).l
   IF d% = 2 THEN GetLine$ = D2(cfg(2).lin).l
END FUNCTION

FUNCTION GetStr$ (p$, d$, m%)
   PCOPY 0, 1: SCREEN , , 1, 1: l% = 7 + LEN(p$) + m%: ins% = 1
   y% = 40 - l% \ 2: x% = 11: COLOR 9, 1: LOCATE x%, y%
   FOR t% = 1 TO 3: LOCATE , y%: PRINT STRING$(l%, CHR$(176)): NEXT
   LOCATE x% + 1, y% + 2: COLOR 15, 4: PRINT " "; p$; " "; : x% = CSRLIN
   y% = POS(0): PRINT SPACE$(m% + 1); : d$ = LEFT$(d$ + SPACE$(m%), m%)
   b$ = CHR$(29): p% = (LEN(RTRIM$(d$)) MOD m%) + 1
   WHILE t% <> 13: LOCATE , y%, 0: COLOR 14, 4: PRINT d$; : COLOR 1, 7
      LOCATE , y% + p% - 1, ins%, 13, 13: PRINT MID$(d$, p%, 1); b$;
      t% = GetKey%
      SELECT CASE t%
         CASE 32 TO 127
            IF ins% <> 0 THEN d$ = LEFT$(LEFT$(d$, p%) + MID$(d$, p%), m%)
            MID$(d$, p%) = CHR$(t%): IF p% < m% THEN p% = (p% MOD m%) + 1
         CASE 8
            IF p% > 1 THEN MID$(d$, p% - 1) = MID$(d$, p%) + " "
            p% = p% + (p% > 1): IF p% = 1 THEN SOUND 1000, .1
         CASE 27: d$ = "": t% = 13
         CASE -82: ins% = ABS(ins% - 1)
         CASE -83: MID$(d$, p%) = MID$(d$, p% + 1) + " "
         CASE -75: p% = ((p% - 2 + m%) MOD m%) + 1
         CASE -77: p% = (p% MOD m%) + 1
         CASE 25: d$ = SPACE$(m%): p% = 1
         CASE -71: p% = 1
         CASE -79: p% = LEN(RTRIM$(d$)): p% = p% - (p% <> m%)
      END SELECT
   WEND: SCREEN , , 0, 0: GetStr$ = RTRIM$(d$)
END FUNCTION

SUB Help
   SCREEN , , 1, 1: COLOR 11, 1: CLS : t$ = STRING$(80, CHR$(177))
   FOR t% = 1 TO 25: LOCATE t%, 1: PRINT t$; : NEXT: COLOR 14, 0
   LOCATE 3, 7, 0: PRINT "____NORMAL KEYS_______________"
   LOCATE , 7: PRINT " Left Arrow     (Cursor Left) "
   LOCATE , 7: PRINT " Right Arrow   (Cursor Right) "
   LOCATE , 7: PRINT " Up Arrow         (Cursor Up) "
   LOCATE , 7: PRINT " Down Arrow     (Cursor Down) "
   LOCATE , 7: PRINT " Page Up     (Cursor Page UP) "
   LOCATE , 7: PRINT " Page Down (Cursor Page Down) "
   LOCATE , 7: PRINT " Home           (Cursor Home) "
   LOCATE , 7: PRINT " End             (Cursor End) "
   LOCATE , 7: PRINT " Insert       (Toggle Insert) "
   LOCATE , 7: PRINT " Delete              (Delete) "
   LOCATE , 7: PRINT " F1                    (Help) "
   LOCATE , 7: PRINT " F2               (Load Text) "
   LOCATE , 7: PRINT " F3               (Save Text) "
   LOCATE , 7: PRINT " ESCAPE        (EXIT  EDITOR) "
   LOCATE 19, 7: PRINT "____SHIFT KEYS________________"
   LOCATE , 7: PRINT " Insert  (Insert CTRL/Y Line) "
   LOCATE , 7: PRINT "    Any Cursor movement key   "
   LOCATE , 7: PRINT "   with SHIFT will mark text. "
   LOCATE 3, 45: PRINT "____CONTROL KEYS______________"
   LOCATE , 45: PRINT " F1              (Document 1) "
   LOCATE , 45: PRINT " F2              (Document 2) "
   LOCATE , 45: PRINT " F3          (View Clipboard) "
   LOCATE , 45: PRINT " Home              (Page Top) "
   LOCATE , 45: PRINT " End            (Page Bottom) "
   LOCATE , 45: PRINT " Page Up           (Text top) "
   LOCATE , 45: PRINT " Page Down      (Text Bottom) "
   LOCATE , 45: PRINT " Left Arrow    (-line length) "
   LOCATE , 45: PRINT " Right Arrow   (+line length) "
   LOCATE , 45: PRINT " CTRL/Y         (Delete Line) "
   LOCATE 17, 45: PRINT "____ALT KEYS__________________"
   LOCATE , 45: PRINT " ALT/H                 (Help) "
   LOCATE , 45: PRINT " ALT/C    (Copy To Clipboard) "
   LOCATE , 45: PRINT " ALT/D   (Delete Marked Text) "
   LOCATE , 45: PRINT " ALT/P      (Paste ClipBoard) "
   LOCATE , 45: PRINT " ALT/X                 (EXIT) "
   t% = GetKey%: SCREEN , , 0, 0
END SUB

SUB InfoBar (d%)
   p$ = SPACE$(80): MID$(p$, 7) = "l" + MID$(STR$(cfg(d%).lin + 1), 2)
   MID$(p$, 27) = STR$(cfg(d%).tot) + " lines": MID$(p$, 40) = Doc$(d%)
   MID$(p$, 24) = CHR$(64 + d%): MID$(p$, 18) = Doc$(cfg(d%).ins + 4)
   MID$(p$, 13) = "w" + MID$(STR$(cfg(d%).wid), 2): LOCATE 1, 1
   MID$(p$, 2) = "c" + MID$(STR$(cfg(d%).crs), 2): COLOR 15, 1: PRINT p$;
END SUB

SUB InsLine (d%, s$)
   l% = cfg(d%).lin
   IF d% = 1 THEN
      FOR t% = 799 TO l% + 1 STEP -1: D1(t%).l = D1(t% - 1).l: NEXT
      D1(cfg(1).lin).l = s$
   END IF
   IF d% = 2 THEN
      FOR t% = 799 TO l% + 1 STEP -1: D2(t%).l = D2(t% - 1).l: NEXT
      D2(cfg(2).lin).l = s$
   END IF: IF cfg(d%).tot < 800 THEN cfg(d%).tot = cfg(d%).tot + 1
   ShowDoc d%
END SUB

SUB LineCat (d%, l%)
   r% = cfg(d%).lin: cfg(d%).lin = l% + 1: p$ = RTRIM$(GetLine$(d%))
   c% = cfg(d%).crs: cfg(d%).lin = l%: t$ = GetLine$(d%)
   IF r% = l% THEN t$ = LEFT$(t$, c% - 1)
   IF r% <> l% THEN t$ = RTRIM$(t$): cfg(d%).crs = LEN(t$) + 1
   t$ = t$ + p$: t% = LEN(t$): IF t% < 80 THEN p$ = DelLine$(d%)
   IF LEN(t$) > 79 THEN cfg(d%).lin = l% + 1: PutLine d%, MID$(t$, 80)
   cfg(d%).lin = l%: PutLine d%, LEFT$(t$, 79): ShowDoc d%
END SUB

SUB LoadDoc (d%)
   DN$ = GetStr$("Load File?", "", 32): IF DN$ <> "" THEN DocOpn DN$, d%
END SUB

FUNCTION Msg% (m$, p$)
   m% = LEN(m$): p$ = UCASE$(p$): PCOPY 0, 1: SCREEN , , 1, 1
   IF p$ = "" THEN p$ = CHR$(13) + CHR$(27) + " "
   WHILE INKEY$ <> "": WEND: c% = 37 - (m% + 1) \ 2: LOCATE 10, c%
   COLOR 9, 1: LOCATE 11, c%, 0: PRINT STRING$(m% + 6, CHR$(176))
   LOCATE , c%: PRINT CHR$(176); CHR$(176); : COLOR 14, 4
   PRINT " "; m$; " "; : COLOR 9, 1: PRINT CHR$(176); CHR$(176)
   LOCATE , c%: PRINT STRING$(m% + 6, CHR$(176)): alarm
   t% = 0: WHILE INSTR(p$, UCASE$(CHR$(t%))) < 1: t% = GetKey%: WEND
   SCREEN , , 0, 0: Msg% = INSTR(p$, UCASE$(CHR$(t%)))
END FUNCTION

SUB noise
   SOUND 1000, .1
END SUB

SUB OrderMark (d%, la%, lz%, ca%, cz%)
   ca% = cfg(d%).mca: la% = cfg(d%).mla
   cz% = cfg(d%).mcz: lz% = cfg(d%).mlz
   la& = la%: lz& = lz%: la& = la& * 80 + ca%: lz& = lz& * 80 + cz%
   IF la& > lz& THEN SWAP la%, lz%: SWAP ca%, cz%
END SUB

SUB parmsinit (d%)
   cfg(d%).ins = 1: cfg(d%).top = 0: cfg(d%).lin = 0: cfg(d%).crs = 1
   cfg(d%).tot = 1: cfg(d%).mla = 0: cfg(d%).mca = 0: cfg(d%).mlz = 0
   cfg(d%).mcz = 0: cfg(d%).wid = 70
END SUB

SUB PasteText (d%)
   IF cfg(0).tot = 1 AND RTRIM$(D0(0).l) = "" THEN
      t% = Msg%("No text in Clipboard", ""): EXIT SUB
   END IF: l% = cfg(d%).lin: r% = l%: c% = cfg(d%).crs
   cfg(d%).mca = 0: o% = cfg(0).tot: i% = o% - 1: e% = cfg(d%).tot
   IF (c% > 1) AND (i% = 0) THEN
      t$ = GetLine$(d%) + SPACE$(80)
      t$ = RTRIM$(LEFT$(t$, c% - 1) + RTRIM$(D0(0).l) + MID$(t$, c%))
      PutLine d%, LEFT$(t$, 79)
      IF LEN(t$) > 79 AND l% < 799 THEN
         cfg(d%).lin = l% + 1: InsLine d%, MID$(t$, 80): cfg(d%).lin = l%
      END IF: noise: EXIT SUB
   END IF
   FOR t% = 799 TO l% STEP -1
      IF t% - 0 >= 0 THEN
         IF d% = 1 THEN D1(t%).l = D1(t% - o%).l
         IF d% = 2 THEN D2(t%).l = D2(t% - o%).l
      END IF
   NEXT
   FOR t% = 0 TO i%
      IF d% = 1 THEN D1(t% + l%).l = D0(t%).l
      IF d% = 2 THEN D2(t% + l%).l = D0(t%).l
   NEXT: cfg(d%).tot = e% + o%: IF e% + o% > 800 THEN cfg(d%).tot = 800
   noise
END SUB

SUB PutLine (d%, t$)
   IF d% = 1 THEN D1(cfg(d%).lin).l = LEFT$(t$ + SPACE$(80), 80)
   IF d% = 2 THEN D2(cfg(d%).lin).l = LEFT$(t$ + SPACE$(80), 80)
END SUB

SUB SaveDoc (d%)
   ON ERROR GOTO WhatThe
   Doc$(3) = "ok": l% = cfg(d%).lin: cfg(d%).lin = 0: t% = cfg(d%).tot
   p$ = RTRIM$(GetLine$(d%))
   IF p$ = "" AND t% = 1 THEN t% = Msg%("No Text", ""): EXIT SUB
   DN$ = Doc$(d%): DN$ = GetStr$("Save As?", DN$, 32)
   IF DN$ = "" THEN cfg(d%).lin = l%: EXIT SUB
   Doc$(d%) = DN$: OPEN DN$ FOR OUTPUT AS #1
   IF Doc$(3) <> "ok" THEN
      t% = Msg%("File Error :: Hit a Key", "")
   ELSE
      FOR p% = 0 TO t% - 1
         cfg(d%).lin = p%: p$ = RTRIM$(GetLine$(d%))
         IF p$ = "" THEN p$ = " "
         PRINT #1, p$
      NEXT: cfg(d%).lin = l%: alarm: CLOSE 1
   END IF
END SUB

SUB SaveQry (d%)
   l% = cfg(d%).lin: cfg(d%).lin = 0: t% = cfg(d%).tot: cfg(d%).lin = l%
   p$ = RTRIM$(GetLine$(d%)): IF p$ = "" AND t% = 1 THEN noise: EXIT SUB
   t% = Msg%("Save Current Document (Y:N)?", " ny" + CHR$(13))
   IF t% > 2 THEN noise: SaveDoc d%:  ELSE alarm
END SUB

SUB SetMark (d%, sh%)
   IF sh% = 0 THEN EXIT SUB
   l% = cfg(d%).lin: c% = cfg(d%).crs: l& = l%: l& = l& * 80 + c%
   a% = cfg(d%).mla: a& = a%: a& = a% * 80 + cfg(d%).mca
   cfg(d%).mlz = l%: cfg(d%).mcz = c%
END SUB

SUB ShowDoc (d%)
   l% = cfg(d%).lin: t% = cfg(d%).top: COLOR 10, 0: LOCATE , , 0
   IF t% + 23 < l% THEN t% = l% - 23: cfg(d%).top = t%
   FOR b% = 0 TO 23
      l% = b% + t%
      IF l% > 799 THEN
         LOCATE 2 + b%, 1: PRINT SPACE$(80);
      ELSE
         ShowLine d%, l%, 2 + b%
      END IF
   NEXT
END SUB

SUB ShowLine (d%, l%, s%)
   la% = 0: lz% = 0: ca% = 0: cz% = 0
   IF cfg(d%).mca <> 0 THEN OrderMark d%, la%, lz%, ca%, cz%
   IF d% = 0 THEN t$ = D0(l%).l + SPACE$(80)
   IF d% = 1 THEN t$ = D1(l%).l + SPACE$(80)
   IF d% = 2 THEN t$ = D2(l%).l + SPACE$(80)
   LOCATE s%, 1: COLOR 10, 0
   IF ca% = 0 OR (la% > l%) OR (lz% < l%) THEN
      PRINT LEFT$(t$, 80);
   ELSE
      IF la% = lz% THEN
         IF ca% > 1 THEN PRINT LEFT$(t$, ca% - 1);
         COLOR 0, 3: PRINT MID$(t$, ca%, cz% - ca% + 1);
         COLOR 10, 0: PRINT MID$(t$, cz% + 1, 80 - cz%);
      ELSE
         IF (l% > la%) AND (l% < lz%) THEN
            COLOR 0, 3: PRINT LEFT$(t$, 80);
         ELSE
            IF l% = la% THEN
               IF ca% > 1 THEN PRINT LEFT$(t$, ca% - 1);
               COLOR 0, 3: PRINT MID$(t$, ca%, 81 - ca%);
            ELSE
               COLOR 0, 3: PRINT LEFT$(t$, cz%);
               COLOR 10, 0: PRINT MID$(t$, cz% + 1, 80 - cz%);
            END IF
         END IF
      END IF
   END IF
END SUB

