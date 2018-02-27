DECLARE SUB WarpToEnd ()
DECLARE SUB WarpToStart ()
DECLARE SUB WordLeft ()
DECLARE SUB WordRight ()
DECLARE SUB DeleteToEOL ()
DECLARE SUB DeleteToBOL ()
DECLARE SUB DoTabs ()
DECLARE SUB SaveDoc ()
DECLARE SUB LoadDoc ()
DECLARE SUB ShowDocs ()
DECLARE SUB InitializeConfig (I%)
DECLARE SUB InitializeEditor (I%)
DECLARE SUB UpdateStats ()
DECLARE SUB Display (SL AS INTEGER)
DECLARE SUB Message (M$)
DECLARE SUB Backspace ()
DECLARE SUB CarriageReturn ()
DECLARE SUB GoHome ()
DECLARE SUB CursorUp ()
DECLARE SUB PageUp ()
DECLARE SUB CursorLeft ()
DECLARE SUB CursorRight ()
DECLARE SUB GoEnd ()
DECLARE SUB CursorDown ()
DECLARE SUB PageDown ()
DECLARE SUB InsertToggle ()
DECLARE SUB Delete ()
DECLARE SUB IndentToggle ()
DECLARE SUB AppendCharacter (K$)
DECLARE SUB Wrap (TXT$, Overflow$)
DECLARE SUB UseStr (N AS INTEGER)
DECLARE SUB BreakString (s$, l AS INTEGER)
DECLARE SUB IncrementLine ()
DECLARE SUB SetTop (D AS INTEGER)
DECLARE SUB MakeString (s$, l AS INTEGER)
DECLARE SUB PhreStr (l AS INTEGER)
DECLARE SUB DecrementLine ()
DECLARE FUNCTION GetStr$ (l%)
DECLARE SUB InitializeDocument (D AS INTEGER)
DECLARE SUB ScrPocket (save%)
DECLARE SUB MoveTop (D AS INTEGER)
DECLARE FUNCTION QueryExit% ()
DECLARE FUNCTION QueryResponse$ (Msg$, Allowed$)
DECLARE FUNCTION WalkTheListTo% (l AS INTEGER)
ON ERROR GOTO HandleErrors: '$DYNAMIC
TYPE TextIndex: NxtLine AS INTEGER: PrvLine AS INTEGER: END TYPE
TYPE DocumentData
   FirstLine AS INTEGER: LastLine AS INTEGER: Indent AS INTEGER
   NumberOfLines AS INTEGER: TopLine AS INTEGER: MarkBeginLine AS INTEGER
   MarkEndLine AS INTEGER: MarkBeginColumn AS INTEGER: Cat AS INTEGER
   MarkEndColumn AS INTEGER: Insert AS INTEGER: Margin AS INTEGER
   CursorLine AS INTEGER: CursorColumn AS INTEGER: LinkLine AS INTEGER
   PhysicalLine AS INTEGER: DocumentName AS STRING * 32: END TYPE
TYPE LledConfigurationData: TextFore AS INTEGER: TextBack AS INTEGER
   StatFore AS INTEGER: StatBack AS INTEGER: Dab AS INTEGER
   DefaultMargin AS INTEGER: END TYPE
MemoryFree& = FRE(-1): MemForText& = MemoryFree& - 70000
DIM SHARED TextLines AS INTEGER, ReportError AS INTEGER
TextLines = INT(MemForText& \ 80): t% = TextLines
DIM SHARED TSeg0(1 TO t%) AS STRING * 8, TSeg1(1 TO t%) AS STRING * 8
DIM SHARED TSeg2(1 TO t%) AS STRING * 8, TSeg3(1 TO t%) AS STRING * 8
DIM SHARED TSeg4(1 TO t%) AS STRING * 8, TSeg5(1 TO t%) AS STRING * 8
DIM SHARED TSeg6(1 TO t%) AS STRING * 8, TSeg7(1 TO t%) AS STRING * 8
DIM SHARED TSeg8(1 TO t%) AS STRING * 8, TSeg9(1 TO t%) AS STRING * 8
DIM SHARED Index(0 TO t%) AS TextIndex, TEXT AS STRING * 80
DIM SHARED USED AS STRING * 8000, ScrData(0) AS STRING * 1
DIM SHARED DocDat(1 TO 10) AS DocumentData, Doc AS INTEGER
DIM SHARED DocChanged(1 TO 10) AS INTEGER
DIM SHARED TextLinesUsed AS INTEGER, CFG AS LledConfigurationData
DIM SHARED ShiftStatus AS INTEGER, Shift AS INTEGER
DIM SHARED Ctrl AS INTEGER, Alt AS INTEGER
USED = "": Shift = 3: Ctrl = 4: Alt = 8: TextLinesUsed = 1
InitializeConfig 0: InitializeEditor 0: COLOR CFG.TextFore, CFG.TextBack
CLS : UpdateStats: Display 1: M$ = "WELCOME TO LLed. There are"
Message M$ + STR$(TextLines - 1) + " lines free for editing."
DO
   B% = DocDat(Doc).Insert * 13
   LOCATE DocDat(Doc).CursorLine, DocDat(Doc).CursorColumn, 1, B%, 13
   DO: K$ = INKEY$: LOOP WHILE K$ = ""
   DEF SEG = &H40: ShiftStatus = PEEK(&H17) AND 15
   K% = ASC(K$): IF K% = 0 THEN K% = -ASC(MID$(K$, 2))
   IF DocChanged(Doc) THEN
      SELECT CASE K%
         CASE -60, -61, -35, -38: DocChanged(Doc) = 0
      END SELECT
   ELSE
      SELECT CASE K%
         CASE 8, 13, -83, 7, -17, -119, IS > 31
             DocChanged(Doc) = -1
      END SELECT
   END IF
   SELECT CASE K%
      CASE 1: WordLeft                          'ctrl/a
      CASE 6: WordRight                         'ctrl/f
      CASE 8: Backspace                         'backspace
      CASE 9: DoTabs                            'tab-ctrl/i
      CASE 13: CarriageReturn                   'return
      CASE 27, 17, -45: IF QueryExit% THEN END  'alt/x-ctrl/q-esc
      CASE -23: IndentToggle                    'alt/i
      CASE -59, -35: ShowDocs                   'f1-alt/h
      CASE -60, -31: SaveDoc                    'f2-alt/s
      CASE -61, -38: LoadDoc                    'f3-alt/l
      CASE -71: GoHome                          'home
      CASE -72, 5: CursorUp                     'up-ctrl/e
      CASE -73, 18: PageUp                      'Pgup-ctrl/r
      CASE -75, 19: CursorLeft                  'left-ctrl/s
      CASE -77, 4: CursorRight                  'right-ctrl/d
      CASE -79: GoEnd                           'end
      CASE -80, 24: CursorDown                  'down-ctrl/x
      CASE -81, 3: PageDown                     'Pgdn-ctrl/c
      CASE -82, 21: InsertToggle                'ins-ctrl/u
      CASE -83, 7: Delete                       'del-ctrl/g
      CASE -117: DeleteToEOL                    'ctrl/end
      CASE -118: WarpToEnd                      'ctrl/Pgdn
      CASE -119: DeleteToBOL                     'ctrl/home
      CASE -132: WarpToStart                    'ctrl/Pgup
      CASE IS > 31: AppendCharacter K$          'text keys
      CASE ELSE: 'Eat other keys
   END SELECT: UpdateStats
LOOP
HandleErrors:
   ReportError = ERR: RESUME NEXT

REM $STATIC
SUB AppendCharacter (K$)
   c% = DocDat(Doc).CursorColumn: M% = DocDat(Doc).Margin
   l% = LEN(RTRIM$(TEXT)): I% = DocDat(Doc).Insert: K% = LEN(K$)
   IF (l% < M%) AND (c% < M%) THEN
      IF I% > 0 THEN
         FOR t% = 1 TO K%: TEXT = LEFT$(TEXT, c%) + MID$(TEXT, c%): NEXT
      END IF: MID$(TEXT, c%) = K$: DocDat(Doc).CursorColumn = c% + K%
      LOCATE , 1: PRINT TEXT;
      IF c% < M% THEN EXIT SUB:  ELSE CarriageReturn: EXIT SUB
   END IF: Temp$ = TEXT + SPACE$(80)
   IF I% > 0 THEN Temp$ = LEFT$(Temp$, c%) + MID$(Temp$, c%)
   MID$(Temp$, c%) = K$: DocDat(Doc).CursorColumn = c% + K%
   l% = LEN(RTRIM$(Temp$)): IF c% > (l% + 1) THEN l% = c% - 1
   Temp$ = LEFT$(Temp$, l%): Over$ = "": Wrap Temp$, Over$
   PL% = DocDat(Doc).PhysicalLine: LL% = DocDat(Doc).LinkLine
   I% = DocDat(Doc).Indent: IL% = LEN(Temp$) - LEN(LTRIM$(Temp$))
   IF c% > LEN(Temp$) THEN
      UseStr PL%: TEXT = Temp$ + Over$
      IF PL% = 0 THEN LOCATE , 1: PRINT TEXT; : EXIT SUB
      IF I% <> 0 THEN Over$ = SPACE$(IL%) + Over$
      TEXT = Temp$: BreakString Over$, LL%: DocDat(Doc).Cat = 0
      BreakString Temp$, DocDat(Doc).LinkLine: IncrementLine
      SetTop 1: Display 1
      DocDat(Doc).CursorColumn = c% - LEN(Temp$) + 1 + (IL% * I%)
      EXIT SUB
   END IF: NX% = Index(LL%).NxtLine: Concat% = DocDat(Doc).Cat
   NXL$ = "": IF (NX% <> 0) AND Concat% = 1 THEN MakeString NXL$, NX%
   IF I% <> 0 THEN NXL$ = MID$(NXL$, IL% + 1)
   NXL$ = RTRIM$(NXL$): LNXL% = LEN(NXL$): LO% = LEN(Over$) + LNXL% + 1
   IF (Concat% = 0) OR (NX% = 0) OR (LO% > M%) THEN
      UseStr PL%: TEXT = Temp$ + Over$: NXL$ = ""
      IF PL% = 0 THEN LOCATE , 1: PRINT TEXT; : EXIT SUB
      LL% = DocDat(Doc).LinkLine: NX% = Index(LL%).NxtLine
   END IF: BreakString SPACE$(I% * IL%) + Over$ + " " + NXL$, NX%
   BreakString Temp$, LL%: TEXT = Temp$: Display 1: DocDat(Doc).Cat = 1
END SUB

SUB Backspace
   c% = DocDat(Doc).CursorColumn - 1: l% = DocDat(Doc).PhysicalLine
   LL% = DocDat(Doc).LinkLine: t% = DocDat(Doc).TopLine
   IF (c% = 0) AND (l% = 1) THEN SOUND 999, 1: EXIT SUB
   IF c% > 0 THEN
      IF (ShiftStatus AND Shift) = 0 THEN
         MID$(TEXT, c%) = (MID$(TEXT, c% + 1) + " ")
      ELSE
         E% = c% + 1
         IF MID$(TEXT, c%, 1) = " " THEN
            DO: c% = c% - 1
            LOOP WHILE (MID$(TEXT, c%, 1) = " ") AND (c% > 0)
         ELSE
            DO: c% = c% - 1
            LOOP WHILE (MID$(TEXT, c%, 1) <> " ") AND (c% > 0)
         END IF: t$ = "": IF c% > 0 THEN t$ = LEFT$(TEXT, c%)
         c% = c% + 1: TEXT = t$ + MID$(TEXT, E%)
      END IF
      BreakString TEXT, LL%: LOCATE , 1: PRINT TEXT;
      DocDat(Doc).CursorColumn = c%
   ELSE PL% = Index(LL%).PrvLine: MakeString t$, PL%: t$ = RTRIM$(t$)
      Over$ = RTRIM$(TEXT): c% = LEN(RTRIM$(t$)) + 1
      IF Over$ <> "" THEN
         LT% = LEN(Over$): t$ = t$ + Over$: Wrap t$, Over$
      END IF: TEXT = t$
      IF Over$ = "" THEN
         PhreStr LL%: BreakString t$, PL%
         DocDat(Doc).PhysicalLine = l% - 1
         DocDat(Doc).LinkLine = PL%: DocDat(Doc).CursorColumn = c%
      ELSE
         BreakString Over$, LL%: BreakString t$, PL%
         DocDat(Doc).LinkLine = PL%: DocDat(Doc).PhysicalLine = l% - 1
         DocDat(Doc).CursorColumn = c%
      END IF: DocDat(Doc).Cat = 0: SetTop -1: Display 1
   END IF
END SUB

SUB BreakString (s$, l AS INTEGER)
   TSeg0(l) = LEFT$(s$, 8): TSeg1(l) = MID$(s$, 9, 8)
   TSeg2(l) = MID$(s$, 17, 8): TSeg3(l) = MID$(s$, 25, 8)
   TSeg4(l) = MID$(s$, 33, 8): TSeg5(l) = MID$(s$, 41, 8)
   TSeg6(l) = MID$(s$, 49, 8): TSeg7(l) = MID$(s$, 57, 8)
   TSeg8(l) = MID$(s$, 65, 8): TSeg9(l) = MID$(s$, 73, 8)
END SUB

SUB CarriageReturn
   IF TextLinesUsed = TextLines THEN Message "Out of Memory": EXIT SUB
   Indnt$ = SPACE$(LEN(TEXT) - LEN(LTRIM$(TEXT)))
   c% = DocDat(Doc).CursorColumn: l% = LEN(RTRIM$(TEXT)): t$ = "":
   IF c% <= l% THEN t$ = MID$(TEXT, c%): MID$(TEXT, c%) = SPACE$(80)
   BreakString t$, (DocDat(Doc).LinkLine): UseStr DocDat(Doc).PhysicalLine
   BreakString TEXT, DocDat(Doc).LinkLine: IncrementLine
   TEXT = t$: c% = 0: DocDat(Doc).Cat = 0
   IF DocDat(Doc).Indent <> 0 THEN
      TEXT = Indnt$ + LTRIM$(t$): c% = LEN(Indnt$)
      IF c% = 80 THEN Indnt$ = "": c% = 0
   END IF: BreakString TEXT, DocDat(Doc).LinkLine
   DocDat(Doc).CursorColumn = 1 + c%: SetTop 1: Display 1
END SUB

SUB CursorDown
   NL% = DocDat(Doc).NumberOfLines: PL% = DocDat(Doc).PhysicalLine
   LL% = DocDat(Doc).LinkLine: DocDat(Doc).Cat = 0
   IF Index(LL%).NxtLine = 0 THEN SOUND 999, 1: EXIT SUB
   BreakString TEXT, LL%: IncrementLine: t% = DocDat(Doc).TopLine
   SetTop 1: IF DocDat(Doc).TopLine <> t% THEN Display 1
END SUB

SUB CursorLeft : c% = DocDat(Doc).CursorColumn
   DocDat(Doc).CursorColumn = c% + (c% > 1): END SUB

SUB CursorRight : c% = DocDat(Doc).CursorColumn
   DocDat(Doc).CursorColumn = c% - (c% < 80): END SUB

SUB CursorUp
   NL% = DocDat(Doc).NumberOfLines: PL% = DocDat(Doc).PhysicalLine
   LL% = DocDat(Doc).LinkLine: DocDat(Doc).Cat = 0
   IF Index(LL%).PrvLine = 0 THEN SOUND 999, 1: EXIT SUB
   BreakString TEXT, LL%: DecrementLine: t% = DocDat(Doc).TopLine
   SetTop -1: IF DocDat(Doc).TopLine <> t% THEN Display 1
END SUB

SUB DecrementLine
   IF Index(DocDat(Doc).LinkLine).PrvLine = 0 THEN EXIT SUB
   DocDat(Doc).LinkLine = Index(DocDat(Doc).LinkLine).PrvLine
   DocDat(Doc).PhysicalLine = DocDat(Doc).PhysicalLine - 1
   MakeString TEXT, DocDat(Doc).LinkLine: DocDat(Doc).Cat = 0
END SUB

SUB Delete
   c% = DocDat(Doc).CursorColumn: t$ = RTRIM$(TEXT): TL% = LEN(t$)
   IF c% <= TL% THEN
      IF (ShiftStatus AND Shift) = 0 THEN
         MID$(TEXT, c%) = MID$(TEXT, c% + 1): LOCATE , 1
         PRINT TEXT; : BreakString TEXT, DocDat(Doc).LinkLine
      ELSE
         E% = c%: IF c% = 0 THEN t$ = "":  ELSE t$ = LEFT$(TEXT, c% - 1)
         IF MID$(TEXT, c%, 1) = " " THEN
            DO: c% = c% + 1
            LOOP WHILE (MID$(TEXT, c%, 1) = " ") AND (c% <= TL%)
         ELSE
            DO: c% = c% + 1
            LOOP WHILE (MID$(TEXT, c%, 1) <> " ") AND (c% <= TL%)
         END IF: TEXT = t$ + MID$(TEXT, c%): LOCATE , 1: PRINT TEXT
      END IF
   ELSE
      LL% = DocDat(Doc).LinkLine: l% = DocDat(Doc).PhysicalLine
      IF Index(LL%).NxtLine = 0 THEN SOUND 999, 1: EXIT SUB
      t$ = LEFT$(TEXT, c% - 1): NL% = Index(LL%).NxtLine
      MakeString Over$, NL%: t$ = t$ + Over$: Wrap t$, Over$: TEXT = t$
      IF Over$ = "" THEN
         PhreStr LL%: LL% = Index(LL%).NxtLine
         DocDat(Doc).LinkLine% = LL%: BreakString t$, LL%
      ELSE BreakString t$, LL%: BreakString Over$, NL%
      END IF: Display DocDat(Doc).CursorLine
   END IF: DocDat(Doc).Cat = 0: END SUB

SUB DeleteToBOL : c% = DocDat(Doc).CursorColumn
   IF c% > 1 THEN
      TEXT = MID$(TEXT, c%): DocDat(Doc).CursorColumn = 1
      LOCATE , 1: PRINT TEXT; : DocChanged(Doc) = -1
   END IF: END SUB

SUB DeleteToEOL : c% = DocDat(Doc).CursorColumn
   IF c% = 1 THEN TEXT = "":  ELSE TEXT = LEFT$(TEXT, c% - 1)
   LOCATE , 1: PRINT TEXT; : END SUB

SUB Display (SL AS INTEGER)
   l% = WalkTheListTo%(DocDat(Doc).TopLine + SL - 1): s$ = SPACE$(80)
   FOR t% = SL TO 24: LOCATE t%, 1, 0
      IF l% > 0 THEN
         PRINT TSeg0(l%); : PRINT TSeg1(l%); : PRINT TSeg2(l%);
         PRINT TSeg3(l%); : PRINT TSeg4(l%); : PRINT TSeg5(l%);
         PRINT TSeg6(l%); : PRINT TSeg7(l%); : PRINT TSeg8(l%);
         PRINT TSeg9(l%); : l% = Index(l%).NxtLine
      ELSE PRINT s$;
      END IF: NEXT: END SUB

SUB DoTabs : K$ = SPACE$(CFG.Dab): AppendCharacter K$: END SUB

FUNCTION GetStr$ (l%)
   X% = CSRLIN: Y% = POS(0): t$ = SPACE$(l%)
   ins% = 1: c% = 0: LOCATE , , 1, 13, 13
   DO: LOCATE X%, Y%, 0: PRINT t$
      LOCATE X%, Y% + c%, 1, 1 + 12 * ins%, 13
      DO: K$ = INKEY$: LOOP WHILE K$ = ""
      K% = ASC(K$):  IF K% = 0 THEN K% = -ASC(MID$(K$, 2))
      SELECT CASE K%
         CASE 32 TO 126
            IF ins% > 0 THEN
               t$ = LEFT$(LEFT$(t$, c% + 1) + MID$(t$, c% + 2), l%)
            END IF: MID$(t$, c% + 1, 1) = K$: c% = c% - ((c% + 1) < l%)
         CASE 8
            IF c% > 0 THEN t$ = LEFT$(t$, c%) + MID$(t$, c% + 2) + " "
            c% = c% + (c% > 0)
         CASE 13: EXIT DO
         CASE 27: EXIT FUNCTION
         CASE -82: ins% = ABS(ins% - 1)
         CASE -75: c% = (c% + l% - 1) MOD l%
         CASE -77: c% = (c% + 1) MOD l%
         CASE -71: c% = 0
         CASE -79: c% = LEN(RTRIM$(t$)): c% = c% + (c% = l%)
         CASE -83: t$ = LEFT$(t$, c%) + MID$(t$, c% + 2) + " "
      END SELECT: LOOP: PRINT : GetStr$ = RTRIM$(t$)
END FUNCTION

SUB GoEnd
   c% = DocDat(Doc).CursorColumn: l% = LEN(RTRIM$(TEXT)) + 1
   IF c% <> l% THEN DocDat(Doc).CursorColumn = l%:  ELSE SOUND 999, 1
END SUB

SUB GoHome
   c% = DocDat(Doc).CursorColumn
   IF c% = 1 THEN SOUND 999, 1: EXIT SUB
   IF DocDat(Doc).Indent = 0 THEN DocDat(Doc).CursorColumn = 1: EXIT SUB
   FOR H% = 1 TO 80: IF MID$(TEXT, H%, 1) <> " " THEN EXIT FOR
   NEXT: IF (c% = H%) OR (H% > 80) THEN c% = 1:  ELSE c% = H%
   DocDat(Doc).CursorColumn = c%: END SUB

SUB IncrementLine
   IF Index(DocDat(Doc).LinkLine).NxtLine = 0 THEN EXIT SUB
   DocDat(Doc).LinkLine = Index(DocDat(Doc).LinkLine).NxtLine
   DocDat(Doc).PhysicalLine = DocDat(Doc).PhysicalLine + 1
   MakeString TEXT, DocDat(Doc).LinkLine: DocDat(Doc).Cat = 0
END SUB

SUB IndentToggle
   I% = DocDat(Doc).Indent: DocDat(Doc).Indent = ABS(I% - 1)
   IF I% = 0 THEN SOUND 333, 1:  ELSE SOUND 666, 1
END SUB

SUB InitializeConfig (I%)
   CFG.TextFore = 15: CFG.TextBack = 1: CFG.StatFore = 0
   CFG.StatBack = 7: CFG.Dab = 3: CFG.DefaultMargin = 70
   H% = FREEFILE: ReportError = 0: OPEN "LLED.CFG" FOR INPUT AS #H%
   IF ReportError = 0 THEN
      WHILE NOT EOF(H%)
         LINE INPUT #H%, c$: c$ = UCASE$(c$)
         IF INSTR(c$, "TAB") THEN CFG.Dab = VAL(c$)
         IF INSTR(c$, "MARGIN") THEN CFG.DefaultMargin = VAL(c$)
         IF INSTR(c$, "TEXT BACK") THEN CFG.TextBack = VAL(c$)
         IF INSTR(c$, "TEXT FORE") THEN CFG.TextFore = VAL(c$)
         IF INSTR(c$, "STAT BACK") THEN CFG.StatBack = VAL(c$)
         IF INSTR(c$, "STAT FORE") THEN CFG.StatFore = VAL(c$)
      WEND
   END IF
END SUB

SUB InitializeDocument (D AS INTEGER)
   DocDat(D).Insert = 1: DocDat(D).TopLine = 1: DocDat(D).LastLine = 0
   DocDat(Doc).Cat = 0: DocDat(D).LinkLine = 0: DocDat(D).FirstLine = 0
   DocDat(D).Indent = 1: DocDat(D).CursorLine = 1
   DocDat(D).NumberOfLines = 0: DocDat(D).PhysicalLine = 0
   DocDat(D).CursorColumn = 1: DocDat(D).MarkBeginLine = 0
   DocDat(D).MarkEndLine = 0: DocDat(D).MarkBeginColumn = 0
   DocDat(D).MarkEndColumn = 0: DocDat(D).Margin = CFG.DefaultMargin
   DocDat(D).DocumentName = "LLED" + MID$(STR$(D), 2) + ".TXT"
END SUB

SUB InitializeEditor (I%) : TextLinesUsed = 1: Doc = 1
   FOR t% = 1 TO TextLines
      TSeg0(t%) = "": TSeg1(t%) = "": TSeg2(t%) = "": TSeg3(t%) = ""
      TSeg4(t%) = "": TSeg5(t%) = "": TSeg6(t%) = "": TSeg7(t%) = ""
      TSeg8(t%) = "": TSeg9(t%) = "": NEXT: InitializeDocument 1
   FOR t% = 2 TO 10:
     DocDat(t%) = DocDat(1): DocChanged(t%) = 0: NEXT
   Doc = 1: UseStr 1: END SUB

SUB InsertToggle
   I% = DocDat(Doc).Insert: DocDat(Doc).Insert = ABS(I% - 1)
   IF I% = 0 THEN SOUND 333, 1:  ELSE : SOUND 666, 1
END SUB

SUB LoadDoc
END SUB

SUB MakeString (s$, l AS INTEGER)
   s$ = TSeg0(l) + TSeg1(l) + TSeg2(l) + TSeg3(l) + TSeg4(l)
   s$ = s$ + TSeg5(l) + TSeg6(l) + TSeg7(l) + TSeg8(l) + TSeg9(l)
END SUB

SUB Message (M$) : ScrPocket -1: COLOR 14, 4
  LOCATE 13, 39 - LEN(M$) / 2, 0: PRINT " "; M$; " "
  FOR t% = 10 TO 50: SOUND 100 - t, .04: SOUND 999, .07: NEXT
  WHILE INKEY$ <> "": WEND: WHILE INKEY$ = "": WEND
  COLOR 15, 1: ScrPocket 0: END SUB

SUB MoveTop (D AS INTEGER)
   l% = DocDat(Doc).CursorLine: t% = DocDat(Doc).TopLine
   PL% = DocDat(Doc).PhysicalLine: NL% = DocDat(Doc).NumberOfLines
   BreakString TEXT, DocDat(Doc).LinkLine: DocDat(Doc).Cat = 0
   IF D < 0 THEN
      IF PL% = 1 THEN SOUND 999, 1: EXIT SUB
      IF PL% > -D THEN
         PL% = PL% + D
         IF t% <= -D THEN t% = 1: l% = PL%:  ELSE t% = t% + D
      ELSE l% = 1: PL% = 1
      END IF
   ELSE
   IF PL% = NL% THEN SOUND 999, 1: EXIT SUB
      IF (PL% + D) <= NL% THEN
         PL% = PL% + D
         IF (t% + 24 + D) > NL% THEN
            t% = NL% - 23: l% = 24 - (NL% - PL%)
         ELSE t% = t% + D
         END IF
      ELSE
         t% = 1: l% = NL%: PL% = NL%
         IF NL% > 24 THEN t% = NL% - 23: l% = l% - t% + 1
      END IF
   END IF
   DocDat(Doc).CursorLine = l%: DocDat(Doc).PhysicalLine = PL%
   DocDat(Doc).TopLine = t%: LL% = WalkTheListTo(PL%)
   DocDat(Doc).LinkLine = LL%: MakeString TEXT, LL%: Display 1
END SUB

SUB PageDown : MoveTop 24: END SUB

SUB PageUp : MoveTop -24: END SUB

SUB PhreStr (l AS INTEGER)
   MID$(USED, l, 1) = " ": TextLinesUsed = TextLinesUsed - 1
   DocDat(Doc).NumberOfLines = DocDat(Doc).NumberOfLines - 1
   BreakString "", l: Nxt% = Index(l).NxtLine: Prv% = Index(l).PrvLine
   IF Nxt% = 0 THEN
      DocDat(Doc).LastLine = Prv%: Index(Prv%).NxtLine = 0
   ELSE Index(Prv%).NxtLine = Nxt%
   END IF   '_|_|   Unlink from previous line
   IF Prv% = 0 THEN
      DocDat(Doc).FirstLine = Nxt%: Index(Nxt%).PrvLine = 0
   ELSE Index(Nxt%).PrvLine = Prv%
   END IF   '_|_|   Unlink from next line
END SUB

FUNCTION QueryExit%
   QueryExit% = (QueryResponse$("Exit LLED (Y/N)?", "YN") = "Y")
END FUNCTION

FUNCTION QueryResponse$ (Msg$, Allowed$)
   Allowed$ = UCASE$(Allowed$): IF Allowed$ = "" THEN EXIT FUNCTION
   ScrPocket -1: COLOR 14, 4: LOCATE 13, 39 - LEN(Msg$) / 2, 0
   PRINT " "; Msg$; " "
   FOR t% = 1 TO 20: SOUND 2000, .05: SOUND 7000, .07: NEXT
   FOR t% = 100 TO 300 STEP 20
      SOUND 2000 + t% * 5, .07: SOUND 5000 + t% * 2, .03: NEXT
   WHILE INKEY$ <> "": WEND
   DO: DO: K$ = UCASE$(INKEY$): LOOP WHILE K$ = ""
   LOOP WHILE INSTR(Allowed$, K$) < 1
   COLOR 15, 1: ScrPocket 0: QueryResponse$ = K$
END FUNCTION

SUB SaveDoc
END SUB

SUB ScrPocket (save%) : DEF SEG = &HB800
   IF save% THEN
      REDIM ScrData(4000) AS STRING * 1
      FOR t% = 0 TO 3999: ScrData(t%) = CHR$(PEEK(t%)): NEXT
   ELSE
      FOR t% = 0 TO 3999: POKE t%, ASC(ScrData(t%)): NEXT: ERASE ScrData
   END IF: END SUB

SUB SetTop (D AS INTEGER)
   l% = DocDat(Doc).CursorLine: t% = DocDat(Doc).TopLine
   PL% = DocDat(Doc).PhysicalLine: NL% = DocDat(Doc).NumberOfLines
   IF (PL% > NL%) OR (PL% < 1) THEN SOUND 999, 1: EXIT SUB
   IF D < 0 THEN
      IF l% = 1 THEN t% = t% - 1:  ELSE l% = l% - 1
   ELSE
      IF l% > 23 THEN t% = t% + 1:  ELSE l% = l% + 1
   END IF
   DocDat(Doc).CursorLine = l%: DocDat(Doc).TopLine = t%
END SUB

SUB ShowDocs
   COLOR 10, 1: CLS
   UL$ = CHR$(213) + CHR$(209): LL$ = CHR$(212) + CHR$(207)
   UR$ = CHR$(209) + CHR$(184): LR$ = CHR$(207) + CHR$(190)
   H$ = CHR$(205): V$ = CHR$(179): V$ = V$ + V$
   PRINT UL$; STRING$(75, H$); UR$
   FOR t% = 1 TO 10: PRINT V$; SPACE$(75); V$: NEXT
   PRINT LL$; STRING$(75, H$); LR$
   LOCATE 3, 6: PRINT "crtl/a       WordLeft"
   LOCATE 4, 6: PRINT "crtl/c       Page Down"
   LOCATE 5, 6: PRINT "crtl/d       Cursor Right"
   LOCATE 6, 6: PRINT "crtl/e       Cursor Up"
   LOCATE 7, 6: PRINT "crtl/f       WordRight"
   LOCATE 8, 6: PRINT "crtl/g       Delete"
   LOCATE 9, 6: PRINT "crtl/i       Tab"
   LOCATE 10, 6: PRINT "crtl/q       Quit Editor"
   LOCATE 3, 43: PRINT "crtl/r       Page Up"
   LOCATE 4, 43: PRINT "crtl/s       Cursor Left"
   LOCATE 5, 43: PRINT "crtl/u       Toggle Insert"
   LOCATE 6, 43: PRINT "crtl/x       Cursor Down"
   LOCATE 7, 43: PRINT "crtl/end     Delete to EOL"
   LOCATE 8, 43: PRINT "crtl/Pgdn    Warp to End"
   LOCATE 9, 43: PRINT "crtl/Home    Delete to BOL"
   LOCATE 10, 43: PRINT "crtl/Pgup    Warp to Start"
   LOCATE 14, 1: PRINT UL$; STRING$(75, H$); UR$
   FOR t% = 15 TO 24: LOCATE t%, 1: PRINT V$; SPACE$(75); V$; : NEXT
   LOCATE 25, 1: PRINT LL$; STRING$(75, H$); LR$;
   LOCATE 15, 6: PRINT "Backspace    Backspace"
   LOCATE 16, 6: PRINT "Tab          Tab:"
   LOCATE 17, 6: PRINT "Enter        Carriage Return"
   LOCATE 18, 6: PRINT "Insert       Toggle Insert"
   LOCATE 19, 6: PRINT "Delete       Delete"
   LOCATE 20, 6: PRINT "Home         Home"
   LOCATE 21, 6: PRINT "End          End"
   LOCATE 22, 6: PRINT "Page Up      Page Up"
   LOCATE 23, 6: PRINT "Page Down    Page Down"
   LOCATE 15, 43: PRINT "alt/i        Toggle AutoIndent"
   LOCATE 16, 43: PRINT "Esc-alt/x    Quit Editor"
   LOCATE 17, 43: PRINT "F1-alt/h     Invoke This Screen"
   LOCATE 18, 43: PRINT "F2-alt/s     Save Document"
   LOCATE 19, 43: PRINT "F3-alt/l     Load Document"
   LOCATE 20, 43: PRINT "Up Arrow     Cursor Up";
   LOCATE 21, 43: PRINT "Down Arrow   Cursor Down"
   LOCATE 22, 43: PRINT "Left Arrow   Cursor Left"
   LOCATE 23, 43: PRINT "Right Arrow  Cursor Right"
   Message "Editor Command Keys  ::  Press Any Key to Exit"
   COLOR CFG.TextFore, CFG.TextBack: Display 1
END SUB

SUB UpdateStats
   LOCATE 25, 1, 0: COLOR CFG.StatFore, CFG.StatBack
   StatLine$ = SPACE$(80): p% = 1: l% = DocDat(Doc).NumberOfLines
   t$ = STR$(DocDat(Doc).PhysicalLine)
   MID$(StatLine$, p%) = t$: p% = p% + LEN(t$)
   t$ = ":" + MID$(STR$(DocDat(Doc).CursorColumn), 2)
   MID$(StatLine$, p%) = t$
   MID$(StatLine$, 10) = " " + DocDat(Doc).DocumentName: p% = 43
   t$ = STR$(l%) + " " + "Line"
   MID$(StatLine$, p%) = t$: p% = p% + LEN(t$)
   IF l% = 1 THEN t$ = " :":   ELSE t$ = "s :"
   MID$(StatLine$, p%) = t$: p% = p% + LEN(t$)
   t$ = STR$(TextLines - TextLinesUsed) + " Free  D" + MID$(STR$(Doc), 2)
   MID$(StatLine$, p%) = t$
   IF DocDat(Doc).Insert <> 0 THEN MID$(StatLine$, 73) = "INS"
   IF DocDat(Doc).Indent <> 0 THEN MID$(StatLine$, 77) = "IND"
   PRINT StatLine$; : COLOR 15, 1
END SUB

SUB UseStr (N AS INTEGER)
   Phree% = INSTR(USED, " ")
   IF Phree% > TextLines THEN Message "Out Of Memory": N = 0: EXIT SUB
   BreakString "", Phree%: TextLinesUsed = TextLinesUsed + 1
   NOL% = DocDat(Doc).NumberOfLines + 1
   IF NOL% = 1 THEN InitializeDocument Doc
   MID$(USED, Phree%, 1) = "*": p% = WalkTheListTo%(N)
   DocDat(Doc).LinkLine = Phree%: DocDat(Doc).NumberOfLines = NOL%
   IF NOL% = 1 THEN
      DocDat(Doc).FirstLine = Phree%: DocDat(Doc).PhysicalLine = 1
      DocDat(Doc).LastLine = Phree%: Index(Phree%).NxtLine = 0
      Index(Phree%).PrvLine = 0: TEXT = STR$(FRE(-1)) + "Bytes Free"
      BreakString TEXT, Phree%: EXIT SUB
   END IF
   PL% = Index(p%).PrvLine
   IF PL% = 0 THEN
      Index(Phree%).PrvLine = 0: DocDat(Doc).FirstLine = Phree%
   ELSE
      Index(Phree%).PrvLine = PL%: Index(PL%).NxtLine = Phree%
   END IF   '_|_|   Link to Previous Line
   Index(Phree%).NxtLine = p%: Index(p%).PrvLine = Phree%
   '_|_|   Link to Next Line
END SUB

FUNCTION WalkTheListTo% (l AS INTEGER)
   r% = DocDat(Doc).FirstLine: t% = 1
   WHILE t% < l: r% = Index(r%).NxtLine: t% = t% + 1: WEND
   WalkTheListTo% = r%: END FUNCTION

SUB WarpToEnd
   DocDat(Doc).PhysicalLine = DocDat(Doc).NumberOfLines - 1
   DocDat(Doc).CursorColumn = 1: PageDown: END SUB

SUB WarpToStart : MoveTop -DocDat(Doc).PhysicalLine + 1: END SUB

SUB WordLeft
   '$DYNAMIC
   DO
      c% = DocDat(Doc).CursorColumn: DIM sp(1 TO 80) AS INTEGER
      alpha$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      alpha$ = alpha$ + LCASE$(alpha$) + "1234567890"
      s% = 1: p$ = " "
      FOR t% = 1 TO 80
         t$ = MID$(TEXT, t%, 1): p% = INSTR(alpha$, t$)
         IF (p% <> 0) AND (p$ = " ") THEN sp(s%) = t%: s% = s% + 1
         p$ = t$: NEXT
      IF (c% > sp(1)) AND (sp(1) > 0) THEN
          DO: s% = s% - 1: LOOP WHILE sp(s%) >= c%
          DocDat(Doc).CursorColumn = sp(s%): EXIT SUB
      ELSE
          IF DocDat(Doc).PhysicalLine = 1 THEN
             SOUND 999, 1: EXIT SUB
          ELSE CursorUp: ERASE sp
             DocDat(Doc).CursorColumn = LEN(RTRIM$(TEXT)) + 1
          END IF
      END IF
   LOOP
END SUB

REM $STATIC
SUB WordRight
   '$DYNAMIC
   DO
      c% = DocDat(Doc).CursorColumn: DIM sp(81) AS INTEGER
      alpha$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      alpha$ = alpha$ + LCASE$(alpha$) + "1234567890"
      s% = 0: p$ = " ": sp(0) = 81: sc% = 0
      FOR t% = 1 TO 80
         t$ = MID$(TEXT, t%, 1): p% = INSTR(alpha$, t$)
         IF (p% <> 0) AND (p$ = " ") THEN s% = s% + 1: sp(s%) = t%
         p$ = t$: NEXT
      IF (c% < sp(s%)) AND (s% > 0) THEN
          DO: sc% = sc% + 1: LOOP WHILE sp(sc%) <= c%
          DocDat(Doc).CursorColumn = sp(sc%): EXIT SUB
      ELSE
          IF DocDat(Doc).PhysicalLine = DocDat(Doc).NumberOfLines THEN
             SOUND 999, 1: EXIT SUB
          ELSE CursorDown: ERASE sp: DocDat(Doc).CursorColumn = 0
          END IF
      END IF
   LOOP
END SUB

REM $STATIC
SUB Wrap (TXT$, Overflow$)
   M% = DocDat(Doc).Margin: B% = M% + 1: E% = M% - M% * .25: s% = 0
   DO: B% = B% - 1: s% = INSTR("/\,- ", MID$(TXT$, B%, 1))
   LOOP WHILE (B% >= E%) AND (s% = 0): IF s% = 0 THEN B% = M%
   Overflow$ = RTRIM$(MID$(TXT$, B% + 1)): TXT$ = LEFT$(TXT$, B%)
END SUB

