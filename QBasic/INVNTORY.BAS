'_|_|_|   INVNTORY.BAS   PUBLIC DOMAIN   by Kurt Kuzba.  (9/4/1997) 1/2
TYPE InventoryControlDat
   PartName AS STRING * 60: PartNumb AS STRING * 20
   PartDesc AS STRING * 160: PartsPrc AS LONG: PartsCst AS LONG
   PartsAvl AS LONG: PartsOrd AS LONG: END TYPE
DECLARE SUB EditInventory (i AS InventoryControlDat)
DECLARE SUB ShowInventory (i AS InventoryControlDat)
DECLARE SUB ShowItem (x%, s$, H%)
DECLARE SUB ShowLine (x%, hilite%, i AS InventoryControlDat)
DECLARE SUB DoEdit (item%, i AS InventoryControlDat)
DECLARE SUB ShowTime ()
DECLARE SUB EditName (PartName$)
DECLARE SUB EditNumb (PartNumb$)
DECLARE SUB EditDesc (PartDesc$)
DECLARE SUB EditAvl (Avl&)
DECLARE SUB EditOrd (Ord&)
DECLARE SUB EditPrc (Prc&)
DECLARE SUB EditCst (Cst&)
DECLARE FUNCTION Currency$ (V&)
DECLARE FUNCTION Caption$ (C%)
DECLARE FUNCTION GetDate$ ()
DECLARE FUNCTION GetString$ (d$, x%, y%, l%, k%)
ON ERROR GOTO BooBoo
DIM Inv AS InventoryControlDat
Inv.PartName = "": Inv.PartNumb = "": Inv.PartDesc = ""
Inv.PartsAvl = 0: Inv.PartsOrd = 0
Inv.PartsPrc = 0: Inv.PartsCst = 0
EditInventory Inv: SYSTEM
BooBoo: FError$ = STR$(ERR): RESUME NEXT
FUNCTION Caption$ (C%)
   SELECT CASE C%
      CASE 0: s$ = " " + CHR$(213) + CHR$(209) + STRING$(62, 205)
         Caption$ = s$ + CHR$(209) + CHR$(184) + " "
      CASE 1: Caption$ = "  <==  Part I.D. Number"
      CASE 2: Caption$ = "==>       "
      CASE 3: Caption$ = "Parts On Hand  :"
      CASE 4: Caption$ = "Parts On Order :"
      CASE 5: Caption$ = "Price Per Part : $"
      CASE 6: Caption$ = "Cost Per Part  : $"
      CASE 7: s$ = " " + CHR$(212) + CHR$(207) + STRING$(62, 205)
         Caption$ = s$ + CHR$(207) + CHR$(190) + " "
      CASE 8: Caption$ = "========  Part Description or Comments"
   END SELECT: END FUNCTION
FUNCTION Currency$ (V&) : d$ = MID$(STR$(V& \ 100), 2) + "."
   C$ = RIGHT$("00" + MID$(STR$(V& MOD 100), 2), 2)
   Currency$ = d$ + C$: END FUNCTION
SUB DoEdit (item%, i AS InventoryControlDat)
   SELECT CASE item%
      CASE 0: EditName i.PartName
      CASE 1: EditNumb i.PartNumb
      CASE 2: EditDesc i.PartDesc
      CASE 3: EditAvl i.PartsAvl
      CASE 4: EditOrd i.PartsOrd
      CASE 5: EditPrc i.PartsPrc
      CASE 6: EditCst i.PartsCst
   END SELECT: COLOR 10, 2: LOCATE 2, 1, 0
   PRINT STRING$(80, 177); : END SUB
SUB EditAvl (Avl&) : LOCATE 2, 7: COLOR 10, 0
   PRINT " Enter number of parts to add. "
   NumberAvl$ = GetString$("     ", 14, 41, 6, 0)
   Avl& = Avl& + VAL(NumberAvl$): END SUB
SUB EditCst (Cst&) : LOCATE 2, 7: COLOR 10, 0
   PRINT " Enter new cost per piece. "
   Cst$ = GetString$("", 17, 41, 10, 0)
   IF VAL(Cst$) > 0 THEN Cst& = VAL(Cst$) * 100
END SUB
SUB EditDesc (PartDesc$) : LOCATE 2, 7: COLOR 10, 0: cLine% = 0
   PRINT " Enter Description or comments. ": DIM Desc$(3)
   Desc$(0) = MID$(PartDesc$, 1, 40)
   Desc$(1) = MID$(PartDesc$, 41, 40)
   Desc$(2) = MID$(PartDesc$, 81, 40)
   Desc$(3) = MID$(PartDesc$, 121, 40)
   DO: x% = cLine% + 10
      Desc$(cLine%) = GetString$(Desc$(cLine%), x%, 21, 40, k%)
      LOCATE x%, 21: COLOR 15, 1
      PRINT LEFT$(Desc$(cLine%) + SPACE$(40), 40);
      IF k% = -72 THEN cLine% = (cLine% + 3) MOD 4
      IF (k% = 13) OR (k% = -80) THEN cLine% = (cLine% + 1) MOD 4
   LOOP WHILE k% <> 27
   MID$(PartDesc$, 1, 40) = Desc$(0)
   MID$(PartDesc$, 41, 40) = Desc$(1)
   MID$(PartDesc$, 81, 40) = Desc$(2)
   MID$(PartDesc$, 121, 40) = Desc$(3): END SUB
SUB EditName (PartName$) : LOCATE 2, 7: COLOR 10, 0
   PRINT " Enter full part name. "
   PartName$ = GetString$(PartName$, 7, 11, 60, 0): END SUB
SUB EditNumb (PartNumb$) : LOCATE 2, 7: COLOR 10, 0
   PRINT " Enter part number. "
   PartNumb$ = GetString$(PartNumb$, 8, 11, 20, 0): END SUB
'_|_|_|   INVNTORY.BAS   PUBLIC DOMAIN   by Kurt Kuzba.  (9/4/1997)  2/2
SUB EditInventory (i AS InventoryControlDat)
   ShowInventory i: item% = 1: k% = -72: Finished% = 0
   DO
      SELECT CASE k%
         CASE -72, ASC("8"), ASC("1"): ShowLine item%, 0, i
            item% = (item% + 6) MOD 7: ShowLine item%, 1, i
         CASE -80, ASC("2"), ASC("3"): ShowLine item%, 0, i
            item% = (item% + 1) MOD 7: ShowLine item%, 1, i
         CASE 13: ShowLine item%, 0, i
            DoEdit item%, i: ShowLine item%, 1, i
         CASE 27, -45, ASC("+"): Finished% = -1
      END SELECT: k% = 0: k$ = INKEY$
      IF k$ > "" THEN
         k% = ASC(k$): IF k% = 0 THEN k% = -ASC(MID$(k$, 2))
      END IF: ShowTime: LOOP WHILE NOT (Finished%): END SUB
SUB EditOrd (Ord&) : LOCATE 2, 7: COLOR 10, 0
   PRINT " Enter number of parts to add. "
   NumberOrd$ = GetString$("     ", 15, 41, 6, 0)
   Ord& = Ord& + VAL(NumberOrd$): END SUB
SUB EditPrc (Prc&) : LOCATE 2, 7: COLOR 10, 0
   PRINT " Enter new price per piece. "
   Prc$ = GetString$("", 16, 41, 10, 0)
   IF VAL(Prc$) > 0 THEN Prc& = VAL(Prc$) * 100
END SUB
FUNCTION GetDate$
   d$ = DATE$: m% = VAL(d$): d$ = MID$(d$, INSTR(d$, "-"))
   MID$(d$, 1) = " ": mon$ = "  JanFebMarAprMayJunJulAugSepOctNovDec"
   GetDate$ = MID$(mon$, m% * 3, 3) + d$: END FUNCTION
FUNCTION GetString$ (d$, x%, y%, l%, k%)
   s$ = LEFT$(d$ + SPACE$(l%), l%): done% = 0: ins% = 1: GOSUB insert
   C% = LEN(RTRIM$(s$)) - 1
   IF C% < l% THEN C% = C% + 1
   DO: LOCATE x%, y%, 0: COLOR 0, 3: PRINT s$;
      DO: k% = 0: ShowTime: LOCATE x%, y% + C%: COLOR 4 * ins%, 7
         PRINT MID$(s$, C% + 1, 1); : k$ = INKEY$
         IF k$ > "" THEN
            k% = ASC(k$): IF k% = 0 THEN k% = -ASC(MID$(k$, 2))
         END IF: LOOP WHILE k% = 0
      SELECT CASE k%
         CASE 32 TO 126
            IF ins% > 0 THEN
               s$ = LEFT$(LEFT$(s$, C% + 1) + MID$(s$, C% + 1), l%)
            END IF
            MID$(s$, C% + 1) = CHR$(k%): C% = C% - (C% < l%)
         CASE 8
            IF C% > 0 THEN
               C% = C% - 1: s$ = LEFT$(s$, C%) + MID$(s$, C% + 2) + " "
            END IF
         CASE 13, -72, -80: GetString$ = s$: done% = -1
         CASE 27: GetString$ = d$: done% = -1
         CASE -82: ins% = ABS(ins% - 1): SOUND 999, .5: GOSUB insert
         CASE -75: C% = (C% + l% - 1) MOD l%
         CASE -77: C% = (C% + 1) MOD l%
         CASE -71: C% = 0
         CASE -79: C% = LEN(RTRIM$(s$)) - 1: IF C% < l% THEN C% = C% + 1
         CASE -83: s$ = LEFT$(s$, C%) + MID$(s$, C% + 2) + " "
      END SELECT: LOOP WHILE NOT (done%): ins% = 0: GOSUB insert
EXIT FUNCTION
insert: : LOCATE 6, 38: COLOR 9, 1
   IF ins% > 0 THEN PRINT " INS "; :  ELSE PRINT STRING$(5, 205);
   RETURN
END FUNCTION
SUB ShowInventory (i AS InventoryControlDat) : COLOR 10, 2: CLS
   FOR t% = 1 TO 25: LOCATE t%, 1, 0: PRINT STRING$(80, 177); : NEXT
   LOCATE 6, 7, 0: COLOR 9, 1: PRINT Caption$(0);
   FOR t% = 1 TO 7: ShowLine t%, 0, i: NEXT
   COLOR 9, 1: LOCATE 18, 7: PRINT Caption$(7): END SUB
SUB ShowItem (x%, s$, H%) : border$ = " " + STRING$(2, 179) + " "
   LOCATE x%, 7, 0: COLOR 9, 1: PRINT border$;
   IF H% > 0 THEN COLOR 4, 7:  ELSE COLOR 15, 1
   PRINT LEFT$(s$ + SPACE$(60), 60); : COLOR 9, 1
   PRINT border$: END SUB
SUB ShowLine (x%, hi%, i AS InventoryControlDat)
   SELECT CASE x%
      CASE 0: ShowItem 7, i.PartName, hi%
      CASE 1: ShowItem 8, i.PartNumb + Caption$(1), hi%
      CASE 2: ShowItem 9, Caption$(8), hi%
         ShowItem 10, Caption$(2) + MID$(i.PartDesc, 1, 40), hi%
         ShowItem 11, Caption$(2) + MID$(i.PartDesc, 41, 40), hi%
         ShowItem 12, Caption$(2) + MID$(i.PartDesc, 81, 40), hi%
         ShowItem 13, Caption$(2) + MID$(i.PartDesc, 121, 40), hi%
      CASE 3: ShowItem 14, Caption$(3) + STR$(i.PartsAvl), hi%
      CASE 4: ShowItem 15, Caption$(4) + STR$(i.PartsOrd), hi%
      CASE 5: ShowItem 16, Caption$(5) + Currency$(i.PartsPrc), hi%
      CASE 6: ShowItem 17, Caption$(6) + Currency$(i.PartsCst), hi%
   END SELECT: END SUB
SUB ShowTime : LOCATE 4, 7, 0: COLOR 10, 0: tm$ = TIME$
   t% = VAL(tm$): tm$ = MID$(tm$, INSTR(tm$, ":")): am$ = "am"
   IF t% = 0 THEN t% = 12 ELSE IF t% > 12 THEN t% = t% - 12: am$ = "pm"
   tm$ = RIGHT$(STR$(t%), 2) + tm$
   PRINT "  " + GetDate$ + "  " + tm$ + " " + am$ + "  ": END SUB
