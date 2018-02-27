DECLARE SUB SetParms ()
DECLARE SUB ScreenPocket (IO$)
DECLARE SUB SetParm (P$)
DECLARE FUNCTION Menu% ()
DECLARE SUB Warning ()
DECLARE SUB SaveParms ()
DECLARE SUB ReadParms ()
ON ERROR GOTO OOps
TYPE ProgramParameters
   fore AS INTEGER: back AS INTEGER: warn AS INTEGER: END TYPE
DIM SHARED Scr(3999) AS STRING * 1, parm AS ProgramParameters
parm.fore = 2: parm.back = 0: parm.warn = 100: ReadParms
DO: LOOP WHILE Menu%: SYSTEM
OOps: FErr$ = STR$(ERR): RESUME NEXT
FUNCTION Menu%
   COLOR parm.fore, parm.back: CLS : LOCATE 10
   PRINT , "[ R ]> Reset Parameters": PRINT
   PRINT , "[ S ]> Sound a warning": PRINT
   PRINT , "[ Q ]> Quit program"
   DO: DO: K$ = UCASE$(INKEY$): LOOP WHILE K$ = ""
      SELECT CASE K$
         CASE "R": SetParms: Menu% = 1: EXIT DO
         CASE "S": Warning
         CASE "Q": Menu% = 0: EXIT DO
      END SELECT: LOOP: END FUNCTION
SUB ReadParms
   SHARED FErr$: FErr$ = "ok": config% = FREEFILE
   OPEN "parms.cfg" FOR INPUT AS #config%
   IF FErr$ = "ok" THEN
      WHILE NOT EOF(config%)
         LINE INPUT #config%, P$: SetParm P$
      WEND: CLOSE config%
   END IF: END SUB
SUB SaveParms
   SHARED FErr$: ScreenPocket "in": CLS : LOCATE 10, 20
   PRINT "Save program parameters ( Y/N )? ";
   DO: DO: K$ = UCASE$(INKEY$): LOOP WHILE K$ = ""
   LOOP WHILE INSTR("YN", K$) < 1
   IF K$ = "N" THEN ScreenPocket "out": EXIT SUB
   config% = FREEFILE
   FErr$ = "ok": OPEN "parms.cfg" FOR OUTPUT AS #config%
   IF FErr$ <> "ok" THEN PRINT "Drive error": SYSTEM
   PRINT "Saving..."
   PRINT #config%, "Foreground="; MID$(STR$(parm.fore), 2)
   PRINT #config%, "Background="; MID$(STR$(parm.back), 2)
   PRINT #config%, "Warning="; MID$(STR$(parm.warn), 2)
   CLOSE config%: PRINT : PRINT "hit a key"
   WHILE INKEY$ = "": WEND: ScreenPocket "out": END SUB
SUB ScreenPocket (IO$)
   DEF SEG = &HB800
      SELECT CASE IO$
         CASE "in"
            FOR t% = 0 TO 3999: Scr(t%) = CHR$(PEEK(t%)): NEXT
         CASE "out"
            FOR t% = 0 TO 3999: POKE t%, ASC(Scr(t%)): NEXT
      END SELECT: DEF SEG : END SUB
SUB SetParm (P$)
   eq% = INSTR(P$, "="): IF eq% = 0 THEN EXIT SUB
   P$ = UCASE$(P$): C$ = RTRIM$(LTRIM$(LEFT$(P$, eq% - 1)))
   V$ = MID$(P$, eq% + 1)
   SELECT CASE C$
      CASE "FOREGROUND": parm.fore = VAL(V$): COLOR parm.fore
      CASE "BACKGROUND": parm.back = VAL(V$): COLOR , parm.fore
      CASE "WARNING": parm.warn = VAL(V$): Warning
   END SELECT: END SUB
SUB SetParms
   DIM Pp AS ProgramParameters: Pp = parm: COLOR 14, 0: CLS
   LOCATE 10, 2: PRINT "Select a background color => ";
   FOR t% = 0 TO 7: COLOR 15, t%: PRINT "#"; : NEXT: C% = 31
   DO: LOCATE 10, C%, 1
      DO: K$ = UCASE$(INKEY$): LOOP WHILE K$ = ""
      SELECT CASE K$
         CASE CHR$(27): EXIT SUB
         CASE CHR$(13): Pp.back = C% - 31: EXIT DO
         CASE CHR$(0) + "K": C% = C% + (C% > 31)
         CASE CHR$(0) + "M": C% = C% - (C% < 38)
      END SELECT: LOOP: COLOR , Pp.back
   CLS : LOCATE 10, 2: PRINT "Select a foreground color => ";
   FOR t% = 0 TO 15: COLOR t%, 0: PRINT "#"; : NEXT: C% = 31
   DO: LOCATE 10, C%, 1: DO: K$ = UCASE$(INKEY$): LOOP WHILE K$ = ""
      SELECT CASE K$
         CASE CHR$(27): EXIT SUB
         CASE CHR$(13): Pp.fore = C% - 31: EXIT DO
         CASE CHR$(0) + "K": C% = C% + (C% > 31)
         CASE CHR$(0) + "M": C% = C% - (C% < 46)
      END SELECT: LOOP: COLOR Pp.fore, Pp.back: CLS
   LOCATE 10, 2: PRINT "Enter a value to use as a warning tone.";
   INPUT "( 50-4000 ) =>", t%
   IF (t% < 50) OR (t% > 4000) THEN EXIT SUB
   Pp.warn = t%: parm = Pp: Warning: SaveParms: END SUB
SUB Warning : FOR t% = 1 TO 10: SOUND parm.warn, .07: NEXT: END SUB
