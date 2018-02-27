'_|_|_|   QUIKTERM.BAS   (part 1 of 3)
'_|_|_|   This program will operate in ANSI emulation with ANSI
'_|_|_|   auto-detect. Must be compiled for high speeds.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (6/5/96)
DECLARE SUB QuikCFG (d() AS STRING)
DECLARE SUB ansi (A$)
ON ERROR GOTO BooBoo
DIM FKEYS(13) AS STRING: port% = 0
FError$ = "ok": OPEN "quikterm.cfg" FOR INPUT AS #2
IF FError$ = "ok" THEN
   FOR t% = 0 TO 13
      IF EOF(2) THEN EXIT FOR
      LINE INPUT #2, FKEYS(t%)
   NEXT: port% = VAL(FKEYS(0))
END IF: CLOSE 2
Ex$ = CHR$(27) + "[": CrLf$ = CHR$(13) + CHR$(10)
CLS : altx$ = Ex$ + "0;1;32mALT/X to exit terminal" + CrLf$
IF port% = 0 THEN
   LOCATE 1, 1, 1: PRINT "Choose a port (1/2)"
   DO: port% = INSTR(" 12", INKEY$): LOOP WHILE port% < 2
   port% = port% - 1: FError$ = "ok"
   OPEN "quikterm.cfg" FOR OUTPUT AS #2
   IF FError$ = "ok" THEN PRINT #2, MID$(STR$(port%), 2)
END IF: CLOSE 2
FOR t% = 1 TO 34: ansi MID$(altx$, t%, 1): NEXT
port$ = "COM" + MID$(STR$(port%), 2) + ":19200,N,8,1"
FError$ = "ok": OPEN port$ FOR RANDOM AS #1 LEN = 8192
IF FError$ <> "ok" THEN PRINT "MODEM ERROR"; FError$: END
DO
   Modemin$ = "": IF NOT EOF(1) THEN Modemin$ = INPUT$(1, #1)
   ansi Modemin$
   IF ANSIDetect$ <> "" THEN PRINT #1, ANSIDetect$: ANSIDetect$ = ""
   kb$ = INKEY$
   IF kb$ <> "" THEN
      k% = ASC(kb$)
      IF k% = 0 THEN
         k% = ASC(MID$(kb$, 2))
         SELECT CASE k%
            CASE 45: CLOSE #1: END
            CASE 59 TO 68: k% = k% - 58
               kb$ = FKEYS(k%)
               DO: e% = INSTR(UCASE$(kb$), "^M")
                  IF e% > 0 THEN MID$(kb$, e%) = MID$(kb$, e% + 1)
                  IF e% > 0 THEN kb$ = LEFT$(kb$, LEN(kb$) - 1)
                  IF e% > 0 THEN MID$(kb$, e%, 1) = CHR$(13)
               LOOP WHILE e% > 0
               IF FKEYS(k%) = "" THEN k% = 0
            CASE 133, 134: k% = k% = 122
               kb$ = FKEYS(k%)
               IF FKEYS(k%) = "" THEN k% = 0
            CASE 37: QuikCFG FKEYS(): k% = 0
            CASE ELSE: k% = 0
         END SELECT
      END IF: IF k% > 0 THEN PRINT #1, kb$;
   END IF
LOOP: CLOSE #1: END
BooBoo:
   FError$ = STR$(ERR): RESUME NEXT
'_|_|_|   end   QUIKTERM.BAS   (part 1 of 3)
 
'_|_|_|   QUIKTERM.BAS   (part 2 of 3)
'_|_|_|   This program will operate in ANSI emulation with ANSI
'_|_|_|   auto-detect. Must be compiled for high speeds.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (6/5/96)
SUB ansi (A$)
DEFINT A-Z: DEF SEG = &HB800
STATIC W, e, L, C, O, M, F, B, V, e$: SHARED ANSIDetect$
IF W < 99 THEN W = 100: C = 0: F = 7: B = 0: A = 0: M = F + 16 * B
IF A$ = "" THEN LOCATE C \ 80 + 1, C MOD 80 + 1, 1: EXIT SUB
IF e <> 27 THEN
   IF ASC(A$) <> 27 THEN GOSUB CHRout:  ELSE e = 27: e$ = A$
   EXIT SUB
END IF
IF O <> 27 AND ASC(A$) = 34 THEN O = e: EXIT SUB
IF O = 27 THEN
   IF ASC(A$) = 34 THEN O = 0
   EXIT SUB
END IF: e$ = e$ + A$
IF LEN(e$) = 2 AND A$ <> "[" THEN e = 0: e$ = "": EXIT SUB
S = INSTR("HfABCDsuJKmhlpn", A$)
SELECT CASE S
  CASE 0: EXIT SUB
  CASE 1: GOSUB CursorA
  CASE 2: GOSUB CursorA
  CASE 3: L = -1: GOSUB CursorL
  CASE 4: L = 1: GOSUB CursorL
  CASE 5: L = 1: GOSUB CursorC
  CASE 6: L = -1: GOSUB CursorC
  CASE 7: V = C
  CASE 8: C = V
  CASE 9: COLOR F, B: CLS : C = 0
  CASE 10: FOR L = C TO C + 79 - (C MOD 80)
           POKE L * 2, 32: POKE L * 2 + 1, M: NEXT
  CASE 11: GOSUB Colorz
  CASE 15: A$ = CHR$(27) + "[" + MID$(STR$(C \ 80 + 1), 2) + ";"
           ANSIDetect$ = A$ + MID$(STR$((C MOD 80) + 1), 2) + "R"
END SELECT: e = 0: e$ = "": EXIT SUB
CursorA: L = VAL(MID$(e$, INSTR(e$, "[") + 1))
   C = VAL(MID$(e$, INSTR(e$, ";") + 1))
   IF C > 0 THEN C = (C - 1): IF C > 79 THEN C = 79
   IF L > 0 THEN L = (L - 1): IF L > 24 THEN L = 24
   C = L * 80 + C: RETURN
CursorL: p = VAL(MID$(e$, INSTR(e$, "[") + 1))
   p = p - (p < 1): L = INT(C \ 80) + p * L
   IF L < 0 THEN L = 0:  ELSE IF L > 24 THEN L = 24
   C = (C MOD 80) + L * 80: RETURN
CursorC: p = VAL(MID$(e$, INSTR(e$, "[") + 1))
   p = p - (p < 1): L = (C MOD 80) + p * L: C = INT(C \ 80) * 80
   IF L < 1 THEN L = 0:  ELSE IF L > 79 THEN L = 79
   C = C + L: RETURN
Colorz: e$ = MID$(e$, INSTR(e$, "[") + 1)
   DO: e = VAL(e$)
      SELECT CASE e
         CASE 0: F = 15: B = 0
         CASE 1: F = F OR 8
         CASE 5: B = B OR 8
         CASE 8: F = B
         CASE 30 TO 37: p = e - 29
            e = ASC(MID$("@DBFAECG", p)) AND 7: F = (F AND 248) + e
         CASE 40 TO 47: p = e - 39
            e = ASC(MID$("@DBFAECG", p)) AND 7: B = (B AND 248) + e
      END SELECT: p = INSTR(e$, ";"): e$ = MID$(e$, p + 1)
   LOOP WHILE p > 0: M = F + 16 * B: RETURN
CHRout: p = ASC(A$)
   IF p = 7 THEN FOR t% = 800 TO 1111 STEP 20: SOUND t%, .1: NEXT: RETURN
   IF p = 8 THEN
      IF (C MOD 80) > 0 THEN
         FOR t% = C * 2 TO (C \ 80) * 160 + 159
            POKE t% - 2, PEEK(t%)
         NEXT: C = C - 1
      END IF: RETURN
   END IF
   IF p = 13 THEN C = C - (C MOD 80): RETURN
   IF p = 10 THEN C = C + 80
   IF p <> 10 THEN POKE C * 2, p: POKE C * 2 + 1, M: C = C + 1
   IF C >= 2000 THEN
      C = C - 80: LOCATE 30, 80: PRINT
      DIM PK%(2): PK%(0) = 32: PK%(1) = M
      FOR L = 3680 TO 3839
         POKE L, PEEK(L + 160): POKE L + 160, PK%(L AND 1)
      NEXT
   END IF: RETURN
END SUB
'_|_|_|   end   QUIKTERM.BAS   (part 2 of 3)
 
'_|_|_|   QUIKTERM.BAS   (part 3 of 3)
'_|_|_|   This program will operate in ANSI emulation with ANSI
'_|_|_|   auto-detect. Must be compiled for high speeds.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (6/5/96)
SUB QuikCFG (d() AS STRING)
   SHARED port%
   DIM buf(4000) AS STRING * 1: DEF SEG = &HB800: F$ = SPACE$(80)
   FOR t% = 0 TO 3999: buf(t%) = CHR$(PEEK(t%)): NEXT
   csr% = LEN(d(0)) + 1: macro% = 0: COLOR 10, 0: CLS : COLOR 14, 4
   PRINT " COM "; : COLOR 10, 0: PRINT LEFT$(d(0) + F$, 75);
   FOR t% = 1 TO 12
      COLOR 14, 4: LOCATE t% + 1, 1: PRINT " F"; RIGHT$(STR$(t%), 2); " ";
      COLOR 10, 0: PRINT LEFT$(d(t%) + F$, 75); : NEXT
   PRINT : PRINT : PRINT "RETURN exits: ALT/S saves"
   DO: LOCATE macro% + 1, 6: COLOR 15, 1: PRINT LEFT$(d(macro%) + F$, 75);
      LOCATE , csr% + 5: DO: k$ = INKEY$: LOOP WHILE k$ = ""
      k% = ASC(k$): IF k% = 0 THEN k% = -ASC(MID$(k$, 2))
      SELECT CASE k%
         CASE 8
            IF csr% > 1 THEN
               csr% = csr% - 1
               MID$(d(macro%), csr%) = MID$(d(macro%), csr% + 1)
               d(macro%) = LEFT$(d(macro%), LEN(d(macro%)) - 1)
            ELSE SOUND 999, .7
            END IF
         CASE 13: FOR t% = 0 TO 3999: POKE t%, ASC(buf(t%)): NEXT: EXIT SUB
         CASE 32 TO 255
            L$ = LEFT$(d(macro%), csr% - 1): r$ = MID$(d(macro%), csr%)
            d(macro%) = LEFT$(L$ + k$ + r$, 70)
            IF csr% < 75 THEN csr% = csr% + 1:  ELSE SOUND 999, .7
         CASE -31
            FError$ = "ok": OPEN "quikterm.cfg" FOR OUTPUT AS #2
            IF FError$ = "ok" THEN
               FOR t% = 0 TO 13: PRINT #2, d(t%): NEXT: port% = VAL(d(0))
            END IF: CLOSE 2
         CASE -71: csr% = 1
         CASE -72
            IF macro% > 0 THEN
               LOCATE macro% + 1, 6: COLOR 10, 0
               PRINT LEFT$(d(macro%) + F$, 75);
               macro% = macro% - 1: csr% = LEN(d(macro%)) + 1
            ELSE SOUND 999, .7
            END IF
         CASE -75
            IF csr% > 1 THEN csr% = csr% - 1:  ELSE SOUND 999, .7
         CASE -77
            IF csr% < 70 THEN csr% = csr% + 1:  ELSE SOUND 999, .7
         CASE -79: csr% = LEN(d(macro%)) + 1
         CASE -80
            IF macro% < 12 THEN
               LOCATE macro% + 1, 6: COLOR 10, 0
               PRINT LEFT$(d(macro%) + F$, 75);
               macro% = macro% + 1: csr% = LEN(d(macro%)) + 1
            ELSE SOUND 999, .7
            END IF
         CASE -83
            IF LEN(d(macro%)) >= csr% THEN
               MID$(d(macro%), csr%) = MID$(d(macro%), csr% + 1)
               d(macro%) = LEFT$(d(macro%), LEN(d(macro%)) - 1)
            ELSE SOUND 999, .7
            END IF
      END SELECT
   LOOP
END SUB
'_|_|_|   end   QUIKTERM.BAS   (part 3 of 3)
 
