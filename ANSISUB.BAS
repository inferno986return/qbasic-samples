'_|_|_|   ANSISUB.BAS
'_|_|_|   This program interprets ANSI ESC codes without ANSI.SYS.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (1/22/96)
ON ERROR GOTO Done: i$ = STRING$(16, " ") + STRING$(15, CHR$(29))
COLOR 2, 0: CLS : F$ = COMMAND$: PRINT F$
IF F$ = "" THEN LOCATE 5, 10: INPUT "File to display => ", F$
IF F$ = "" THEN GOTO Done:  ELSE COLOR 2, 0: OPEN F$ FOR BINARY AS #1
WHILE NOT EOF(1): F$ = INPUT$(1024, #1): L% = LEN(F$)
   FOR t% = 1 TO L%: A$ = (MID$(F$, t%, 1)): ansi A$: NEXT
'_|_|_|   IF ANSIDetect$ <> "" THEN SendToModem (ANSIDetect$)
'_|_|_|   ANSIDetect$ = ""
WEND: ansi ""   '_|_|_|   POSITION CURSOR
Done: CLOSE 1: COLOR 2, 0: FOR S% = 5 TO 35: SOUND S% * 200, .1: NEXT
   R$ = "": WHILE R$ = "": R$ = INKEY$: WEND: END
SUB ansi (A$) : DEFINT A-Z: DEF SEG = &HB800
   STATIC W, e, L, C, O, M, F, B, V, e$: SHARED ANSIDetect$
   IF W < 99 THEN W = 100: C = 0: F = 7: B = 0: A = 0: M = F + 16 * B
   IF A$ = "" THEN LOCATE C \ 80 + 1, C MOD 80 + 1, 1: EXIT SUB
   IF e <> 27 THEN
      IF ASC(A$) <> 27 THEN GOSUB CHRout:  ELSE e = 27: e$ = A$
      EXIT SUB
   END IF: IF O <> 27 AND ASC(A$) = 34 THEN O = e: EXIT SUB
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
         CASE 0: F = 7: B = 0
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
   END IF: RETURN: END SUB
'_|_|_|   end   ANSISUB.BAS
 
