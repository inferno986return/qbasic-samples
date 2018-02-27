''''' -=*=--=*=--=*=-  begin CANSI.BAS  -=*=--=*=--=*=-
DECLARE SUB ansi (a$)
ON ERROR GOTO botched
DEF SEG = &HB800: DIM SHARED SCR%(2): SCR%(1) = 80: SCR%(2) = 25
WIDTH 80, 25: F$ = COMMAND$: COLOR 7, 0: CLS
IF F$ = "" THEN INPUT "File to display"; F$
OPEN F$ FOR INPUT AS #1
WHILE NOT EOF(1): ansi (INPUT$(1, #1)): WEND: CLOSE #1
fini: COLOR 2, 0: FOR S% = 5 TO 35: SOUND S% * 200, .1: NEXT
   R$ = "": WHILE R$ = "": R$ = INKEY$: WEND: END
botched: COLOR 2, 0
   PRINT "file "; CHR$(34); F$; CHR$(34); " not found"
   PRINT "error"; ERR: RESUME fini
''''' -=*=--=*=--=*=-  end CANSI.BAS  -=*=--=*=--=*=-

SUB ansi (a$)
DEFINT A-Z: STATIC H, W, R, E, L, C, F, B, O, V, E$
IF W < 40 THEN W = SCR%(1): H = SCR%(2) - 1: R = W - 1: C = 0: F = 7: B = 0
IF E <> 27 THEN
   IF ASC(a$) <> 27 THEN GOSUB CHRout:  ELSE E = 27: E$ = a$
   EXIT SUB
END IF
IF O <> 27 AND ASC(a$) = 34 THEN O = E: EXIT SUB
IF O = 27 THEN
   IF ASC(a$) = 34 THEN O = 0
   EXIT SUB
END IF: E$ = E$ + a$
IF LEN(E$) = 2 AND a$ <> "[" THEN E = 0: E$ = "": EXIT SUB
S = INSTR("HfABCDsuJKmhlp", a$)
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
  CASE 9: CLS : C = 0
  CASE 10: L = C: WHILE L MOD W <> 0: POKE L * 2, 32: L = L + 1: WEND
  CASE 11: GOSUB Colorz
END SELECT: E% = 0: E$ = "": EXIT SUB
CursorA: L = VAL(MID$(E$, INSTR(E$, "[") + 1)) - 1
   C = VAL(MID$(E$, INSTR(E$, ";") + 1)) - 1
   IF C < 0 THEN C = 0:  ELSE IF C > R THEN C = R
   IF L < 1 THEN L = 0:  ELSE IF L > H THEN L = H
   C = L * W + C: RETURN
CursorL: P = VAL(MID$(E$, INSTR(E$, "[") + 1)): IF P < 1 THEN P = 1
   L = INT(C / W) + P * L
   IF L < 0 THEN L = 0:  ELSE IF L > H THEN L = H
   C = (C MOD W) + L * W: RETURN
CursorC: P = VAL(MID$(E$, INSTR(E$, "[") + 1)): IF P < 1 THEN P = 1
   L = (C MOD W) + P * L: C = INT(C / W) * W
   IF L < 1 THEN L = 0:  ELSE IF L > R THEN L = R
   C = C + L: RETURN
Colorz: E$ = MID$(E$, INSTR(E$, "[") + 1)
  DO: E = VAL(E$)
  SELECT CASE E
     CASE 0: F = 7: B = 0
     CASE 1: F = (F AND 7) OR 8
     CASE 5: B = (B AND 7) OR 8
     CASE 8: F = B
     CASE 30 TO 37: P = E - 29: E = ASC(MID$("@DBFAECG", P)) AND 7
                    F = (F AND 248) OR E
     CASE 40 TO 47: P = E% - 39: E = ASC(MID$("@DBFAECG", P)) AND 7
                    B = (B AND 248) OR E
  END SELECT: P = INSTR(E$, ";"): E$ = MID$(E$, P + 1): LOOP WHILE P > 0
COLOR F, B: RETURN
CHRout: P = ASC(a$)
   IF P = 7 THEN BEEP: RETURN
   IF P = 13 THEN C = C - C MOD W: RETURN
   IF P = 10 THEN C = C + W
   IF P <> 10 THEN POKE C * 2, P: POKE C * 2 + 1, F + 16 * B: C = C + 1
   IF C >= W * (H + 1) THEN
      C = C - W: LOCATE H + 1, W: PRINT
      P = W * 2: L = (H - 1) * P
      FOR L = L TO L + P: POKE L, PEEK(L + P): POKE L + P, B: NEXT
   END IF
   RETURN
END SUB

