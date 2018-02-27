DECLARE FUNCTION StrWin$ (X%, Y%, Z%, Default$)
DEFINT A-Z
p = POS(0): c = CSRLIN
MyStr$ = StrWin$(11, 27, 21, "C:\STUFF\STRING.BAS")
COLOR 2, 0:  LOCATE c, p:  PRINT MyStr$

FUNCTION StrWin$ (X, Y, Z, Default$)
 '|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|
 '| This function has 4 parameters, MyStr$ = StrWin$(9, 28, 19, Default$) |
 '| Parameters passed are Line, Column, and String Length.  Line is the   |
 '| upper edge of the window border, 1 to 23.  String Length is the       |
 '| maximum length of the string to be returned by the function.          |
 '| Column is the left border of the window, 1 to (77 - String Length)    |
 '| Default$ is the optional default string, or "" if no default needed.  |
 '|_______________________________________________________________________|
DEF SEG = &HB800:
H = 160 * (X - 1) + (Y - 1) * 2
i = 1:   p = 1:   K = 0:   E = 0
buff$ = "":   s$ = STRING$(Z, " "):   MID$(s$, 1) = Default$
IF Y + Z + 3 > 80 THEN StrWin$ = "StrWin$ - ERROR": GOTO SWerror:
COLOR 1, 7
FOR L = 0 TO 2:   FOR c = 0 TO (Z + 3.5) * 2
   K = H + 160 * L + c:   buff$ = buff$ + CHR$(PEEK(K))
   IF K MOD 2 THEN POKE K, 40:   ELSE POKE K, 176
   NEXT:   NEXT
X = X + 1:   Y = Y + 1
WHILE K <> 13 AND K <> 27
  LOCATE X, Y + 1: PRINT s$; : LOCATE X, Y + p, 1, i * 12, 13
  i$ = "":   DO:   i$ = INKEY$:   LOOP WHILE i$ = ""
  K = ASC(i$):   SELECT CASE i$
  CASE IS = CHR$(0) + CHR$(77)
    IF p < Z THEN p = p + 1:  ELSE SOUND 900, 1
  CASE IS = CHR$(0) + CHR$(75)
     IF p > 1 THEN p = p - 1:   ELSE SOUND 500, 1
  CASE IS = CHR$(0) + CHR$(82)
     i = ABS(i - 1):   SOUND 300 - i * 100, .3
  CASE IS = CHR$(0) + CHR$(71):   p = 1
  CASE IS = CHR$(0) + CHR$(79):   p = LEN(RTRIM$(s$)):   p = p - (p < Z)
  CASE IS = CHR$(8)
     IF p > 2 THEN s$ = LEFT$(s$, p - 2) + MID$(s$, p) + " "
     IF p = 2 THEN s$ = MID$(s$, 2) + " "
     IF p > 1 THEN p = p - 1:   ELSE SOUND 1000, 1
  CASE IS = CHR$(0) + CHR$(83)
     IF p > LEN(RTRIM$(s$)) THEN SOUND 700, 1
     IF p = Z THEN MID$(s$, Z) = " "
     IF p < Z THEN s$ = LEFT$(s$, p - 1) + MID$(s$, p + 1) + " "
  END SELECT
  SELECT CASE K
  CASE 32 TO 127
     IF i = 1 THEN s$ = LEFT$(LEFT$(s$, p - 1) + i$ + MID$(s$, p), Z)
     IF i = 0 THEN MID$(s$, p) = CHR$(K):
     IF p < Z THEN p = p + 1:   ELSE SOUND 800, 1
  END SELECT
WEND
IF K = 27 THEN s$ = ""
FOR L = 0 TO 2:   FOR c = 0 TO (Z + 3.5) * 2
K = H + 160 * L + c:   POKE K, ASC(buff$): buff$ = MID$(buff$, 2)
NEXT: NEXT
StrWin$ = RTRIM$(s$)
SWerror:
END FUNCTION

