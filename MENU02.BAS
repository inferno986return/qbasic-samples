' [=-=] ANIPRMPT.BAS      [=-=] [=-=] [=-=] [=-=] [=-=] [=-=] [=-=]
' [=-=]  This program demostrates a few simple principles of  [=-=]
' [=-=]  creation and use of animated prompts and menus.      [=-=]
' [=-=]  Released to the Public Domain     by     Kurt Kuzba  [=-=]
' [=-=] [=-=] [=-=] [=-=] [=-=] [=-=] [=-=] [=-=] [=-=] [=-=] [=-=]
DIM menu$(9), prompt$(5): COLOR 9, 1: CLS
prompt$(0) = "/%%%/-\|": prompt$(1) = "OOO))|((": prompt$(2) = "\_/|\~/|"
prompt$(3) = "/-\|": prompt$(4) = "\_/|": prompt$(5) = "ÜÝßÞ"
menu$(0) = "Exit Program": menu$(1) = "Prompt #1": menu$(2) = "Prompt #1"
menu$(3) = "Prompt #3": menu$(4) = "Prompt #4": menu$(5) = "Prompt #5"
menu$(6) = "Prompt #6": menu$(7) = "Speed Up": menu$(8) = "Slow Down"
LOCATE 5, 20: PRINT "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
LOCATE , 20: PRINT "³      ANIMATED PROMPT DEMO MENU      ³"
LOCATE , 20: PRINT "ÃÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
FOR p% = 0 TO 8
   LOCATE , 20: PRINT "³   ³                                 ³"
NEXT
LOCATE , 20: PRINT "ÀÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
COLOR 10: FOR p% = 0 TO 8
   COLOR 14: LOCATE 8 + p%, 21: PRINT p%;
   COLOR 12: LOCATE , 26: PRINT menu$(p%);
   IF p% > 0 AND p% < 7 THEN PRINT "   "; prompt$(p% - 1):  ELSE PRINT
NEXT
pr% = 0: lim% = 7: speed% = 0: p% = -1
COLOR 15: LOCATE 18, 20: PRINT " Select : ";

WHILE p% <> 0
  c% = (c% MOD lim%) + 1
  p$ = MID$(prompt$(pr%), c%, 1)
  FOR slow% = 0 TO speed%
    WAIT &H3DA, 8, 8: WAIT &H3DA, 8, 0
  NEXT
  PRINT p$; CHR$(29);
  p$ = INKEY$: p% = INSTR(" 012345678", p$) - 2
  SELECT CASE p%
      CASE 0: END
      CASE 1: pr% = 0: lim% = LEN(prompt$(pr%)) - 1
      CASE 2: pr% = 1: lim% = LEN(prompt$(pr%)) - 1
      CASE 3: pr% = 2: lim% = LEN(prompt$(pr%)) - 1
      CASE 4: pr% = 3: lim% = LEN(prompt$(pr%)) - 1
      CASE 5: pr% = 4: lim% = LEN(prompt$(pr%)) - 1
      CASE 6: pr% = 5: lim% = LEN(prompt$(pr%)) - 1
      CASE 7: IF speed% > 1 THEN : speed% = speed% - 1:  ELSE BEEP
      CASE 8: IF speed% < 5 THEN speed% = speed% + 1:  ELSE BEEP
   END SELECT
WEND
PRINT p$
' [=-=] ANIPRMPT.BAS      [=-=] [=-=] [=-=] [=-=] [=-=] [=-=] [=-=]

