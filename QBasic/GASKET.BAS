'From: Kevin Krumwiede           Conference: FidoQBasic   (139)
'Date: 02-13-96 22:15              BBS Name: Exec-PC
'Subject: Sierpinski Gasket             BBS ID: EXECPC
' Sierpinski Gasket Proggy
' Rulz!
' (I took too many math courses...)
SCREEN 12
DEFINT X-Y
FOR y = 0 TO 255
  FOR x = 0 TO y
    IF (x AND (y - x)) = 0 THEN
    PSET (x + 140 - .5 * y, y + 112), (x XOR y) MOD 15 + 1
    PSET (x + 500 - .5 * y, y + 112), (x AND y) MOD 15 + 1
    PSET (x + 320 - .5 * y, 367 - y), (x OR y) MOD 15 + 1
    END IF
  NEXT x
NEXT y
END
'--- WinQwk 2.0 a#0
'--- Alexi/Mail+T.A.G. 2.7b Standard
' * Origin: T.A.R.D.I.S. BASICly Rules! 1-219-426-4232 (1:236/75)
                                       

