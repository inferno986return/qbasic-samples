TYPE AttributeScreen
   ch AS STRING * 1
   cl AS STRING * 1
END TYPE
DECLARE SUB GetChar (Row%, Column%, C AS ANY)
DECLARE SUB PutChar (Row%, Column%, AttChr%, AttClr%)
msg$ = " Hello World! "
DIM buffer(1 TO 14) AS AttributeScreen
fore% = 15: back% = 1: blink% = 0
Attrib% = blink% * 128 + back% * 16 + fore%
FOR t% = 1 TO 14
   PutChar 10, t% + 33, ASC(MID$(msg$, t%)), Attrib%
NEXT
FOR t% = 1 TO 14: GetChar 10, t% + 33, buffer(t%): NEXT
FOR t% = 1 TO 14
   PutChar 12, t% + 33, ASC(buffer(t%).ch), ASC(buffer(t%).cl)
NEXT

SUB GetChar (Row%, Column%, C AS AttributeScreen)
   DEF SEG = &HB800
   Cel% = ((Row% * 80) + Column%) * 2 - 162
   C.ch = CHR$(PEEK(Cel%))
   C.cl = CHR$(PEEK(Cel% + 1))
   DEF SEG
END SUB

SUB PutChar (Row%, Column%, AttChr%, AttClr%)
   DEF SEG = &HB800
   Cel% = ((Row% * 80) + Column%) * 2 - 162
   POKE Cel%, AttChr%
   POKE Cel% + 1, AttClr%
   DEF SEG
END SUB

