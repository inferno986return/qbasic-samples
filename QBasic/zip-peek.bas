DECLARE FUNCTION Save$ (text$)
t$ = SPACE$(5)
head$ = SPACE$(2)
fils = 0
dirs = 0
item = 0

CLS
INPUT "Open file  : ", file$
PRINT
OPEN file$ FOR BINARY AS #1
IF LOF(1) = 0 THEN CLOSE : KILL file$: PRINT "File does not exist.": END
GET #1, 1, head$
IF head$ <> "PK" THEN PRINT : PRINT "Not a zip file!": END
FOR I& = 25 TO LOF(1) - 5
  oldt$ = t$
  GET #1, I&, t$
  IF LEFT$(t$, 1) = CHR$(0) AND RIGHT$(t$, 3) = STRING$(3, CHR$(0)) THEN
    item = item + 1
    filename$ = SPACE$(ASC(MID$(t$, 2)))
    GET #1, I& + 5, filename$
    IF filename$ <> Save(filename$) THEN item = item - 1: GOTO getback
    IF LTRIM$(RTRIM$(filename$)) = "" THEN item = item - 1: GOTO getback
    IF LEN(LTRIM$(RTRIM$(filename$))) = 1 THEN item = item - 1: GOTO getback
    IF INSTR(filename$, "*") <> 0 THEN item = item - 1: GOTO getback
    PRINT "Item"; item; ":  "; filename$
    IF RIGHT$(filename$, 1) = "/" THEN dirs = dirs + 1 ELSE fils = fils + 1
  ELSEIF LEFT$(t$, 2) = "PK" AND RIGHT$(t$, 3) = CHR$(1) + CHR$(2) + CHR$(20) THEN
    PRINT I&
    EXIT FOR
  END IF
getback:
  xp = CSRLIN: yp = POS(1)
  IF I& + (fils * 65) + 20 > LOF(1) THEN EXIT FOR
  oldbar = bar
  bar = (I& * 80) \ LOF(1)
  IF bar <> oldbar THEN
    LOCATE 25, 1
    PRINT STRING$(bar, CHR$(177));
    LOCATE xp, yp
  END IF
NEXT I&
CLOSE #1

PRINT
PRINT
IF fils = 1 THEN PRINT " 1 file in total." ELSE PRINT fils; "files in total."
IF dirs = 1 THEN PRINT " 1 dir in total." ELSE PRINT dirs; "dirs in total."
LOCATE 25, 1: PRINT STRING$(80, CHR$(177));
SLEEP

FUNCTION Save$ (text$)
  FOR Position& = 1 TO LEN(text$)
    Char = ASC(MID$(text$, Position&, 1))
    IF Char < 32 OR Char > 126 THEN Char = 46
    Temp$ = Temp$ + CHR$(Char)
  NEXT
  Save$ = Temp$
END FUNCTION

