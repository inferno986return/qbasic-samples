'_|_|_|   NAMEBASE.BAS   PUBLIC DOMAIN   by Kurt Kuzba. (6/6/1999)
'_|_|_|   A simple example of using RANDOM files with QB.
'_|_|_|   No warrantee or guarantee is given or implied.
DECLARE SUB EditField (Fld%, d%)
DECLARE FUNCTION GetStr$ (row%, col%, p$, l%)
DECLARE SUB ShowRecord ()
DECLARE SUB ShowField (x%)
DECLARE FUNCTION GetKey% ()
TYPE Addressbook
  f AS STRING * 21: M AS STRING * 21: l AS STRING * 21
  str AS STRING * 21: cty AS STRING * 21: stt AS STRING * 2
  z AS STRING * 10: ph AS STRING * 12: END TYPE
DIM SHARED Adr(1) AS Addressbook: AK$ = "<*>-Press any Key-<*>"
Adr(0).f = " ": Adr(0).M = " ": Adr(0).l = " ": Adr(0).str = " "
Adr(0).cty = " ": Adr(0).stt = " ": Adr(0).z = " ": Adr(0).ph = " "
COLOR 15, 1: CLS : R% = 1: Fld% = 0
OPEN "adrsbook.dat" FOR RANDOM AS #1 LEN = 129
DO: Recs% = LOF(1) \ 129: LOCATE 2, 5: COLOR 15, 1
  PRINT "There are"; Recs%; "records.       "
  IF Recs% = 1 THEN LOCATE 2, 5: PRINT "There is 1 record.       "
  COLOR 1, 3: LOCATE 4, 5
  PRINT " Record #"; LEFT$(STR$(R%) + "   ", 5); : COLOR , 1
  IF R% > Recs% THEN
    COLOR 28: PRINT "  Record does not exist": Adr(1) = Adr(0)
  ELSE PRINT "                       ": GET #1, R%, Adr(1)
  END IF: ShowRecord: COLOR 14, 0
  LOCATE 18, 5: PRINT " Use cursor up and down to change field.   "
  LOCATE , 5: PRINT " Use page up and down to change record.    "
  LOCATE , 5: PRINT " Use ESCAPE to exit program                "
  LOCATE , 5: PRINT " Use Ctrl/S to save present record         "
  LOCATE , 5: PRINT " Use any alpha to edit present field       "
  DO: k% = GetKey
keyloop:
    SELECT CASE k%
      CASE 13, 32 TO 127: EditField Fld%, k%
        IF k% < 0 THEN GOTO keyloop    '_|_|_|  GOTO can be useful!
      CASE 19: SOUND 300, 5: PUT #1, R%, Adr(1): EXIT DO
      CASE 27: CLOSE 1: SYSTEM
      CASE -81: R% = R% + (R% > 1): Fld% = 0: EXIT DO
      CASE -80: f% = Fld%: Fld% = (Fld% + 1) MOD 8
        ShowField Fld%: ShowField f%
      CASE -73: R% = R% + 1: Fld% = 0: EXIT DO
      CASE -72: f% = Fld%: Fld% = (Fld% + 7) MOD 8
        ShowField Fld%: ShowField f%
      CASE -60:
    END SELECT: LOOP: LOOP: SYSTEM
'_|_|_|   END   NAMEBASE.BAS   PUBLIC DOMAIN

SUB EditField (f%, d%) : SELECT CASE f%
    CASE 0: l% = 21: Adr(1).f = GetStr$(f% + 6, 22, Adr(1).f, l%)
    CASE 1: l% = 21: Adr(1).M = GetStr$(f% + 6, 22, Adr(1).M, l%)
    CASE 2: l% = 21: Adr(1).l = GetStr$(f% + 6, 22, Adr(1).l, l%)
    CASE 3: l% = 21: Adr(1).str = GetStr$(f% + 6, 22, Adr(1).str, l%)
    CASE 4: l% = 21: Adr(1).cty = GetStr$(f% + 6, 22, Adr(1).cty, l%)
    CASE 5: l% = 2: Adr(1).stt = GetStr$(f% + 6, 22, Adr(1).stt, l%)
    CASE 6: l% = 10: Adr(1).z = GetStr$(f% + 6, 22, Adr(1).z, l%)
    CASE 7: l% = 12: Adr(1).ph = GetStr$(f% + 6, 22, Adr(1).ph, l%)
  END SELECT: ShowField f%: d% = l%: END SUB

FUNCTION GetKey% : k$ = INKEY$: IF k$ = "" THEN EXIT FUNCTION
  k% = ASC(k$): IF k% = 0 THEN k% = -ASC(MID$(k$, 2))
  GetKey% = k%: END FUNCTION

FUNCTION GetStr$ (row%, col%, p$, l%) : Ed$ = RTRIM$(p$):
  DO: COLOR 4, 7: LOCATE row%, col%, 0: PRINT SPACE$(l%);
    LOCATE , col%: PRINT Ed$; : c% = LEN(Ed$): LOCATE , c% + col%, 1
    DO: k% = GetKey%: SELECT CASE k%
        CASE 8: SELECT CASE c%
            CASE 1: Ed$ = "": EXIT DO
            CASE IS > 1: Ed$ = LEFT$(Ed$, c% - 1): EXIT DO
            CASE ELSE: SOUND 800, 1
          END SELECT
        CASE 13, -72, -80: l% = k%: GetStr$ = Ed$: EXIT FUNCTION
        CASE 27: GetStr$ = p$: EXIT FUNCTION
        CASE 32 TO 127: SELECT CASE c%
            CASE IS >= l%: MID$(Ed$, l%) = CHR$(k%)
            CASE ELSE: Ed$ = Ed$ + CHR$(k%)
          END SELECT: EXIT DO
      END SELECT: LOOP: LOOP: END FUNCTION

SUB ShowField (x%) : SHARED Fld%: LOCATE x% + 6, 5, 0: COLOR 11, 1
  IF Fld% = x% THEN COLOR 1, 3
  SELECT CASE x%
    CASE 0: PRINT "| First Name .=> "; Adr(1).f
    CASE 1: PRINT "| Middle Name => "; Adr(1).M
    CASE 2: PRINT "| Last Name ..=> "; Adr(1).l
    CASE 3: PRINT "| Address ....=> "; Adr(1).str
    CASE 4: PRINT "| City .......=> "; Adr(1).cty
    CASE 5: PRINT "| State ......=> "; Adr(1).stt
    CASE 6: PRINT "| Zip ........=> "; Adr(1).z
    CASE 7: PRINT "| Phone ......=> "; Adr(1).ph
  END SELECT: END SUB

SUB ShowRecord : FOR x% = 0 TO 7: ShowField x%: NEXT: END SUB

