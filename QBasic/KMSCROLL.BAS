CONST Esc = 27, DOWN = 80, UP = 72, LEFT = 75, RIGHT = 77, BS = 8
CONST Home = 71, EndKey = 79, PgDn = 81, PgUp = 73, ENTER = 13
CONST ScrnLen = 22
CONST FileName$ = "kmscroll.bas"
DIM File(800) AS STRING * 80
OPEN FileName$ FOR INPUT AS #1
CLS
DO
Lines = Lines + 1
LINE INPUT #1, File$(Lines)
LOOP WHILE NOT EOF(1)
CLOSE #1
LOCATE 1, 1
FOR a = 1 TO ScrnLen + 1
PRINT File$(a)
NEXT
CurA = ScrnLen + 1
CurB = 1

DO
1
        G$ = INKEY$
        SELECT CASE G$
                CASE CHR$(0) + CHR$(DOWN)
                        IF CurA = Lines THEN GOTO 1
                        LOCATE 1, 1
                        CurA = CurA + 1
                        FOR a = CurA - ScrnLen TO CurA
                                PRINT File$(a)
                        NEXT
               
                CASE CHR$(0) + CHR$(UP)
                        IF CurA - ScrnLen = 0 THEN GOTO 1
                        CurA = CurA - 1
                        LOCATE 1, 1
                        FOR a = CurA - ScrnLen TO CurA
                                PRINT File$(a)
                        NEXT

                CASE CHR$(0) + CHR$(PgDn)
                        IF CurA + ScrnLen >= Lines THEN CurA = Lines - ScrnLen
                        CurA = CurA + ScrnLen
                        LOCATE 1, 1
                        FOR a = CurA - ScrnLen TO CurA
                                PRINT File$(a)
                        NEXT
               
                CASE CHR$(0) + CHR$(PgUp)
                        IF CurA <= ScrnLen * 2 THEN CurA = 44
                        CurA = CurA - ScrnLen
                        LOCATE 1, 1
                        FOR a = CurA - ScrnLen TO CurA
                                PRINT File$(a)
                        NEXT
                                                                        
                CASE CHR$(0) + CHR$(Home)
                        CurA = ScrnLen + 1
                        LOCATE 1, 1
                        FOR a = CurA - ScrnLen TO CurA
                                PRINT File$(a)
                        NEXT
               
                CASE CHR$(0) + CHR$(EndKey)
                        CurA = Lines
                        LOCATE 1, 1
                        FOR a = CurA - ScrnLen TO CurA
                                PRINT File$(a)
                        NEXT
       
                CASE CHR$(Esc)
                        CLS
                        END
        END SELECT


LOOP

