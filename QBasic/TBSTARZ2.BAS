'============================================================================
'     To: All                           Number: 36       Refer: 0005  
'   From: Tommy Brake               Conference: FidoQBasic   (139)
'   Date: 01-18-96 01:27              BBS Name: Exec-PC
'Subject: Graphics                      BBS ID: EXECPC
'----------------------------------------------------------------------------

'well, This is my Starfield, version 2...I found it runs about 3000% faster
'than the original. Plus, It has a nifty "%DONE" thing. No color selection
'though.
'$DYNAMIC
DEFINT A-Z
SCREEN 12
INPUT "Number of stars (suggested 10-30000)", FF
INPUT "Speed(suggested 10-100)", DF
SCREEN 0
LOCATE 10, 36
COLOR 20, 0
PRINT "INITALIZING"
COLOR 12, 0
LOCATE 12, 31
PRINT "PRESS ANY KEY TO ABORT"
COLOR 15, 0
A$ = ""
RANDOMIZE TIMER
DIM A(FF): DIM B(FF)
DIM C(FF): DIM D(FF)
DIM XX(FF)
FOR G = 1 TO FF
A$ = INKEY$
IF A$ <> "" THEN
CLS
PRINT "SEQUENCE GENERATION ABORTED....RETURNING TO SYSTEM"
SLEEP 4
SYSTEM
END IF
X = INT(RND * 4) + 1
IF X = 1 THEN
A(G) = INT(RND * 10) + 310: B(G) = INT(RND * 10) + 230
C(G) = INT(RND * DF + 1) - DF - 1: D(G) = INT(RND * DF + 1) - DF - 1
ELSEIF X = 2 THEN
A(G) = INT(RND * 10) + 320: B(G) = INT(RND * 10) + 230
C(G) = INT(RND * DF + 1): D(G) = INT(RND * DF + 1) - DF - 1
ELSEIF X = 3 THEN
A(G) = INT(RND * 10) + 310: B(G) = INT(RND * 10) + 240
C(G) = INT(RND * DF + 1) - DF - 1: D(G) = INT(RND * DF + 1)
ELSEIF X = 4 THEN
A(G) = INT(RND * 10) + 320: B(G) = INT(RND * 10) + 240
C(G) = INT(RND * DF + 1): D(G) = INT(RND * DF + 1)
END IF
XX(G) = 7
LOCATE 11, 36
PRINT INT((G / FF) * 100); "% DONE     "
NEXT
SCREEN 12
PALETTE 15, 63 + 63 * 256 + 63 * 256 ^ 2
PALETTE 7, 32 + 32 * 256 ^ 2 + 32 * 256
DO
FOR G = 1 TO FF
PSET (A(G), B(G)), 0
PSET (A(G) + C(G), B(G) + D(G)), XX(G)
A(G) = A(G) + C(G): B(G) = B(G) + D(G)
PK = INT(RND * 200) + 1
IF PK < 25 THEN
XX(G) = 15
END IF
IF A(G) < 0 OR A(G) > 640 OR B(G) < 0 OR B(G) > 480 THEN
X = INT(RND * 4) + 1
IF X = 1 THEN
A(G) = INT(RND * 10) + 310: B(G) = INT(RND * 10) + 230
C(G) = INT(RND * DF + 1) - DF - 1: D(G) = INT(RND * DF + 1) - DF - 1
ELSEIF X = 2 THEN
A(G) = INT(RND * 10) + 320: B(G) = INT(RND * 10) + 230
C(G) = INT(RND * DF + 1): D(G) = INT(RND * DF + 1) - DF - 1
ELSEIF X = 3 THEN
A(G) = INT(RND * 10) + 310: B(G) = INT(RND * 10) + 240
C(G) = INT(RND * DF + 1) - DF - 1: D(G) = INT(RND * DF + 1)
ELSEIF X = 4 THEN
A(G) = INT(RND * 10) + 320: B(G) = INT(RND * 10) + 240
C(G) = INT(RND * DF + 1): D(G) = INT(RND * DF + 1)
END IF
XX(G) = 7
END IF
NEXT
A$ = INKEY$: LOOP UNTIL A$ <> ""
FOR G = 1 TO FF
FOR T = 1 TO 15
PSET (A(G), B(G)), T
NEXT
FOR J = 1 TO 2000
NEXT
FOR T = 15 TO 0 STEP -1
PSET (A(G), B(G)), T
NEXT
FOR J = 1 TO 2000
NEXT
NEXT
END
' þ JABBER v1.2 þ Windows 3.0 - From the people who brought you EDLIN.
'--- ProBoard v2.12 [Reg]
' * Origin: Ye Olde Coffee House, Watkinsville, GA, USA (1:370/50)
                                                                     

