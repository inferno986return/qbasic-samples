'============================================================================
'     To: ALL                           Number: 120      Refer: 0     
'   From: NICK BELTRAN              Conference: QUIK_BAS -Qui (298)
'   Date: 10-19-97 03:10              BBS Name: YE OLDE PAWN SHOPPE ]I[
'Subject: Ummm Hi!                      BBS ID: YOPSIII
'----------------------------------------------------------------------------
'
'Hi! my name Is nick and I have only written here once before but now have a
'true-real to life-Honest to god-cross my heart-Account so I can learn all
'The secrets to the world/universe! hmm<42>.
'
'I need the world's combined help or at least a couple
'of you guys that know WHY <sniff><Cry> somtimes some things just wont work.
'
'I wrote this small illusion (well kinda is) its modified now but
'before it had 8 arrays BUT it worked in the IDE but would not compile.
'Anyway I centered it (that's the cheap +37 (to lazy to add)).
'
'Could someone tell me :-
'        1, Why dose it do this to me? :(
'        2, Can it kindly be asked not to this any more ?
'        3, How ?
'        4, Why windows continues to sell ??
'

'IF ThisProgramIsInstalled THEN
'        Time  AND Space=Waste
'ELSE
'        FREE = Time AND Space
'END IF
'JagWare 18/10/97
SCREEN 13
DIM a(3976) AS INTEGER, b(3976) AS INTEGER, c(3976) AS INTEGER
DIM d(3976) AS INTEGER, e(3976) AS INTEGER

col% = 16: col1% = 16: col2% = 16: col3% = 16: col4% = 16: col5% = 16
col6% = 16: col7% = 16: flag = 1: flag1 = 1: flag2 = 1: flag3 = 1: flag4 = 1
flag5 = 1: flag6 = 1: flag7 = 1

DO
GET (1, 1 + 37)-(318, 25 + 37), a
PUT (2, 1 + 37), a, PSET
LINE (1, 1 + 37)-(1, 25 + 37), col%
IF flag = 1 THEN col% = col% + 1: IF col% = 32 THEN flag = 2
IF flag = 2 THEN col% = col% - 1: IF col% = 16 THEN flag = 1
GET (2, 26 + 37)-(319, 50 + 37), b
PUT (1, 26 + 37), b, PSET
LINE (319, 26 + 37)-(319, 50 + 37), col1%
IF flag1 = 1 THEN col1% = col1% + 1: IF col1% = 32 THEN flag1 = 2
IF flag1 = 2 THEN col1% = col1% - 1: IF col1% = 16 THEN flag1 = 1
GET (1, 51 + 37)-(318, 75 + 37), c
PUT (2, 51 + 37), c, PSET
LINE (1, 51 + 37)-(1, 75 + 37), col2%
IF flag2 = 1 THEN col2% = col2% + 1: IF col2% = 32 THEN flag2 = 2
IF flag2 = 2 THEN col2% = col2% - 1: IF col2% = 16 THEN flag2 = 1
GET (2, 76 + 37)-(319, 100 + 37), d
PUT (1, 76 + 37), d, PSET
LINE (319, 76 + 37)-(319, 100 + 37), col3%
IF flag3 = 1 THEN col3% = col3% + 1: IF col3% = 32 THEN flag3 = 2
IF flag3 = 2 THEN col3% = col3% - 1: IF col3% = 16 THEN flag3 = 1
GET (1, 101 + 37)-(318, 125 + 37), e
PUT (2, 101 + 37), e, PSET
LINE (1, 101 + 37)-(1, 125 + 37), col4%
IF flag4 = 1 THEN col4% = col4% + 1: IF col4% = 32 THEN flag4 = 2
IF flag4 = 2 THEN col4% = col4% - 1: IF col4% = 16 THEN flag4 = 1
LOOP UNTIL LEN(INKEY$)


'... Don't worry Nick it wasn't you; Oh, Good!
'--- FMail/Win32 1.22
' * Origin: Cyberia: You know you want it. [02-9534-1702] (3:712/848)          
                                                                                
                    

