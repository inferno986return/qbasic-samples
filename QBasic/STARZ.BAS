'============================================================================
'     To: All                           Number: 26       Refer: 0008  
'   From: Stephan van Loendersloot  Conference: FidoQBasic   (139)
'   Date: 01-06-96 17:38              BBS Name: Exec-PC
'Subject: Starfield                     BBS ID: EXECPC
'----------------------------------------------------------------------------
'
'Hello everybody,
'Since there have been some people that asked for snow dwarreling down and I
'came across a 666 bytes (.COM) file in the PASCAL echo that did a nice
'starfield, I decided to write the following starfield in BASIC. It even has
'parallax scrolling ;-)) (i.e. foreground moves faster than background).
'Now I'll start the assembly version. It'll be a lot faster I think...
'If you have any questions about the source, just ask me.
' Starfield - By Stephan van Loendersloot
' Date: 6 January 1996
' Released to the Public Domain
' No comment: It should be easy enough if you
' take a look at the names of the variables.
DIM XPos%(100)
DIM YPos%(100)
DIM Status%(100)
RANDOMIZE TIMER
SCREEN 13
COLOR 9
LOCATE 12, 13: PRINT "S T A R F I E L D"
FOR Init% = 1 TO 100
 XPos%(Init%) = INT(RND * 320) + 1
 YPos%(Init%) = INT(RND * 200) + 1
 Status%(Init%) = INT(RND * 3) + 1
 SELECT CASE Status%(Init%)
  CASE 1
   Col% = 8
  CASE 2
   Col% = 7
   CASE 3
    Col% = 15
 END SELECT
 PSET (XPos%(Init%), YPos%(Init%)), Col%
NEXT Init%
DO
 WAIT &H3DA, 8, 8
 WAIT &H3DA, 8, 0
 FOR Speed% = 1 TO 3
  FOR Display% = 1 TO 100
   IF POINT(XPos%(Display%), YPos%(Display%)) <> 9 THEN
    PSET (XPos%(Display%), YPos%(Display%)), 0
   END IF
   IF Speed% <= Status%(Display%) THEN XPos%(Display%) = XPos%(Display%) - 1
   IF XPos%(Display%) = 0 THEN
    XPos%(Display%) = 320
    YPos%(Display%) = INT(RND * 200) + 1
   END IF
   SELECT CASE Status%(Display%)
    CASE 1
     Col% = 8
    CASE 2
     Col% = 7
    CASE 3
     Col% = 15
   END SELECT
   IF POINT(XPos%(Display%), YPos%(Display%)) <> 9 THEN
    PSET (XPos%(Display%), YPos%(Display%)), Col%
   END IF
  NEXT Display%
 NEXT Speed%
LOOP WHILE INKEY$ = ""
'--- FMail/2 1.02
' * Origin: Just a fool to believe I have many things she needs. (2:282/701.9)
                      

