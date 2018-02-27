'===================================================================
'     To: All                           Number: 30       Refer: 0338  
'   From: Kevin Krumwiede           Conference: FidoQBasic   (139)
'   Date: 01-30-96 15:34              BBS Name: Exec-PC
'Subject: Freak                         BBS ID: EXECPC
'-------------------------------------------------------------------
'Here is a little proggie called FREAK which I wrote while bored.
'It is a screen saver of sorts.
RANDOMIZE TIMER
SCREEN 12
DO
   COLOR INT(RND * 16)
   FOR c% = 1 TO 1000
      IF INKEY$ <> "" THEN EXIT DO
      ctrx% = INT(RND * 640)
      ctry% = INT(RND * 480)
      rad% = RND * 50
      CIRCLE (ctrx%, ctry%), rad%
   NEXT c%
LOOP
SCREEN 0
'--- WinQwk 2.0 a#0
'--- Alexi/Mail+T.A.G. 2.7b Standard
' * Origin: T.A.R.D.I.S. BASICly Rules! 1-219-426-4232 (1:236/75)
                                                                           

