'============================================================================
'     To: TIKA CARR                     Number: 4        Refer: 0001  
'   From: CHARLES GODARD            Conference: FidoQBasic   (139)
'   Date: 10-30-96 08:02              BBS Name: Exec-PC
'Subject: Thanks for the credit         BBS ID: EXECPC
'----------------------------------------------------------------------------

'Thanks for sharing the credit.  Your program was great without my help.
'I wrote this for fun and just so you'd know I noticed.
'                            Tika.bas
'                        by Charles Godard
'                           10/24/96
'Marquee type text display for a text box
DECLARE FUNCTION Delay! (Dly!)
CLS : SCREEN 0, , 1, 1: CLS
SCREEN , , 1, 1: LOCATE 10, 37, 0: PRINT "wait a sec"
SCREEN 0, , 0, 1
DEF FnDelayS (Dly#)
start# = TIMER    ' <<WOW>>  Double (15-digit) precision    #
   DO UNTIL ABS(TIMER - start#) >= Dly#
      IF LEN(INKEY$) THEN
         LOCATE 18, 20: COLOR &HE, 0
         PRINT "It's nice to see your name in lights!"
         SYSTEM
      END IF
   LOOP
'FOR i = 1 TO 2900: NEXT i  'I thought this for/next would be less
FbDelayS = -1               'predictable on different speed machines,
                            'but it IS more manageable
END DEF
LOCATE 11
LOCATE 11, 25 'take out when adding last 2 Msg$ lines
COLOR &HF, 0
Tabs$ = SPACE$(16)
Msg$ = Tabs$ + "File Manager Selector" + CHR$(13)
Msg$ = Msg$ + Tabs$ + "FILEMAN.BAS" + CHR$(13)
Msg$ = Msg$ + Tabs$ + "by Tika Carr & Charles Godard, QuickBasic Echo"
'Msg$ = Msg$ + CHR$(13) + Tabs$ + "Donated to the Public Domain"
'Msg$ = Msg$ + CHR$(13) + Tabs$ + "No Gaurantee is expressed or implied."
PRINT Msg$
Fg = &H1E: Fg1 = &HC: Dly# = .01#: Star$ = CHR$(42)
LOCATE 10
DO
   FOR x = 10 TO 70 STEP 2
      LOCATE , x: COLOR Fg: SWAP Fg, Fg1
      PRINT Star$;
      status = FnDelayS(Dly#)
   NEXT
   FOR y = 10 TO 16
      LOCATE y, 70: COLOR Fg
      PRINT Star$; : SWAP Fg, Fg1
      status = FnDelayS(Dly#)
   NEXT y
   FOR x = 70 TO 10 STEP -2
      LOCATE , x: COLOR Fg: SWAP Fg, Fg1
      PRINT Star$;
      status = FnDelayS(Dly#)
   NEXT
   FOR y = 15 TO 10 STEP -1
      LOCATE y, 10: COLOR Fg
      PRINT Star$; : SWAP Fg, Fg1
      status = FnDelayS(Dly#)
   NEXT y
   SCREEN 0, , 0, 0:
LOOP UNTIL LEN(INKEY$)
END
'     Delhi, La.,  Gateway to:   Poverty Point  Archaeological Center
'     Site of the oldest  pre-historic  civilization in North America
'     HTTP://cust.iamerica.net/ggodard.................Chas.G@hdh.com
' -- SPEED 2.00 #2781:  Facts can ruin a Liberal's whole day.
'--- KBBS vZ.20p (#ARI-00115)
' * Origin: * The Macon Ridge Connection * (318) 878-8044 * Sysop:(1:380/10)
 

