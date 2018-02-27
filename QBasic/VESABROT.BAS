'============================================================================
'     To: ALL                           Number: 273      Refer: 0035  
'   From: Earl Montgomery           Conference: FidoQBasic   (139)
'   Date: 04-17-96 22:34              BBS Name: Exec-PC
'Subject: VESABROT.BAS Part 1 of 2      BBS ID: EXECPC
'----------------------------------------------------------------------------

 '$INCLUDE: 'qb.bi'
 DIM inreg AS RegType
 DIM outreg AS RegType
 DEFDBL A-Z
 RANDOMIZE TIMER
 KEY(10) ON
 ON KEY(10) GOSUB endofprogram
 CLS : LOCATE 12, 30: COLOR 14: PRINT "WELCOME TO VESABROT"
 LOCATE 14, 23: PRINT "Brought to you by Earl Montgomery"
 SLEEP (2)
makeselection:
 CLS
 LOCATE 1, 1
 PRINT "1. 640x400x256"
 PRINT "2. 640x480x256"
 PRINT "3. 800x600x256"
 PRINT "4. 1024x768X16"
 PRINT "5. 1024x768x256"
 PRINT : INPUT "Select mode"; m%
 IF m% < 1 OR m% > 5 THEN BEEP: GOTO makeselection
 CLS
 INPUT "Let the computer select values <Y/N>"; q$
 q$ = UCASE$(q$)
 IF q$ = "Y" THEN CLS : GOTO randomselect
 INPUT "Acorner "; a
 acorner = a
 INPUT "Bcorner "; b
 bcorner = b
 INPUT "Side "; s
 side = s
 GOTO start
randomselect:
 acorner = (RND * 5 - 2)
 IF acorner > 2 THEN GOTO randomselect
 IF acorner < -2 THEN GOTO randomselect
bcornerselect:
 bcorner = (RND * 5 - 2)
 IF bcorner > 2 THEN GOTO bcornerselect
 IF bcorner < -2 THEN GOTO bcornerselect
sideselect:
 side = (RND * 5 - 2)
 IF side > 2 THEN GOTO sideselect
 IF side < -2 THEN GOTO sideselect
start:
 maxdwell = 150
 inreg.ax = &H4F02
 IF m% = 1 THEN inreg.bx = &H100
 IF m% = 2 THEN inreg.bx = &H101
 IF m% = 3 THEN inreg.bx = &H103
 IF m% = 4 THEN inreg.bx = &H104
 IF m% = 5 THEN inreg.bx = &H105
 CALL interrupt(&H10, inreg, outreg)
 IF m% = 4 THEN numcolors = 16:  ELSE numcolors = 256
 IF m% = 1 THEN numrows = 399
 IF m% = 2 THEN numrows = 479
 IF m% = 3 THEN numrows = 599
 IF m% = 4 OR m% = 5 THEN numrows = 767
 IF m% = 1 OR m% = 2 THEN numcols = 639
 IF m% = 3 THEN numcols = 799
 IF m% = 4 OR m% = 5 THEN numcols = 1023
 yoffset = 1: xoffset = 1
 highdwell = 0
 gap = side / numrows
 ac = acorner
 FOR x = xoffset TO numcols - 1 + xoffset
 ac = ac + gap: bc = bcorner
 FOR y = yoffset TO numrows - 1 + xoffset
 bc = bc + gap
 az = 0: bz = 0: count% = 0: size = 0
 WHILE (size < 4) AND (count% < maxdwell)
 temp = az * az - bz * bz + ac
 bz = 2 * az * bz + bc
 az = temp: size = az * az + bz * bz
 count% = count% + 1
 WEND
 IF (count% < maxdwell) AND (count% > highdwell) THEN highdwell = count%
 IF count% <> maxdwell THEN GOSUB setpixelcolor
 i$ = INKEY$
 IF i$ = CHR$(32) THEN GOTO restart
 NEXT: NEXT
 BEEP
wait1:
 i$ = INKEY$
 IF i$ = CHR$(32) THEN GOTO endofprogram:  ELSE GOTO wait1
setpixelcolor:
 c = count% MOD (numcolors - 1) + 1
 cc = &HC00 + c: z = numrows - y + 1
 inreg.ax = cc: inreg.bx = 0: inreg.cx = x: inreg.dx = z
 CALL interrupt(&H10, inreg, outreg)
 RETURN
endofprogram:
 SLEEP (500)
 inreg.ax = &H3: CALL interrupt(&H10, inreg, outreg)
 CLS : LOCATE 1, 1
 INPUT "[R]un again or [E]xit to DOS"; q$
 q$ = UCASE$(q$)
 IF q$ = "R" THEN RUN
 CLS : LOCATE 14, 23: PRINT "Make sure you rename any saved files!"
 SLEEP (2): SCREEN 0: CLS : END
restart:
 inreg.ax = &H3
 CALL interrupt(&H10, inreg, outreg)
 RUN

'--- Msgedsq 2.2e
' * Origin: Earl's Point * Dallas TX * (1:124/5125.11)

