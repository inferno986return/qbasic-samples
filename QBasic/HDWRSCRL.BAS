'===================================================================
'     To: All                           Number: 5        Refer: 1929 
'   From: Earl Montgomery           Conference: FidoQBasic   (139)
'   Date: 02-01-96 01:22              BBS Name: Exec-PC
'Subject: Horizontal Scroll             BBS ID: EXECPC
'-------------------------------------------------------------------
 REM I have not figured out how to keep that horizontal line from
 REM appearing at the bottom of the screen. If any of you graphics
 REM gurus know, please tell me!
 REM By Earl Montgomery
 RANDOMIZE TIMER
 KEY(10) ON
 ON KEY(10) GOSUB endofprogram
 ST% = 4
 PRINT "Once the figure is drawn press any key to RESTART scroll."
 PRINT "                                          ^^^^^^^";
 PRINT : PRINT "Exit this program by pressing the <F10 Key!!>"
 PRINT "PRESS ANY KEY TO BEGIN"
 SLEEP (100)
 PI = 3.141593: DEF FNX (A) = (SIN(A) + 1) * 300
 DEF FNY (A) = (COS(A) + 1) * 150: A = 0: B = 0
 SCREEN 9: COLOR 8, 0
 PALETTE 8, 0
tryagain:
 AA = RND * 20: BB = RND * 20: T = RND * 8
 IF AA = BB% THEN GOTO tryagain:
 IF AA < 8 OR BB < 8 THEN GOTO tryagain
 IF T < 6 THEN T = 6
 FOR X = 0 TO PI * 2 STEP PI / AA / T
 A = A + PI / AA: B = B + PI / BB: LINE (FNX(A), FNY(A))-(FNX(B), FNY(B))
 NEXT
 PAINT (2, 2), 8, 8
 PAINT (638, 2), 8, 8
 PAINT (2, 238), 8, 8
 PAINT (2, 638), 8, 8
 FOR H% = 0 TO 610 STEP 16
 FOR V% = 0 TO 320 STEP ST%
 LOCATE 24, 2
 PRINT "This is a test of the emergency broadcast system."
choosecolor:
 f% = RND * 15
 IF f% = 8 THEN GOTO choosecolor
 PAINT (H%, V%), f%, 8
 NEXT: NEXT
scroll:
 FOR X% = 1 TO 63
 OUT &H3D4, 13: OUT &H3D5, X%
 SOUND 2000, 2
pickagain:
 fg% = RND * 15
 IF fg% = 8 THEN GOTO pickagain
 PALETTE fg%, X%
 NEXT
 SLEEP (100)
 GOTO scroll
endofprogram:
 OUT &H3D4, 13: OUT &H3D5, 0: CLS : COLOR 15, 0: PALETTE: END
'--- Maximus 2.02
' * Origin: Verbose Ink * 214-437-0914 * USR V34+FAX (1:124/5125)
                                                                                
                                           

