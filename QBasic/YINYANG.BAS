'To: All                           Number: 5588     Refer: 0    
'From: Alex Wellerstein          Conference: Quik_Bas (3)
'Date: 06-17-96 10:43              BBS Name: The Programmer's Mark BBS
'Subject: YIN-YANG                      BBS ID: progmark
'I like this one alot. It uses the PALETTE statement to get the color fading
'effect. In SCREEN 12, and yah need 256 colors.  You might wanna adjust the
'delay variable.  It's there because QBASIC v.1.1 doesn't run it quite as
'smooth as QB 4.5 without it. If your using QB 4.5, or even QBASIC v.1.1, try
'it with delay set to 0.  In any matter, just dig it. Feedback?
DEFINT A-Z
delay = 500
k = 64
SCREEN 12: CLS
CIRCLE (320, 240), 200, 7: CIRCLE (320, 140), 100, 7
CIRCLE (320, 340), 100, 7: CIRCLE (320, 340), 15, 7
CIRCLE (320, 140), 15, 7
PAINT (320, 140), 7: PAINT (320, 320), 7
PAINT (300, 240), 7: PAINT (320, 340), 8, 7
PAINT (340, 140), 8, 7: CIRCLE (320, 140), 100, 8
PAINT (350, 140), 8, 7: CIRCLE (320, 240), 200, 15
DO
v = k
FOR i = 1 TO k
PALETTE 7, 65536 * (k - i) + 256 * (k - i) + (k - i)
PALETTE 8, 65536 * (k - v) + 256 * (k - v) + (k - v)
v = v - 1
FOR i# = 1 TO delay: NEXT i#
IF INP(96) = 1 THEN SYSTEM
NEXT i
v = 1
FOR i = k TO 1 STEP -1
PALETTE 7, 65536 * (k - i) + 256 * (k - i) + (k - i)
PALETTE 8, 65536 * (k - v) + 256 * (k - v) + (k - v)
FOR i# = 1 TO delay: WAIT &H3DA, 1, 1: WAIT &H3DA, 1, 0: NEXT i#
IF INP(96) = 1 THEN SYSTEM
NEXT i
LOOP
SYSTEM

