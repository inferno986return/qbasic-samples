============================================================================
     To: All                           Number: 9051     Refer: 0       
   From: James McMurrin            Conference: Quik_Bas (3)
   Date: 08-28-96 19:48              BBS Name: The Programmer's Mark BBS
Subject: Forest                        BBS ID: progmark
----------------------------------------------------------------------------

'A basic mathematical formula dressed up in a pretty way
'Warning: this will take a while on slower computers!
'By: James McMurrin
COMMON SHARED NUM AS DOUBLE
SCREEN 13
FOR PU = 1 TO 255
 OUT &H3C8, PU
 OUT &H3C9, PU / 2 + 20
 OUT &H3C9, PU / 4 + 10
 OUT &H3C9, PU / 6 + 5
NEXT PU
FOR L = 3 TO 3.996875 STEP .003125
 NUM = .5
  FOR Q = 1 TO 50
   NUM = NUM * L * (1 - NUM)
  NEXT Q
  DO
   NUM = NUM * L * (1 - NUM)
   DISROW = 200 - (NUM * 200)
   P = POINT(DISCOL, DISROW)
   IF P = 255 THEN
    EXIT DO
   ELSE
    P = P + 1
    LINE (DISCOL, DISROW)-(DISCOL, DISROW), P
   END IF
  LOOP
 DISCOL = DISCOL + 1
NEXT L
BEEP
WHILE INKEY$ = "": WEND

--- FMail 1.0g
 * Origin: The Philosopher's Stone- Harrison, AR (1:391/3000.4)

                                       
