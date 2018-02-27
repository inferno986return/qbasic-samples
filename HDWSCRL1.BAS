============================================================================
     To: Sean Smith                    Number: 63       Refer: 1987    
   From: Earl Montgomery           Conference: FidoQBasic   (139)
   Date: 02-01-96 14:58              BBS Name: Exec-PC
Subject: Halfway graphic scroll        BBS ID: EXECPC
----------------------------------------------------------------------------

REM Sean is this what you want?
DEFINT A-Z
SCREEN 9: CLS
COLOR 4, 0
LOCATE 22, 1: PRINT "Sean Smith"
PRINT "Is this what you wanted? If not let me know."
CIRCLE (200, 200), 30
SLEEP (4)
FOR i% = 0 TO 13280 STEP 80
m% = FIX(i% / 256): L% = i% - (m% * 256)
OUT &H3D4, 12: OUT &H3D5, m%: OUT &H3D4, 13: OUT &H3D5, L%
WAIT &H3DA, 8 'Wait for vertical retrace
FOR D = 0 TO 1200: NEXT: 'Adjust for different scroll speed NEXT
SLEEP (2)
FOR i% = 13280 TO 0 STEP -80
m% = FIX(i% / 256): L% = i% - (m% * 256)
OUT &H3D4, 12: OUT &H3D5, m%: OUT &H3D4, 13: OUT &H3D5, L%
WAIT &H3DA, 8: 'waits for vertical retrace
FOR DELAY = 0 TO 1200: NEXT: 'Adjust for different scroll speed NEXT
SLEEP (3)
RUN
' That's it.
' Earl
--- Maximus 2.02
 * Origin: Verbose Ink * 214-437-0914 * USR V34+FAX (1:124/5125)
                                                                                
                             
