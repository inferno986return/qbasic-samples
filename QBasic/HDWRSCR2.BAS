'============================================================================
'     To: Michael Yount                 Number: 151      Refer: 0003  
'   From: Earl Montgomery           Conference: FidoQBasic   (139)
'   Date: 02-03-96 15:08              BBS Name: Exec-PC
'Subject: Full screen graphic scrol     BBS ID: EXECPC
'----------------------------------------------------------------------------

DEFINT A-Z
SCREEN 9: CLS
COLOR 4, 0
FOR x = 1 TO 25
PRINT STRING$(80, 97);
NEXT
SLEEP (4)
FOR i% = 0 TO 32000 STEP 80
m% = FIX(i% / 256): L% = i% - (m% * 256)
OUT &H3D4, 12: OUT &H3D5, m%: OUT &H3D4, 13: OUT &H3D5, L%
WAIT &H3DA, 8 'Wait for vertical retrace
FOR D = 0 TO 1200: NEXT: 'Adjust for different scroll speed
NEXT
SLEEP (2)
FOR i% = 32000 TO 0 STEP -80
m% = FIX(i% / 256): L% = i% - (m% * 256)
OUT &H3D4, 12: OUT &H3D5, m%: OUT &H3D4, 13: OUT &H3D5, L%
WAIT &H3DA, 8: 'waits for vertical retrace
FOR DELAY = 0 TO 1200: NEXT: 'Adjust for different scroll speed
NEXT
SLEEP (100)
'--- Maximus 2.02
' * Origin: Verbose Ink * 214-437-0914 * USR V34+FAX (1:124/5125)
                                                                                
                   

