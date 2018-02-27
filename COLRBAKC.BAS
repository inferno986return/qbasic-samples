'To: All                           Number: 9862     Refer: 0     
'From: Jim Broadbent             Conference: Quik_Bas (3)
'Date: 09-13-96 10:31              BBS Name: The Programmer's Mark BBS
'Subject: Palette Problem Solved  2     BBS ID: progmark
'       *************************************************************   
'       ***        Message box routine for SCREEN 12 mode         ***
'       ***               by  James N. Broadbent                  ***
'       ***                 September 12, 1996                    ***
'       *************************************************************
       
'       Set the palette attributes as per the color data below
'       NOTE: ATTRIBUTE 1 will be set to BLACK in subroutine
'             scr12hue.
 
        DECLARE SUB scr12hue ()
        SCREEN 12
        DIM SHARED newcolor&(15)
  
'       set the palette attributes
        CALL scr12hue
 
'       define the background to be newcolor&(2)
'       (same as attribute 2)
        PALETTE 0, newcolor&(2)
 
'       paint the entire screen black (attribute 1)
        LINE (0, 0)-(639, 479), 1, BF
 
'       Create a window using attribute 2 as fill color
'       which is the same as the background color
        LINE (200, 100)-(400, 300), 2, BF
 
'       Add a nice border
        LINE (200, 100)-(400, 300), 3, B
        LINE (202, 102)-(398, 298), 1, B
 
'       Add message for the box
        LOCATE 8, 29
        COLOR 11
        PRINT "Thought for the Day";
        LOCATE 12, 30
        COLOR 3
        PRINT "Still looking for"
        LOCATE 13, 29
        PRINT "LIFE'S CHEAT CODES?";
        LOCATE 18, 27
        COLOR 11
        PRINT "Press <any key> to exit";
       
'       That's all folks..........."    
        DO WHILE INKEY$ = "": LOOP
        SCREEN 0
        END
 
' attribute #1 will be black
colordata:         
 
DATA 0,0,0        
DATA 40,40,60
DATA 40,60,60
DATA 0,40,60
DATA 0,20,40
DATA 20,20,20
DATA 0,20,0
DATA 0,40,0
DATA 60,40,60
DATA 60,0,40
DATA 40,0,20
DATA 40,0,0
DATA 60,0,0
DATA 60,40,0
DATA 60,60,60
'--- Maximus 2.01wb
'* Origin: RASCAL Calgary, Alberta
'(403)686-2550 USR V.Everything (1:134/122)

                                                                                
                                      

SUB scr12hue
      
'       this subroutine assigns a selection of attribute
'       colors based on the information in the data statements
'       following colordata in the main module:
 
        RESTORE colordata
        FOR n% = 1 TO 15
                READ bl%, gr%, rd%
                newcolor&(n%) = (65536 * bl%) + (256 * gr%) + rd%
                PALETTE n%, newcolor&(n%)
        NEXT n%
 
 
END SUB

