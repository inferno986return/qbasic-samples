'From: Andy Hayes                Conference: FidoQBasic   (139)
'Date: 02-17-96 18:15              BBS Name: Exec-PC
'Subject: ANOTHER Circle  Graphic       BBS ID: EXECPC
'Yep!  Here's another one! (:
'ANOTHER Cool Circle ScreenSaver
'Made by Andy Hayes
'Comments, Suggestions, Etc., are welcome.
SCREEN 12: CLS
RANDOMIZE TIMER
Start:
PointX = INT((640 - 0 + 1) * RND + 0)  'Random points for Circle
PointY = INT((480 - 0 + 1) * RND + 0)  'Position
Radius = INT((100 - 10 + 1) * RND + 10)
C = RND * 15
IF INKEY$ <> "" THEN GOTO Keypress
'For ScreenSaver... keypress ends prog.
FOR x = 1 TO 100
 CIRCLE (PointX, PointY), x, C
NEXT x
GOTO Start
Keypress:  'A key was pressed.
END
'Andy  <Netmail 1:231/75>
'* Origin: Digital Dungeon - (317) 398-2622 - Fido (1:231/75)
                                                                                
                 

