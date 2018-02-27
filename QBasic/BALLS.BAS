'From: DAVE MORROW               Conference: QUIK_BAS -Qui (298)
'Date: 05-17-99 09:04              BBS Name: YE OLDE PAWN SHOPPE ]I[
'Subject: Masking Example               BBS ID: YOPSIII
'Here is a little something I made incase someone else is having
'trouble making "see-through" sprites. Thanks to Kurt Kuzba for
'telling me how it's done!

'==========================================================
'|| Very Easy Sprite Masking Example for Screen Mode 13. ||
'==========================================================
                                                
SCREEN 13
DEFINT A-Z
RANDOMIZE TIMER

'Dimension 2 arrays to hold our sprites. Each Sprite is 20x20.

DIM Sprite(212)
DIM Mask(212)
   
'This will draw a metalic looking ball in the top corner to serve as
'our sprite, and put it into the Sprite array.

     clr = 30
     FOR rad = 0 TO 10
      CIRCLE (10, 10), rad, clr
      CIRCLE (9, 10), rad, clr
      clr = clr - 1
     NEXT rad

     GET (0, 0)-(19, 19), Sprite

'Now we will put the color 255 in the spots we want to show through, and
'black in the spots that won't show through, and stick it into the Mask
'array.

     PAINT (0, 0), 255, 20
     PAINT (10, 10), 0, 255
    
     GET (0, 0)-(19, 19), Mask

'Next we put the sprite on the screen, and see that the background is
'preserved behind it!!!

CLS : PAINT (0, 0), RND * 255
WHILE INKEY$ = ""
x = RND * 299: y = RND * 179
PUT (x, y), Mask, AND
PUT (x, y), Sprite, OR
WEND

