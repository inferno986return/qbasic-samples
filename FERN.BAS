============================================================================
     To: All                           Number: 98       Refer: 0005    
   From: Rodney Taylor             Conference: FidoQBasic   (139)
   Date: 03-04-96 12:55              BBS Name: Exec-PC
Subject: Fern Leaf Fractol using P     BBS ID: EXECPC
----------------------------------------------------------------------------

Creates a picture of a fern leaf.  Please play with the numbers!
Especially the window, and the size.
------------cut here-------------
SCREEN 9
CLS
VIEW (0, 0)-(639, 349)
WINDOW (-4, 0)-(6, 10)
RANDOMIZE TIMER
x = 0
y = 0
WHILE INKEY$ = ""
     r = RND
     IF (r <= .01) THEN
             a = 0: b = 0: c = 0: d = .16: e = 0: f = 0
     ELSEIF r > .01 AND r <= .86 THEN
             a = .85: b = .04: c = -.04: d = .85: e = 0: f = 1.6
     ELSEIF r > .86 AND r <= .93 THEN
             a = .2: b = -.26: c = .23: d = .22: e = 0: f = 1.6
     ELSE
             a = -.15: b = .28: c = .26: d = .24: e = 0: f = .44
     END IF
     newx = (a * x) + (b * y) + e
     newy = (c * x) + (d * y) + f
     x = newx
     y = newy
     PSET (x, y), 2
WEND
SCREEN 0
-------------cut here--------------
~~~ TGWave v1.10 [NR]
--- Telegard v2.99.g4 [ml]
 * Origin: The Other Domain * Hawkestone ON * (705) 487-6520 (1:252/107)
                                                                                
                              
