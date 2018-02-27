'============================================================================
'     To: ALL                           Number: 257      Refer: 0     
'   From: AARON TUCKER              Conference: QUIK_BAS -Qui (298)
'   Date: 04-24-97 12:33              BBS Name: YE OLDE PAWN SHOPPE ]I[
'Subject: ..Yet another 3 AM progra     BBS ID: YOPSIII
'----------------------------------------------------------------------------
'
'Subject: ..Yet another 3 AM program
'
'Hmm..well since i wrote this one during ZMH, i decided to go to sleep and
'upload it when I wake up....here ya go...

DEFINT A-Z        'DIEBUG.BAS - It's all yours
CLS               'Started writing another cheesy game that nobody
SCREEN 13         'would ever play, and got this weird effect that
RANDOMIZE TIMER   'sort of reminds me of a wanna-be Public Notice
px = 50           'Commercial.  In special lighting..what it looks
py = 50           'like when there's a drunk, half-dead fly crawling
kolor = 23        'around on your monitor.  (Englarged for detail)
                  'Yah right, drunk flies?  Where the heck am I,
                  'Mars?  Nope...aah..you're seeing it..yes...
                  'its getting clearer in your mind...when else
                  'would you make such a program....3AM!!!!
                  '--------------------------------------------
                  'On a more serious side, this is the first program
                  'I've ever used PSET in...i'm a graphics dummy...
start:          
 ' Lots of GOTOs and extra bloating of code
 ' to demonstrate wanna-be windows software.
 ' ya think Bill Gates would buy it?
IF INKEY$ > "" THEN SCREEN 0: WIDTH 80, 25: END
pickdir = INT(RND * 5) + 1
 
   IF pickdir = 1 AND px > 5 THEN PSET (px, py), 0: px = px - 1
   IF pickdir = 2 AND px < 310 THEN PSET (px, py), 0: px = px + 1
   IF pickdir = 3 AND py > 5 THEN PSET (px, py), 0: py = py - 1
   IF pickdir = 4 AND py < 190 THEN PSET (px, py), 0: py = py + 1
   IF pickdir = 5 THEN GOTO Fire
 
 
noaction:
PSET (px, py), 6: PSET (px + 1, py), 4
PSET (px - 1, py), 2: PSET (px, py + 1), 2
PSET (px, py - 1), 128: PSET (px + 1, py + 1), 6
PSET (px - 1, py - 1), 4
GOTO start
 
Fire:
FOR shot = 5 TO 1 STEP -1: PSET (px, py - shot), kolor
 NEXT shot
FOR shot = 5 TO 1 STEP -1: PSET (px, py + shot), kolor
 NEXT shot
FOR shot = 5 TO 1 STEP -1: PSET (px - shot, py - shot), kolor
 NEXT shot
FOR shot = 5 TO 1 STEP -1: PSET (px + shot, py + shot), kolor
 NEXT shot
FOR shot = 5 TO 1 STEP -1: PSET (px + shot, py - shot), kolor
 NEXT shot
FOR shot = 1 TO 5 STEP 1: PSET (px - shot, py + shot), kolor
 NEXT shot
FOR g = 1 TO 555: NEXT g '*DELAY****DELAY***DELAY***DELAY***
FOR shot = 5 TO 1 STEP -1: PSET (px, py - shot), 0
NEXT shot
FOR shot = 5 TO 1 STEP -1: PSET (px, py + shot), 0
NEXT shot
FOR shot = 5 TO 1 STEP -1: PSET (px + shot, py), 0
NEXT shot
FOR shot = 5 TO 1 STEP -1: PSET (px - shot, py), 0
NEXT shot
FOR shot = 5 TO 1 STEP -1: PSET (px - shot, py - shot), 0
NEXT shot
FOR shot = 5 TO 1 STEP -1: PSET (px + shot, py + shot), 0
NEXT shot
FOR shot = 5 TO 1 STEP -1: PSET (px + shot, py - shot), 0
NEXT shot
FOR shot = 1 TO 5 STEP 1: PSET (px - shot, py + shot), 0
NEXT shot: GOTO start
 
'Be sure to look for more 3AM programs from the twisted and demented
'or just sleep-deprived programmers in Fido's QUIK_BAS...woo hoo!
 
'>>> End of...zzzzzzzzzzzz...progr..zzz.ram...zzzz..here.
'--- Maximus 3.00
' * Origin:  (1:19/102)                                                        
                                                

