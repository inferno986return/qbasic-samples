'To: KEVIN MOONLIGHT               Number: 9874     Refer: 9307  
'From: TONY CAVE                 Conference: Quik_Bas (3)
'Date: 08-30-96 03:55              BBS Name: The Programmer's Mark BBS
'Subject: Starfield                     BBS ID: progmark
'°±²Û Hey Kevin Moonlight, what's up? Û²±°
'I hope you're the right person.  Someone was asking about a
'starfield generator.  Here is mine:

'''begin STARSIM2.BAS'''
'$DYNAMIC
RANDOMIZE TIMER
SCREEN 12
DEFINT A-Z
DIM linewrapfix AS LONG
IF COMMAND$ = "" THEN
INPUT "Enter number of stars:  "; nos
ELSE
nos = VAL(COMMAND$)
END IF
CLS
DIM starx(1 TO nos) AS SINGLE, stary(1 TO nos) AS SINGLE
DIM xplus(1 TO nos) AS SINGLE, yplus(1 TO nos) AS SINGLE
DIM xsav(1 TO nos) AS SINGLE, ysav(1 TO nos) AS SINGLE

FOR sn = 0 TO 10
fc = sn * .75 + 2
linewrapfix = (fc * 63 \ 10) * 256 + (fc * 63 \ 10) * 65536
PALETTE sn + 5, fc * 63 \ 10 + linewrapfix
NEXT

FOR sn = 1 TO nos
starx(sn) = 320: stary(sn) = 240
NEXT

FOR sn = 1 TO nos
xplus(sn) = RND * 4 - 2: yplus(sn) = RND * 4 - 2
NEXT

DO

FOR sn = 1 TO nos
rs = (ABS(xplus(sn)) OR ABS(yplus(sn)) + 1)
starx(sn) = starx(sn) + (xplus(sn) / 5) * rs * 7
stary(sn) = stary(sn) + (yplus(sn) / 5) * rs * 7
linewrapfix = starx(sn) < 0 OR starx(sn) > 640
IF linewrapfix OR stary(sn) < 0 OR stary(sn) > 480 THEN
starx(sn) = 320: stary(sn) = 240
xplus(sn) = RND * 4 - 2: yplus(sn) = RND * 4 - 2
END IF
NEXT

FOR sn = 1 TO nos
PSET (xsav(sn), ysav(sn)), 0
linewrapfix = (ABS(starx(sn) - 320) + ABS(stary(sn) - 240))
PSET (starx(sn), stary(sn)), linewrapfix * (10 / 260) + 5
NEXT

FOR sn = 1 TO nos
xsav(sn) = starx(sn): ysav(sn) = stary(sn)
NEXT

LOOP UNTIL INKEY$ <> ""
'''end STARSIM2.BAS'''
'THE EMPIRE AZLE,TEXAS 1(817)237-5649 237-0999 4-player Doom (FREE)
'GOMail v2.0 [94-0125]
'* Origin: H-E-B BBS  Help!!! get me out of here!!! (1:130/901)

                                            

