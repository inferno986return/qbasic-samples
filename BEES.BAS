'============================================================================
'     To: All                           Number: 135      Refer: 0004  
'   From: Richard Dale              Conference: FidoQBasic   (139)
'   Date: 04-21-96 09:47              BBS Name: Exec-PC
'Subject: Flying bees source code       BBS ID: EXECPC
'----------------------------------------------------------------------------

' Works best when number of bees is 5 to 100 in INPUT statement for a
' 386, slightly higher on 486.  Experiment, though.  A really high
' number can provide a visual excitement, though may take some time to
' set up the display.  May not run on CGA.  All I have is VGA.
DECLARE SUB wasp (wx%(), wy%())
DECLARE SUB bees (wx%(), wy%(), x%(), y%(), xv%(), yv%(), bcolor%)
DEFINT A-Z
COMMON SHARED winW, winH, winX, winY, border, NoBees
DEFLNG T: CLS
INPUT "Number of bees (1 to 10000)"; NoBees
IF NoBees < 1 OR NoBees > 10000 THEN RUN
SCREEN 12: RANDOMIZE TIMER: CLS
DIM wx(0 TO 2): DIM wy(0 TO 2)
DIM x(0 TO 2, 1 TO NoBees): DIM y(0 TO 2, 1 TO NoBees)
DIM xv(1 TO NoBees): DIM yv(1 TO NoBees)
winW = 0: winH = 0: winX = 0: winY = 0: maxx = 640: maxy = 480
' wasp will not go closer than this to the edges
    border = INT(RND * 75): winW = maxx: winH = maxy
' Initialize the wasp
    wx(0) = 640 / 2: wy(0) = 480 / 2: wx(1) = wx(0): wy(1) = wy(0)
'Initialize the bees
    FOR b = 1 TO NoBees
        x(0, b) = INT(RND * winW): x(1, b) = x(0, b)
        y(0, b) = INT(RND * winH): y(1, b) = y(0, b)
        xv(b) = 7 * (RND - .5): yv(b) = 7 * (RND - .5)
    NEXT b
    bcolor = 9: t1 = TIMER
    DO UNTIL INKEY$ <> ""
    CALL wasp(wx(), wy()): CALL bees(wx(), wy(), x(), y(), xv(), yv(), bcolor)
' change bee colors
    IF TIMER - t1 > 2 THEN
        bcolor = bcolor + 1
        IF bcolor = 14 THEN bcolor = 9
        t1 = TIMER
    END IF
    LOOP: SCREEN 0
END
DEFINT T
'---
' * OLX 2.1 * Ted Kennedy Party Tip - Take Off Pants, Mingle
'--- FidoPCB v1.4 [ff013/c]
' * Origin: Sound Advice - 24 Nodes (816)436-4516 (1:280/333)
                                                                                

SUB bees (wx(), wy(), x(), y(), xv(), yv(), bcolor)
' Do not let things settle down
    bb = INT(NoBees * RND + 1)
    xv(bb) = xv(bb) + (RND - .5) * 3: yv(bb) = yv(bb) + (RND - .5) * 3
FOR b = 1 TO NoBees
' Age position arrays
    x(2, b) = x(1, b): y(2, b) = y(1, b)
    x(1, b) = x(0, b): y(1, b) = y(0, b)
    dx = wx(1) - x(1, b): dy = wy(1) - y(1, b)
    distance = ABS(dx) + ABS(dy)    ' Approximation
    IF distance = 0 THEN distance = 1
    bee.acc = 3
' Accelerate
    xv(b) = xv(b) + dx * bee.acc / distance
    yv(b) = yv(b) + dy * bee.acc / distance
    bee.vel = 11
' Speed Checks
    IF xv(b) > bee.vel THEN xv(b) = bee.vel
    IF xv(b) < -bee.vel THEN xv(b) = -bee.vel
    IF yv(b) > bee.vel THEN yv(b) = bee.vel
    IF yv(b) < -bee.vel THEN yv(b) = -bee.vel
' Move the bees
    x(0, b) = x(1, b) + xv(b): y(0, b) = y(1, b) + yv(b)
    LINE (x(1, b), y(1, b))-(x(2, b), y(2, b)), 0
    LINE (x(0, b), y(0, b))-(x(1, b), y(1, b)), bcolor
NEXT b
END SUB

SUB wasp (wx(), wy()) STATIC
' Age the arrays
    wx(2) = wx(1): wx(1) = wx(0): wy(2) = wy(1): wy(1) = wy(0)
' Accelerate (bias random variable to ensure wasp convers entire screen)
    wasp.acc = 5
    wxv = wxv + INT((RND - .4) * wasp.acc)
    wyv = wyv + INT((RND - .4) * wasp.acc)
' Speed limit checks
    wxv.max = INT(RND * 30) + 20: wyv.max = INT(RND * 30) + 20
    IF wxv > wxv.max THEN wxv = wxv.max
    IF wxv < -wxv.max THEN wxv = -wxv.max
    IF wyv > wyv.max THEN wyv = wyv.max
    IF wyv < -wyv.max THEN wyv = -wyv.max
' Move wasp
    wx(0) = wx(1) + wxv: wy(0) = wy(1) + wyv
' Bounce checks
    IF wx(0) < border OR wx(0) > winW - border - 1 THEN
        wxv = -wxv: wx(0) = wx(0) + wxv
    END IF
    IF wy(0) < border OR wy(0) > winH - border - 1 THEN
        wyv = -wyv: wy(0) = wy(0) + wyv
    END IF
    LINE (wx(1), wy(1))-(wx(2), wy(2)), 0
    LINE (wx(0), wy(0))-(wx(1), wy(1)), 15
END SUB

