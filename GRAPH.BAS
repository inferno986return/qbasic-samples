'============================================================================
'     To: Nick Coons                    Number: 9663     Refer: 0     
'   From: Jim Broadbent             Conference: Quik_Bas (3)
'   Date: 09-09-96 13:57              BBS Name: The Programmer's Mark BBS
'Subject: Continuous Graph 2/3          BBS ID: progmark
'----------------------------------------------------------------------------
'
'------------------------<cut here>----------------------------------------
'
'       **************************************************************
'       ***   Demonstration of real-time graphing of 2 parameters  ***
'       ***              with time on the X-axis                   ***
'       ***                                                        ***
'       ***              by   James N. Broadbent                   ***
'       ***                    May 17, 1996                        ***
'       **************************************************************

SCREEN 12
DIM YGRAPH!(8, 2)

'       YGRAPH!(8,2)  is the matrix that holds the vales that make up
'                     the Y-axis for the 3 "ZOOM" levels.
'       "ZOOM" levels are magnification levels of full scale on the
'                     Y-axis. They can be any ratio desired but in this
'                     example the ZOOMFACTORS are 100%, 200% and 400%
'                     magnification.

'       NOTE: This is not a general graphing routine but will have
'             a min "Y" value set to 0.0 and max "Y" value of 100000
START:
        CLS
        LOCATE 9, 20
        LINE INPUT "Enter the Maximum Value for Y-axis: "; a$
        IF VAL(a$) <= 0! OR VAL(a$) > 100000 THEN
           LOCATE 11, 20
           BEEP
           PRINT "Please enter a number from 0 to 100,000!"
           SLEEP 1
           GOTO START
           END IF
        MAXY! = VAL(a$)

'       Graph co-ordinates for ZOOM feature and drawing the Y-axis

'       set min and max for window statement for each zoom level
        YGRAPH!(0, 0) = MAXY! * 1.05
        YGRAPH!(0, 1) = MAXY! * 1.05 / 2
        YGRAPH!(0, 2) = MAXY! * 1.05 / 4
        YGRAPH!(1, 0) = -.05 * MAXY!
        YGRAPH!(1, 1) = -.05 * MAXY! / 2
        YGRAPH!(1, 2) = -.05 * MAXY! / 4
      
'       set Y-axis and X-axis overall coordinates
        FOR I% = 0 TO 2
           YGRAPH!(2, I%) = YGRAPH!(1, I%) * .95
        NEXT I%
       
'       set major Y-axis tick marks
        k! = 1.25
        FOR j% = 3 TO 7
           k! = k! - .25
           FOR I% = 0 TO 2
              l% = I% ^ 2 + 1
              IF l% = 5 THEN l% = 4
              YGRAPH!(j%, I%) = MAXY! * k! / l%
           NEXT I%
        NEXT j%

'       Bottom screen clearing coordinates
        YGRAPH!(8, 0) = -.045 * MAXY!
        YGRAPH!(8, 1) = -.045 * MAXY! / 2
        YGRAPH!(8, 2) = -.045 * MAXY! / 4

'       Create the viewport for the Graph
        VIEW (1, 100)-(637, 440), 1, 9

'       set up the initial graph
        z% = 0                       ' ZOOMFACTOR element
        GOSUB graphcoordinates
        COLOR 14
        LOCATE 1, 10
        PRINT "Demonstration of a Continuous Real-Time Plot of 2 Variables";
        LOCATE 2, 30
        PRINT "by James N. Broadbent"
        ZF% = z% ^ 2 + 100
        IF ZF% > 450 THEN ZF% = 400
        r! = MAXY! / ZF% * 100
        COLOR 11
        LOCATE 4, 5
        PRINT "Y - Axis Range : 0 to"; r!; " units"
        COLOR 3
        LOCATE 5, 5
        PRINT "ZOOM Factor (%):"; ZF%
        COLOR 13
        LOCATE 4, 50
        PRINT "Value of Variable #1:";
        COLOR 15
        LOCATE 5, 50
        PRINT "Value of Variable #2:";
        LOCATE 29, 5
        PRINT "Press <X> to Exit";
        LOCATE 29, 57
        PRINT "Press <Z> to ZOOM";
        COLOR 14
        LOCATE 29, 34
        PRINT "TIME (SECS):";

'      initialize these following parameters for real-time graph
       X! = -1                         ' X axis data position
       STARTTIME! = TIMER              ' for time display = 0
       TRIGGER! = TIMER                ' to trigger data display
       
'       Generate simulated values and plot to the screen

       DO
          a$ = UCASE$(INKEY$)
          IF TIMER - TRIGGER! > .2 THEN
             GOSUB YDATA
             TRIGGER! = TIMER
             COLOR 14
             LOCATE 29, 46
             PRINT INT(TIMER - STARTTIME);
             IF TIMER - STARTIME > 99999 THEN STARTIME = TIMER
             END IF
          IF a$ = "Z" THEN
             z% = z% + 1
             SELECT CASE z%
                CASE 0
                   ZF% = 100
                CASE 1
                   ZF% = 200
                CASE 2
                   ZF% = 400
                CASE 3
                   ZF% = 100
                   z% = 0
             END SELECT

          r! = MAXY! / ZF% * 100
          COLOR 11
          LOCATE 4, 5
          PRINT SPC(35);
          LOCATE 4, 5
          PRINT "Y - Axis Range : 0 to"; r!; " units";
          VIEW (1, 100)-(637, 440), 1, 9
          GOSUB graphcoordinates
          COLOR 3
          LOCATE 5, 22
          COLOR 13
          PRINT ZF%;  ' Display the ZOOM
          END IF

       LOOP WHILE a$ <> "X"

       VIEW
       CLS 0
       COLOR 15
       LOCATE 9, 29
       PRINT "Start again (Y/N)?"
       DO
          a$ = UCASE$(INKEY$)
          IF a$ = "Y" THEN GOTO START
          IF a$ = "N" THEN END
       LOOP

graphcoordinates:

'       Set up Graph coordinates

       WINDOW (-15, YGRAPH!(1, z%))-(620, YGRAPH!(0, z%))
       LINE (-8, YGRAPH!(2, z%))-(600, YGRAPH!(2, z%)), 11
       LINE (-5, YGRAPH!(3, z%))-(-5, YGRAPH!(1, z%)), 11
       LINE (-10, YGRAPH!(3, z%))-(-5, YGRAPH!(3, z%)), 11
       LINE (-7, YGRAPH!(4, z%))-(-5, YGRAPH!(4, z%)), 11
       LINE (-10, YGRAPH!(5, z%))-(-5, YGRAPH!(5, z%)), 11
       LINE (-7, YGRAPH!(6, z%))-(-5, YGRAPH!(6, z%)), 11

'      Next line is the "TICK" indicating ZERO
       LINE (-10, YGRAPH!(7, z%))-(-5, YGRAPH!(7, z%)), 13

       RETURN

YDATA:

'       Generate simulated vales for variables 1 and 2

       IY1! = (MAXY! * RND(1) - MAXY! / 2) * .1
       IY2! = (MAXY! * RND(1) - MAXY! / 2) * .1
       IF Y1! + IY1! > MAXY! OR Y1! + IY1! < 0 THEN
          Y1! = Y1! - IY1!
          ELSE
          Y1! = Y1! + IY1!
          END IF
       IF Y2! + IY2! > MAXY! OR Y2! + IY2! < 0 THEN
          Y2! = Y2! - IY2!
          ELSE
          Y2! = Y2! + IY2!
          END IF
       LOCATE 4, 72
       PRINT "       ";
       LOCATE 5, 72
       PRINT "       ";
       SELECT CASE MAXY!
          CASE IS > 10000
             COLOR 13
             LOCATE 4, 72
             PRINT USING "#####"; Y1!
             COLOR 15
             LOCATE 5, 72
             PRINT USING "#####"; Y2!
          CASE 1000 TO 9999
             COLOR 13
             LOCATE 4, 72
             PRINT USING "####.#"; Y1!
             COLOR 15
             LOCATE 5, 72
             PRINT USING "######.#"; Y2!
          CASE 100 TO 999
             COLOR 13
             LOCATE 4, 72
             PRINT USING "###.##"; Y1!
             COLOR 15
             LOCATE 5, 72
             PRINT USING "###.##"; Y2!
          CASE 10 TO 99
             COLOR 13
             LOCATE 4, 72
             PRINT USING "##.###"; Y1!
             COLOR 15
             LOCATE 5, 72
             PRINT USING "##.###"; Y2!
          CASE 1! TO 9.9
             COLOR 13
             LOCATE 4, 72
             PRINT USING "#.####"; Y1!
             COLOR 15
             LOCATE 5, 72
             PRINT USING "#.####"; Y2!
       END SELECT
       X! = X! + 1
       
'       plotting the variable values     
       IF X! > 600 THEN
          X! = 0
          PSET (X!, Y1!), 13
          PSET (X!, Y2!), 15
          ELSE
          LINE (X! - 1, LY1!)-(X!, Y1!), 13
          LINE (X! - 1, LY2!)-(X!, Y2!), 15
       END IF
       
'       compute and establish data clearing block          
       
       IF X! > 559 THEN
          XL! = X! - 560
          ELSE
          XL! = X! + 41
          END IF
       LINE (XL! - 1, YGRAPH!(8, z%))-(XL! + 1, YGRAPH!(0, z%)), 1, BF
       
'       save last values of each variable         
       LY1! = Y1!: LY2! = Y2!
       RETURN
'
'----------------------------< cut here >-------------------------
'
'There it is....not the tightest code I've written but it works fine
'so hope it is clear enough for you to use.
'
'Regards......Jim
'--- Blue Wave/Max v2.12 [NR]
' * Origin: RASCAL BBS [Calgary, Alberta - (403)686-2550] (1:134/122)

                                                                                
                                  

