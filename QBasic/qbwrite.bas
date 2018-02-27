'
'                       ษออออออออออออออออออออออออออออป
'                       บ ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐบÞ
'                       บ ฐฐ WELCOME TO QB-WRITE! ฐฐฐบÞ
'                       บ ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐบÞ
'                       ศออออออออออออออออออออออออออออผÞ
'                         ฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
'
'       Athough this code is not the prettiest, it does get the job
'       done. If you like, change it to suit your needs. Just don't
'       Share the altered code.
'
'       This is a stand alone monochrome version, a color version is
'       available. For more information and helpful hints, run this
'       program, and load the "README1.QBW" file that came in the "zip".
'
'                                        
'  ** Please read WARNING on line 58 **                 more...
'
'       Your first task to attempt with QBW could be to print up
'       the following key assignments:
'              [Esc]           Exit Menu, Option, or Program
'              [F1]            Highlight HelpBar
'              [F2]            Activate Print Menu PopUp
'              [F3]            Activate File Menu PopUp
'              [F4]            Highlight More HelpBar
'              [F5]            Erase and start new doc.
'              [F6]            Printer Setup PopUp
'              [F7]            Clear Printer to default setup
'              [F8]            Print current doc.
'              [F9]            Print registration form
'              [F10]           Quick Save
'              [F11]           Save As...
'              [F12]           Open a file
'              [Shift]+[F12]   About... PopUp
'                                                       more...
'
'
'       The rest of the keys you should be familiar with, BUT there
'       are some minor differences.
'
'               [BackSpace]     Will only  backup to the top of the screen
'               [Delete]        Will only "pull" text from the same line
'               [Home]          Returns to first character on the line
'               [End]           Will goto column 80 on some files
'               [Enter]         Behaves only as a carriage return, and
'                               will not "bump" text. Also, press [Enter]
'                               to add a "page" to lengthen the doc.
'               [PgUp]&[PgDwn]  Only work when there's a page to go to.
'               [other]         Keystrokes not used will receive a tone
'
'
'
'                                                       more...
'      ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'      บ    WARNING !   WARNING !   WARNING !   WARNING !   WARNING !    บ
'      ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
'       This program trys to behave much like a text editor, but be
'       CAREFUL!!! The files it creates are not compatable with standard
'       ASCII text files. (Every 81st  character is a <CrRtn>).
'       IF YOU DECIDE TO OPEN AN ASCII TEXT FILE, PLEASE BE SURE NOT
'       TO SAVE IT UNDER ITS OWN NAME. (Once a file is opened, the [F10]
'       QuickSave feature is available). QBW won't destroy a text file,
'       but the extra spaces and carriage returns will make it useless to
'       to most other editors. The work envolved in repairing the file
'       will be most tedious.    (QBW files are ANSI TEXT FILES)
'      ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'      บ *  The above statement is for my protection as well as yours. * บ
'      บ This program carries NO warranty expressed or implied! The user บ
'      บ agrees to accept this condition upon acceptance of this program.บ
'      ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
'                                                       more...
'      Limitations:
'
'       QBWrite is a BASIC program, so its not going to be a full
'       functioning word processor. It's more like an editor.
'       I found QBW's limitations on my puny computer, (486sx25/8mg),
'       to be around 12,600 characters; that's about 158 lines or
'       7 1/2 screens. (@80 characters/line). That should be more
'       than enough space to create a memo or two.
'
'      Requirements:
'
'       Obviously, you need QBasic to run QBWrite, but you shouldn't
'       need a color monitor or very much RAM. I altered this version
'       (ver 2.2), so I could run it on my Tandy 1500 HD.(Nec30 w/640k
'       and CGA mono). UGH! I have a full color version (1.1), but it
'       requires at least EGA/VGA, so it can display 80 x 43 text.
'
'                                                       more...
'
'       Although the comments in the code are sparse, I tried to use
'       variable and line label names that would indicate some kind of
'       purpose. Don't worry if looks like spaghetti, darn few basic
'       programs that I've viewed looked all that much better.
'          Go ahead, use it, parse it, dice it, slice it , whatever.
'       Just register it. Also let me know if it crashes and how;
'       we all must learn from our mistakes...
'                                              - LS alias STEELCHARM
'
'       P.S. - Registering gets you the Color/EGA version too. Send comments
'              or suggestions to:
'      ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'      บ   EMail me at AOL;       Register         UWLabs c/o L.Schramm  บ
'      บ   STEELCHARM@AOL.COM        via           Rte.1 Box 6-A         บ
'      บ                         Snail Mail:       Hiddenite, NC 28636   บ
'      ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
'                [Development Donations cheerfully accepted]
' ---------------------------------[ End ]-------------------------------------
DEFINT A-Z
    DECLARE SUB SaveIt (KeyPress)
    COMMON SHARED Npath$, NewFile, KeyPress, PageNo, NoOfPages
    COMMON SHARED FFName$, LastKey
    COMMON SHARED FileLine, FLNo, FileMode, A$, Dpath$, InsertMode
    CLEAR , , 12000
    DIM SHARED TmpDoc$(LineNo)
    DIM SHARED Doc$(LineNo)
    DIM SHARED Text$(row)
SCREEN 0: COLOR 9, 1: CLS
BG$ = "UWLabs "
BG$ = BG$ + BG$ + BG$ + BG$ + BG$
BG$ = BG$ + BG$ + BG$ + BG$
BG$ = BG$ + BG$ + BG$ + BG$
b$ = STRING$(40, " ")
c1$ = CHR$(201) + STRING$(38, 205) + CHR$(187)
c2$ = CHR$(186) + STRING$(38, " ") + CHR$(186)
c3$ = CHR$(200) + STRING$(38, 205) + CHR$(188)
LOCATE 3, 1
 PRINT BG$
 PRINT BG$
 PRINT BG$
COLOR 1, 7
    FOR x = 10 TO 17
        LOCATE x, 22: PRINT b$
     NEXT x
    COLOR 4, 7
    LOCATE 9, 20: PRINT c1$
    FOR x = 10 TO 15
        LOCATE x, 20: PRINT c2$
     NEXT x
    LOCATE 16, 20: PRINT c3$
    COLOR 7, 7
    FOR x = 10 TO 15
        LOCATE x, 22: PRINT STRING$(36, " ")
     NEXT x

COLOR 15, 7
LOCATE 11, 30: PRINT "QBWrite version 2.2"
COLOR 8: LOCATE 12, 31: PRINT "(8Ox25 Text ver.)"
COLOR 0: LOCATE 13, 26: PRINT "This product was created at:"
COLOR 1: LOCATE 14, 29: PRINT "Under"
COLOR 9: LOCATE 14, 34: PRINT "WARE"
COLOR 11: LOCATE 14, 39: PRINT "Labs."
COLOR 15: LOCATE 14, 45: PRINT "(1995)"
COLOR 8, 0: LOCATE 10, 60: PRINT "ab"
LOCATE 11, 60: PRINT " U": LOCATE 12, 60: PRINT "la"
LOCATE 13, 60: PRINT "s ": LOCATE 14, 60: PRINT "WL"
LOCATE 15, 60: PRINT "bs": LOCATE 16, 60: PRINT "UW"
LOCATE 17, 22, 0: PRINT "UWLabs UWLabs UWLabs UWLabs UWLabs UWLab"
COLOR 31, 1
StartTime = VAL(RIGHT$(TIME$, 2)) + 70
COLOR 15
SLEEP 3
'--------------------------------------|
'      -- Clear KeyBoard Buffer --     |        Use this when programs
          DO WHILE INKEY$ <> ""      ' |        are chained or run from
                LOOP                 ' |        another QB program.
'--------------------------------------|        to prevent keyboard buffer
    PageNo = 1                       '          from "carrying" keystrokes
    NoOfPages = 1
    t5$ = STRING$(31, " ")
    REDIM TmpDoc$(32)
    REDIM Doc$(32)
    ON TIMER(15) GOSUB SSaver
ReNewIt:
    REDIM Text$(25)            'only displays 21 lines of text
    row = 3                    'row position
    col = 1                    'column position
    BackUp = 0             'flags backspace position 0 lets cursor goto pos 1
                               '      1 lets cursor goto previous line end
    InsertMode = 0             '0 = overstrike  1 = insert
    IMode$ = "Overstrike"      'string for status bar
    FFName$ = "Untitled.QBW"        'No File Title

    KHome$ = CHR$(0) + CHR$(71)     'Define special keys
     KEnd$ = CHR$(0) + CHR$(79)
     KDel$ = CHR$(0) + CHR$(83)
    KPgUp$ = CHR$(0) + CHR$(73)
    KPgDn$ = CHR$(0) + CHR$(81)
      KUp$ = CHR$(0) + CHR$(72)
      KDn$ = CHR$(0) + CHR$(80)
     KLft$ = CHR$(0) + CHR$(75)
     KRit$ = CHR$(0) + CHR$(77)
     KIns$ = CHR$(0) + CHR$(82)
      KF1$ = CHR$(0) + CHR$(59)
      KF2$ = CHR$(0) + CHR$(60)
      KF3$ = CHR$(0) + CHR$(61)
      KF4$ = CHR$(0) + CHR$(62)
      KF5$ = CHR$(0) + CHR$(63)
      KF6$ = CHR$(0) + CHR$(64)
      KF7$ = CHR$(0) + CHR$(65)
      KF8$ = CHR$(0) + CHR$(66)
      KF9$ = CHR$(0) + CHR$(67)
     KF10$ = CHR$(0) + CHR$(68)
     KF11$ = CHR$(0) + CHR$(133)
     KF12$ = CHR$(0) + CHR$(134)
  ON ERROR GOTO Trapper
GOTO InitScreen
'------------------------------------------------
TimeUpdate:
     hour = VAL(LEFT$(TIME$, 2))
        IF hour >= 12 THEN p$ = "pm" ELSE p$ = "am"
        IF hour > 12 THEN hour = hour - 12
        IF hour = 0 THEN hour = 12
        hour$ = STR$(hour)
     minute$ = MID$(TIME$, 4, 2)
     Now$ = hour$ + ":" + minute$ + " " + p$          'format 24 hours
 COLOR 8, 15                                          ' to 12 w/am pm
 LOCATE 1, 15, 0: PRINT Now$
 COLOR 7, 0
RETURN
'------------------------------------------------
'       --- Initialize screen ---
InitScreen:
   CLS : SCREEN 0:  KEY OFF
  
   PALETTE 0, 63        'For reverse mono or lest than 64k video
   PALETTE 7, 0         'remove these three PALETTE statements.
   PALETTE 8, 7         ' <----------------------<<<
  
   COLOR 7, 0: CLS
   COLOR 0, 7
   LOCATE 25, 1: PRINT "                         QBWrite ver 2.2 for Mono (1995)                        "
   LOCATE 1, 1: PRINT STRING$(160, " ")
   COLOR 8, 7
   LOCATE 1, 3: PRINT DATE$
GOSUB TimeUpdate
   COLOR 8, 7
   LOCATE 1, 60: PRINT " Press [Esc] to Exit "
LOCATE 3, 1, 0
COLOR 7, 0
IF FirstTime = 1 THEN GOTO SkipDoc
FirstTime = 1
PRINT "                      "
PRINT "                        ษออออออออออออออออออออออออออออป"
PRINT "                        บ ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐบÞ"
PRINT "                        บ ฐฐ WELCOME TO QB-WRITE! ฐฐฐบÞ"
PRINT "                        บ ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐบÞ"
PRINT "                        ศออออออออออออออออออออออออออออผÞ"
PRINT "                          ฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿"
PRINT "      Please take time to read `ReadMe1.QBW' & `ReadMe2.QBW' before using"
PRINT "      this program.  To load press [F12] then enter the path and README1."
PRINT "     "
PRINT "  ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
PRINT "  บ NOTE: BEFORE saving any files with QBW please note that it uses a random บ"
PRINT "  บ file mode with carriage returns placed every 80 characters, this format  บ"
PRINT "  บ allows QBWrite to IMPORT ASCII files, BUT WILL NOT WRITE THEM!!! Please  บ"
PRINT "  บ REMEMBER to save all files with the *.QBW extension so there will be no  บ"
PRINT "  บ mistaking them for ascii files. (REMEMBER OPEN ANY, SAVE AS  *.QBW).     บ"
PRINT "  ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
PRINT "                 ษอออออออออออออออออออออออออออออออออออออออออออออป"
PRINT "                 บ Press any key to clear this text and begin. บ"
PRINT "                 ศอออออออออออออออออออออออออออออออออออออออออออออผ"
PRINT ""
SkipDoc:
DO WHILE INKEY$ <> "": LOOP
DO WHILE INKEY$ = "": LOOP
 FOR x = 3 TO 23
        LOCATE x, 1, 0: PRINT STRING$(80, " ")
 NEXT x

 TIMER ON          ' <======================<<< here's the timer
 LOCATE 3, 1, 1, 1, 31
'------------------------------------------------
'               ----- Key input loop ---
 GOSUB StatusBar
EnterDo:                              'ALL the text work is done
 COLOR 7, 0                          'inside this loop.
    DO
        A$ = INKEY$
        IF A$ = CHR$(0) + CHR$(84) THEN A$ = "": GOSUB UWPopUp
        IF InsertMode = 1 THEN LOCATE row, col, 1, 7, 31
        IF InsertMode = 0 THEN LOCATE row, col, 1, 1, 31
        IF A$ = CHR$(27) THEN EXIT DO
     IF A$ <> "" THEN GOSUB CheckKeys: GOSUB StatusBar: LastTime = 0
    LOOP
 GOTO EndItNow
'---------------------------------------------------
'    --- UnderWARE Labs Pop-Up ---
UWPopUp:
 COLOR 0, 15
 LOCATE 6, 25, 0: PRINT "ษอออออออออออออออออออออออออออออป"
 FOR x = 7 TO 16
        LOCATE x, 25, 0: PRINT "บ                             บ"
 NEXT x
 LOCATE x, 25, 0: PRINT "ศอออออออออออออออออออออออออออออผ"
 COLOR 0: LOCATE 7, 32, 0: PRINT " QBWrite ver 2.2 "
 COLOR 7: LOCATE 9, 28, 0: PRINT "To register this version,"
 LOCATE 10, 35: PRINT "send $4 to:"
 COLOR 8: LOCATE 11, 33: PRINT "UnderWARE Labs"
 LOCATE 12, 33: PRINT "c/o L.Schramm"
 LOCATE 13, 33: PRINT "Rte 1 Box 6-A"
 LOCATE 14, 33: PRINT "Hiddenite, NC 28636"
 COLOR 7: LOCATE 15, 27: PRINT "Send comments & suggestions"
 LOCATE 16, 29: PRINT "to: "
 COLOR 0: LOCATE 16, 33, 0: PRINT "STEELCHARM@AOL.COM"
    DO
     IF INKEY$ <> "" THEN EXIT DO
    LOOP
 COLOR 7, 0
 FOR x = 6 TO 17
        LOCATE x, 25, 0: PRINT STRING$(31, " ")
 NEXT x
 FOR x = 6 TO 17
        LOCATE x, 1, 0: PRINT Text$(x)
 NEXT x
RETURN
'---------------------------------------------------
'            --- Print Menu ---
PrintMenu:
 COLOR 8, 7
 LOCATE 6, 25, 0: PRINT "ษอออออออออออออออออออออออออออออป"
 FOR x = 7 TO 13
        LOCATE x, 25, 0: PRINT "บ                             บ"
 NEXT x
 LOCATE x, 25, 0: PRINT "ศอออออออออออออออออออออออออออออผ"
 COLOR 0: LOCATE 7, 35: PRINT "Print Menu"
 COLOR 8
 LOCATE 9, 27: PRINT "[F6] = Set Up Printer"
 LOCATE 10, 27: PRINT "[F7] = Clear Printer Set Up"
 LOCATE 11, 27: PRINT "[F8] = PRINT NOW"
 LOCATE 12, 27: PRINT "[F9] = Print Reg. Form"
 LOCATE 13, 27, 0: PRINT "[Esc]= Clear This Menu"
    DO
        A$ = INKEY$
        IF A$ = KF6$ THEN : LastTime = 0: GOTO SetUpPrn
        IF A$ = KF8$ THEN : LastTime = 0: GOSUB PrintTheDoc
        IF A$ = KF9$ THEN : LastTime = 0: GOTO PrnRegForm
        IF A$ = CHR$(27) THEN : LastTime = 0: EXIT DO
        IF A$ = KF7$ THEN : LastTime = 0: GOTO ClearSetUp
    LOOP
 COLOR 7, 0
 FOR x = 6 TO 14
        LOCATE x, 25, 0: PRINT STRING$(31, " ")
 NEXT x
 FOR x = 6 TO 14
        LOCATE x, 1, 0: PRINT Text$(x)
 NEXT x
RETURN
'--------------------------------------------------
'       -- Clear Printer --
ClearSetUp:
  COLOR 7, 0
  FOR x = 6 TO 14
   LOCATE x, 1, 0: PRINT STRING$(80, " ")
   LOCATE x, 1, 0: PRINT Text$(x)
  NEXT x
  COLOR 8, 15
  LOCATE 9, 25, 0: PRINT "ษอออออออออออออออออออออออออออออป"
  FOR x = 10 TO 12
   LOCATE x, 25, 0: PRINT "บ                             บ"
  NEXT x
  LOCATE 13, 25, 0: PRINT "ศอออออออออออออออออออออออออออออผ"
  COLOR 0
  LOCATE 11, 31, 0: PRINT "Resetting printer..."
        LPRINT CHR$(27); "W"; CHR$(0);
        LPRINT CHR$(27); CHR$(91); CHR$(64); CHR$(4); CHR$(0); CHR$(0); CHR$(0); CHR$(17); CHR$(1);
        LPRINT CHR$(27); "I"; CHR$(0);
  FOR x = 1 TO 3
    LPRINT CHR$(27); CHR$(7);
    SLEEP 1
  NEXT x
  COLOR 7, 0
  FOR x = 6 TO 14
    LOCATE x, 1: PRINT STRING$(80, " ")
    LOCATE x, 1, 0: PRINT Text$(x)
  NEXT x
 LastTime = 0
 DO WHILE INKEY$ <> "": LOOP
RETURN
'--------------------------------------------------
'           -- Set Up IBM-Type Printers --
SetUpPrn:
 COLOR 8, 15
   LOCATE 6, 25, 0: PRINT "ษอออออออออออออออออออออออออออออป"
   FOR x = 7 TO 13
     LOCATE x, 25: PRINT "บ                             บ"
   NEXT x
   LOCATE 14, 25, 0: PRINT "ศอออออออออออออออออออออออออออออผ"
 COLOR 0: LOCATE 7, 32, 0: PRINT "IBM PRINTER SETUP"
 COLOR 8
 LOCATE 9, 27: PRINT " 1 = Near Letter Quality   "
 LOCATE 10, 27: PRINT " 2 = Single High, Dbl wide "
 LOCATE 11, 27: PRINT " 3 = Double High, Sgl wide "
 LOCATE 12, 27: PRINT " 4 = Double high, Dbl wide "
 LOCATE 13, 27, 0: PRINT "  [Esc] = Clear This Menu  "
    DO
        A$ = INKEY$
        IF A$ = "1" THEN
                LPRINT CHR$(27); CHR$(73); CHR$(2);
                LPRINT CHR$(7);
                PrintMode = 0: LastTime = 0
                EXIT DO
                END IF
        IF A$ = "2" THEN
                LPRINT CHR$(27); CHR$(87); CHR$(1);
                LPRINT CHR$(7);
                PrintMode = 0: LastTime = 0
                EXIT DO
                END IF
        IF A$ = "3" THEN
                LPRINT CHR$(27); CHR$(91); CHR$(64); CHR$(4);
                LPRINT CHR$(0); CHR$(0); CHR$(0); CHR$(34); CHR$(1);
                LPRINT CHR$(7);
                PrintMode = 2: LastTime = 0
                EXIT DO
                END IF
        IF A$ = "4" THEN
                LPRINT CHR$(27); CHR$(91); CHR$(64); CHR$(4);
                LPRINT CHR$(0); CHR$(0); CHR$(0); CHR$(34); CHR$(2);
                LPRINT CHR$(7);
                PrintMode = 2: LastTime = 0
                EXIT DO
                END IF
        IF A$ = CHR$(27) THEN EXIT DO
    LOOP
 COLOR 7, 0
  FOR x = 6 TO 14
    LOCATE x, 1: PRINT STRING$(80, " ")
    LOCATE x, 1, 0: PRINT Text$(x)
  NEXT x
RETURN
'--------------------------------------------------
'            -- print reg form --
PrnRegForm:
  COLOR 8, 15
  LOCATE 8, 25, 0: PRINT "ษอออออออออออออออออออออออออออออป"
  FOR x = 9 TO 11
        LOCATE x, 25: PRINT "บ                             บ"
  NEXT x
  LOCATE 12, 25, 0: PRINT "ศอออออออออออออออออออออออออออออผ"
 COLOR 16
  LOCATE 10, 32, 0: PRINT "Printing Form..."
 OPEN "c:\uwlabs\regform1.txt" FOR INPUT AS #1
     DO WHILE NOT EOF(1)
     LINE INPUT #1, p$
     LPRINT p$
     LOOP
 CLOSE
   SLEEP 2
 COLOR 7, 0
 FOR x = 8 TO 12
        LOCATE x, 1: PRINT STRING$(80, " ")
        LOCATE x, 1, 0: PRINT Text$(x)
 NEXT x
  LastTime = 0
RETURN
'---------------------------------------------------
'               -- Print Document --
PrintTheDoc:
 COLOR 8, 15
       LOCATE 7, 20, 0: PRINT CHR$(201); STRING$(38, 205); CHR$(187)
        FOR x = 8 TO 10
          LOCATE x, 20: PRINT CHR$(186); STRING$(38, " "); CHR$(186)
        NEXT x
       LOCATE 11, 20: PRINT CHR$(200); STRING$(38, 205); CHR$(188)
 COLOR 0: LOCATE 9, 23, 0: PRINT "Printing document, please wait..."
    LastTime = 0
    SLEEP 1
    LPRINT
        FOR x = 1 TO UBOUND(Doc$, 1)
                LPRINT Doc$(x)
                IF PrintMode = 2 THEN
                 SELECT CASE x
                        CASE 31, 62, 93, 124, 155, 186, 217, 248
                        LPRINT : LPRINT
                        CASE 279, 310, 341, 372, 403, 434, 465, 496, 527
                        LPRINT : LPRINT
                        CASE 558, 589, 620, 651, 682, 713, 744, 775
                        LPRINT : LPRINT
                   END SELECT
                 END IF
                 IF PrintMode <> 2 THEN
                   SELECT CASE x
                        CASE 62, 124, 186, 248
                        LPRINT : LPRINT : LPRINT : LPRINT
                        CASE 310, 372, 434, 496
                        LPRINT : LPRINT : LPRINT : LPRINT
                        CASE 558, 620, 682, 744
                        LPRINT : LPRINT : LPRINT : LPRINT
                   END SELECT
                 END IF
        NEXT x
        COLOR 7, 0
        FOR x = 6 TO 14
          LOCATE x, 20: PRINT STRING$(40, " ")
          LOCATE x, 1, 0: PRINT Text$(x)
        NEXT x
  LastTime = 0
RETURN
'---------------------------------------------------
'               ----- Select keystroke ----
CheckKeys:
  SELECT CASE A$
        CASE KF1$
                GOTO HelpBar
        CASE KF2$
                GOSUB PrintMenu: RETURN 'Print Menu
        CASE KF6$
                GOSUB SetUpPrn: RETURN
        CASE KF7$
                GOSUB ClearSetUp: RETURN
        CASE KF8$
                GOSUB PrintTheDoc: RETURN
        CASE KF9$
                GOSUB PrnRegForm: RETURN
        CASE KF3$
                CALL SaveIt(3)
                FOR x = 5 TO 13
                 LOCATE x, 1, 0: PRINT Text$(x)
                NEXT x
                  IF NewFile = 1 THEN NewFile = 0: GOTO ReNewIt
                RETURN
        CASE KF5$
                CALL SaveIt(5)
                FOR x = 5 TO 13
                 LOCATE x, 1, 0: PRINT Text$(x)
                NEXT x
                IF NewFile = 1 THEN
                        NewFile = 0: PageNo = 1
                        FLNo = 1: FileMode = 0: PrintMode = 0
                        NoOfPages = 1: REDIM Doc$(32): REDIM Text$(32)
                        GOTO ReNewIt
                END IF
                RETURN
        CASE KF10$
                CALL SaveIt(10)
                FOR x = 5 TO 13
                 LOCATE x, 1, 0: PRINT Text$(x)
                NEXT x
                RETURN
        CASE KF11$
                CALL SaveIt(11)
                FOR x = 5 TO 13
                 LOCATE x, 1, 0: PRINT Text$(x)
                NEXT x
                RETURN
        CASE KF12$
                CALL SaveIt(12)
                FOR x = 5 TO 13
                 LOCATE x, 1, 0: PRINT Text$(x)
                NEXT x
                RETURN
        CASE KF4$
                GOTO MoreHelp
        CASE KPgUp$
                IF PageNo > 1 THEN
                   LOCATE 3, 1, 0
                   PageNo = PageNo - 1
                   FOR x = 3 TO 23: PRINT STRING$(80, " "): NEXT x
                   FileLine = ((PageNo - 1) * 21) + 1
                   FOR x = 3 TO 23
                        Text$(x) = Doc$(FileLine)
                        FileLine = FileLine + 1
                        LOCATE x, 1, 0: PRINT Text$(x)
                   NEXT x
                ELSE FOR t = 1800 TO 1000 STEP -600: SOUND t, .5: NEXT t
                END IF
                RETURN
        CASE KPgDn$
                IF PageNo < NoOfPages THEN
                   LOCATE 3, 1, 0
                   FOR x = 3 TO 23: PRINT STRING$(80, " "): NEXT x
                   FileLine = (PageNo * 21) + 1
                   FOR x = 3 TO 23
                        Text$(x) = Doc$(FileLine)
                        FileLine = FileLine + 1
                        LOCATE x, 1, 0: PRINT Text$(x)
                   NEXT x
                   PageNo = PageNo + 1
                ELSE FOR t = 1800 TO 1000 STEP -600: SOUND t, .5: NEXT t
                END IF
                RETURN
        CASE KIns$
                IF InsertMode = 0 THEN InsertMode = 1: IMode$ = "Insert    ": RETURN
                IF InsertMode = 1 THEN InsertMode = 0: IMode$ = "Overstrike": RETURN
        CASE KDel$
                IF LEN(Text$(row)) < 1 OR col > LEN(Text$(row)) THEN
                   FOR t = 1800 TO 1000 STEP -600: SOUND t, .5: NEXT t: RETURN
                END IF
                IF col <= LEN(Text$(row)) THEN
                  b$ = LEFT$(Text$(row), col - 1)
                  c$ = RIGHT$(Text$(row), LEN(Text$(row)) - col)
                  Text$(row) = b$ + c$
                  LOCATE row, 1, 0: PRINT Text$(row); STRING$(80 - LEN(Text$(row)), " ")
                  LOCATE row, col, 0: RETURN
                END IF
        CASE KLft$
                IF col > 1 THEN
                  col = col - 1
                  LOCATE row, col, 0
                  RETURN
                END IF
        CASE KRit$
                IF col < 80 THEN
                  col = col + 1
                  LOCATE row, col, 0
                  RETURN
                END IF
        CASE KUp$
                IF row > 3 THEN
                  row = row - 1
                  LOCATE row, col, 0
                  RETURN
                END IF
        CASE KDn$
                IF row < 23 THEN
                  row = row + 1
                  LOCATE row, col, 0
                  RETURN
                END IF
        CASE KEnd$
                col = LEN(Text$(row)) + 1
                IF col = 0 THEN col = 1
                IF col > 80 THEN col = 80
                LOCATE row, col, 0
                RETURN
        CASE CHR$(13)
                IF row > 22 AND PageNo < NoOfPages THEN BEEP: RETURN
                IF row > 22 AND PageNo = NoOfPages THEN
                    REDIM TmpDoc$(FLNo + 22)
                    IF PageNo > 1 THEN
                       FOR x = 1 TO FLNo
                         TmpDoc$(x) = Doc$(x)
                       NEXT x
                    ELSE
                       FOR x = 3 TO 22
                         TmpDoc$(x - 2) = Text$(x)
                       NEXT x
                    END IF
                    count = 3
                    REDIM Doc$(FLNo + 22)
                    FOR x = 1 TO UBOUND(TmpDoc$, 1)
                        Doc$(x) = TmpDoc$(x)
                    NEXT x
                  LOCATE 3, 1, 0
                   FOR x = 3 TO 23: PRINT STRING$(80, " "): NEXT x
                   PageNo = PageNo + 1
                   NoOfPages = NoOfPages + 1
                   row = 3: col = 1
                   LOCATE row, col, 0
                     REDIM Text$(25)
                   RETURN
                END IF
                row = row + 1
                col = 1
                LOCATE row, col, 0
        CASE CHR$(9)                                    'tab
                IF col > 74 THEN FOR t = 1800 TO 1000 STEP -600: SOUND t, .5: NEXT t: RETURN
                IF col > LEN(Text$(row)) THEN
                  col = (INT(col / 5) * 5) + 5
                  Text$(row) = Text$(row) + "     "
                  Text$(row) = LEFT$(Text$(row), col - 1)
                  LOCATE row, 1, 0: PRINT Text$(row)
                  LOCATE row, col, 0
                   RETURN
                END IF
                b$ = LEFT$(Text$(row), col - 1)
                c$ = RIGHT$(Text$(row), LEN(Text$(row)) - (col - 1))
                c = 5 - (col MOD 5)
                col = (INT(col / 5) * 5) + 5
                c$ = STRING$(c, " ") + c$
                Text$(row) = b$ + c$
                IF LEN(Text$(row)) > 80 THEN Text$(row) = LEFT$(Text$(row), 80)
                LOCATE row, 1, 0: PRINT Text$(row)
                LOCATE row, col, 0
                RETURN
        CASE CHR$(8)                                    'backspace
                IF col < LEN(Text$(row)) THEN GOTO BKSPC
                col = col - 1
                IF col < 1 THEN
                        row = row - 1
                        col = LEN(Text$(row)) + 1
                        IF col = 0 THEN col = 1
                        IF col >= 80 THEN
                                Text$(row) = LEFT$(Text$(row), 79)
                                col = 80
                        END IF
                        Text$(row + 1) = ""
                        LOCATE row + 1, 1, 0: PRINT " "
                END IF
                IF row = 2 THEN row = 3: col = 1: FOR t = 1800 TO 1000 STEP -600: SOUND t, .5: NEXT t: RETURN
                IF col >= 1 THEN
                        Text$(row) = LEFT$(Text$(row), col - 1)
                END IF
                LOCATE row, 1, 0: PRINT Text$(row); " "
                LOCATE row, col, 0
        CASE KHome$
                StartPos = 0
                FOR q = 1 TO LEN(Text$(row))
                    FirstLtr = INSTR(q, Text$(row), " ")
                    IF FirstLtr <> q THEN EXIT FOR
                    StartPos = FirstLtr
                NEXT
                col = StartPos + 1
                LOCATE row, col, 0
        CASE ELSE
                IF LEN(A$) > 1 THEN
                        FOR t = 1800 TO 1000 STEP -600
                         SOUND t, .5
                        NEXT t
                        RETURN
                END IF
                IF col > LEN(Text$(row)) THEN
                  Text$(row) = Text$(row) + STRING$(col - LEN(Text$(row)) - 1, " ")
                END IF
                IF col < LEN(Text$(row)) THEN GOTO InsOvr
                Text$(row) = Text$(row) + A$
                LOCATE row, 1, 0: PRINT Text$(row)
                col = col + 1
                IF col > 80 AND row < 24 THEN col = 1: row = row + 1
                IF col > 79 AND row > 23 THEN
                        col = 23: FOR t = 1800 TO 1000 STEP -600: SOUND t, .5: NEXT t
                        Text$(row) = LEFT$(Text$(row), 79)
                        RETURN
                  END IF
                LOCATE row, col, 0
  END SELECT
  LastTime = 0
RETURN
'------------------------------
'    --- Insert or Overwrite ---
InsOvr:
  IF InsertMode = 0 THEN
        b$ = LEFT$(Text$(row), col - 1)
        c$ = RIGHT$(Text$(row), LEN(Text$(row)) - (col - 0))
        Text$(row) = b$ + A$ + c$
         LOCATE row, 1, 0: PRINT Text$(row)
         col = col + 1
         LOCATE row, col, 0
        RETURN
  END IF
  IF InsertMode = 1 THEN
        b$ = LEFT$(Text$(row), col - 1)
        c$ = RIGHT$(Text$(row), LEN(Text$(row)) - (col - 1))
        Text$(row) = b$ + A$ + c$
         IF LEN(Text$(row)) > 80 THEN Text$(row) = LEFT$(Text$(row), 80)
        LOCATE row, 1, 0: PRINT Text$(row)
        col = col + 1
        LOCATE row, col, 0
  END IF
RETURN
'-----------------------------------
'       --- Backspace from inside line ---
BKSPC:
        IF col = 1 THEN
            FOR t = 1800 TO 1000 STEP -600
               SOUND t, .5
            NEXT t
            RETURN
        END IF
        b$ = LEFT$(Text$(row), col - 2)
        c$ = RIGHT$(Text$(row), LEN(Text$(row)) - (col - 1))
        Text$(row) = b$ + c$
        LOCATE row, 1, 0: PRINT Text$(row) + " "
        col = col - 1
        LOCATE row, col, 0
GOTO EnterDo
'-------------------------------------------
'     ---- Status bar Update ------
StatusBar:
        LastTime = 0
IF Npath$ <> "" THEN FFName$ = Npath$: COLOR 0, 15: LOCATE 1, 25, 0: PRINT "                                          "
COLOR 0, 15
x = INT(40 - ((LEN(FFName$) + 11) / 2))
LOCATE 1, x, 0: PRINT "File Name: "; FFName$
FLNo = ((PageNo - 1) * 21) + (row - 2)
COLOR 8
LOCATE 2, 3, 0: PRINT "Page:"; PageNo; "/"; NoOfPages
LOCATE 2, 20, 0: PRINT "Line:"; FLNo
LOCATE 2, 32, 0: PRINT "Col:"; col
LOCATE 2, 42, 0: PRINT "Cusor: "; IMode$
LOCATE 2, 65, 0: PRINT "Line Len:"; LEN(Text$(row))
COLOR 7, 0
        Doc$(FLNo) = Text$(row)
        LOCATE row, col, 0
RETURN
'---------------------------------------------
HelpBar:
       COLOR 8, 15
       LOCATE 1, 1, 0: PRINT STRING$(160, " ")
 LOCATE 1, 8, 0: PRINT "= This Menu     [Ins]  = Insert text in line  [Del] = Delete 1 character"
 LOCATE 2, 8, 0: PRINT "= Print Options [End]  = Cursor to text end"
 LOCATE 3, 8, 0: PRINT "= File Options  [Home] = Cusor to line start      [F4] = List More...    "
 COLOR 0
 LOCATE 1, 3, 0: PRINT "[F1]": LOCATE 1, 24: PRINT "[Ins]"
 LOCATE 2, 3, 0: PRINT "[F2]": LOCATE 2, 24: PRINT "[End]"
 LOCATE 3, 1, 0: PRINT "  [F3] ": LOCATE 3, 24: PRINT "[Home]"
 LOCATE 1, 53: PRINT " [Del]": LOCATE 3, 56, 0: PRINT "  [F4]"
    DO
        A$ = INKEY$
        IF A$ <> "" THEN IF A$ = CHR$(0) + CHR$(62) THEN GOTO MoreHelp
        IF A$ = " " THEN EXIT DO
        IF A$ = CHR$(27) THEN EXIT DO
        IF A$ = CHR$(13) THEN EXIT DO
    LOOP
    LastTime = 0
  GOTO EndHelp
MoreHelp:
  COLOR 0, 15
  LOCATE 1, 1, 0: PRINT STRING$(160, " ")
  COLOR 0
  LOCATE 1, 3, 0: PRINT "Other supported keys:"
  COLOR 8
  LOCATE 2, 3: PRINT "[Backspace] = Backup 1 and delete  [Tab] = Tabs text upto 5 spaces"
  LOCATE 3, 3: PRINT "[Enter] = Carriage return          "
  COLOR 0
  LOCATE 2, 3: PRINT "[Backspace]": LOCATE 2, 38: PRINT "[Tab]"
  LOCATE 3, 1: PRINT "  [Enter]": LOCATE 3, 38, 0: PRINT "[Caps], [Shift], ["; CHR$(24);
  PRINT "], ["; CHR$(25); "], ["; CHR$(26); "], ["; CHR$(27); "]        "
    DO
        A$ = INKEY$
        IF A$ = " " THEN EXIT DO
        IF A$ = CHR$(27) THEN EXIT DO
        IF A$ = CHR$(13) THEN EXIT DO
    LOOP
EndHelp:
   COLOR 8, 15
     LOCATE 1, 1, 0: PRINT STRING$(160, " ")
   LOCATE 1, 2, 0: PRINT DATE$
   GOSUB TimeUpdate
   COLOR 8, 15
   LOCATE 1, 60: PRINT " Press [Esc] to Exit "
   GOSUB StatusBar
   LOCATE 3, 1, 0: PRINT STRING$(80, " ")
   COLOR 7, 0: LOCATE 3, 1, 0: PRINT Doc$(((PageNo - 1) * 21) + 1)
   LastTime = 0
GOTO EnterDo
'---------------------------------------------
'       --- End Program w/ optional return ---
EndItNow:
  COLOR 8, 15
  LOCATE 8, 23, 0: PRINT "ษออออออออออออออออออออออออออออออออออป"
   FOR x = 9 TO 11
        LOCATE x, 23: PRINT "บ                                  บ"
   NEXT x
  LOCATE 12, 23, 0: PRINT "ศออออออออออออออออออออออออออออออออออผ"
    COLOR 0
    LOCATE 9, 26, 0: PRINT "ALL UNSAVED WORK WILL BE LOST!"
    COLOR 8
    LOCATE 11, 28, 0: PRINT "Exit Now?  [ Yes ]  [ No  ]"
    COLOR 0
    LOCATE 11, 41, 0: PRINT "Y"
    LOCATE 11, 50, 0: PRINT "N"
    DO
        A$ = UCASE$(INKEY$)
        IF A$ = "Y" THEN GOTO QuitToDOS
        IF A$ = "N" THEN EXIT DO
        IF A$ = CHR$(27) THEN EXIT DO
    LOOP
    COLOR 7, 0
    FOR x = 7 TO 13
        LOCATE x, 1, 0: PRINT STRING$(80, " ")
        LOCATE x, 1, 0: PRINT Text$(x)
    NEXT x
  GOSUB StatusBar
  LastTime = 0
GOTO EnterDo
'------------------------ program ended --------------------------------------
QuitToDOS:
   PALETTE 0, 0         'Reset palette to default before return.
   PALETTE 7, 7
   PALETTE 8, 8
   WIDTH 80, 25: COLOR 7, 0
   CLS : TIMER OFF
        PRINT "  ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
        PRINT "  บ  Thank you for using UnderWARE Labs!   Please don't forget to register.  บ"
        PRINT "  ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
      SLEEP 3
   LOCATE 4, 1, 1
SYSTEM
'---------------------------------------------------------------------------------
'       ------------- Error Trapping Routine -----------------
Trapper:
        'LOCATE 12, 12: PRINT "Error: "; ERR
        'END
  SELECT CASE ERR
        CASE 55: CLOSE : RESUME EnterDo
        CASE 14
                a1$ = " Out of String space file  "
                a2$ = "    too large to open.     "
                a3$ = "      Press any key...     "
                GOSUB PostIt
                 GOSUB ClikToEsc:
                GOSUB ClearBox
                   Npath$ = ""
                RESUME EnterDo
       
        CASE 25
                a1$ = "Please turn printer on, and"
                a2$ = "         try again.        "
                a3$ = "       Press any key...    "
                GOSUB PostIt
                 GOSUB ClikToEsc:
                GOSUB ClearBox
                RESUME PrintMenu
        CASE 27
                a1$ = " There's no paper in the   "
                a2$ = " printer please try again. "
                a3$ = "     Press any key...      "
                GOSUB PostIt
                 GOSUB ClikToEsc:
                GOSUB ClearBox
                RESUME PrintMenu
        CASE 52, 64
                a1$ = " Bad file name, please try "
                a2$ = "           again.          "
                a3$ = "       Press any key...    "
                GOSUB PostIt
                 GOSUB ClikToEsc:
                GOSUB ClearBox
                   Npath$ = ""
                RESUME EnterDo
        CASE 53, 75, 76
                a1$ = " Cannot find file,  please "
                a2$ = " check path and directory. "
                a3$ = " try again.  [press a key] "
                GOSUB PostIt
                 GOSUB ClikToEsc:
                GOSUB ClearBox
                RESUME NEXT
        CASE 71
                a1$ = "Disk Not ready, Insert disk"
                a2$ = "and try again. Press any key"
                a3$ = "to contine, [Esc] to cacel."
                GOSUB PostIt
                 DO
                  c$ = INKEY$
                  IF c$ = CHR$(27) THEN
                        GOSUB ClearBox
                        RESUME EnterDo
                  END IF
                LOOP UNTIL INKEY$ <> ""
                  IF c$ = CHR$(27) THEN GOSUB ClearBox: RESUME EnterDo
                GOSUB ClearBox
                RESUME
        CASE 5
                IF col < 1 THEN col = 1: RESUME
                IF col > 80 THEN col = 1: RESUME
                a1$ = "This computer won't support"
                a2$ = "one of the functions,Ending"
                a3$ = " Program.   [Press a key]. "
                GOSUB ElsePost
                GOSUB ClikToEsc:
                GOTO QuitErr
        CASE ELSE
                a1$ = " Something's wrong, but I  "
                a2$ = " don't know what.  Ending  "
                a3$ = " Program.   [Press a key]. "
                GOSUB ElsePost
                 GOSUB ClikToEsc:
                GOTO QuitToDOS
  END SELECT
ClikToEsc:
        LastTime = 0
         DO WHILE INKEY$ = ""
         LOOP
        LastTime = 0
RETURN
PostIt:
  COLOR 8, 15
  LOCATE 7, 25, 0: PRINT "ษอออออออออออออออออออออออออออออป"
   FOR x = 8 TO 12
        LOCATE x, 25: PRINT "บ                             บ"
   NEXT x
  LOCATE 13, 25, 0: PRINT "ศอออออออออออออออออออออออออออออผ"
  COLOR 16: LOCATE 8, 37: PRINT "ERROR !"
  COLOR 8
  LOCATE 9, 27: PRINT a1$
  LOCATE 10, 27: PRINT a2$
  LOCATE 11, 27: PRINT a3$
  LOCATE 12, 33, 0: PRINT "Error code:"; ERR
RETURN
ElsePost:
  COLOR 7, 0
  SCREEN 0: CLS : WIDTH 80, 25
  LOCATE 9, 25, 0: PRINT "ษอออออออออออออออออออออออออออออป"
    FOR x = 10 TO 14
        LOCATE x, 25: PRINT "บ                             บ"
    NEXT x
  LOCATE 15, 25: PRINT "ศอออออออออออออออออออออออออออออผ"
  COLOR 31, 0: LOCATE 10, 37, 0: PRINT "ERROR !"
  COLOR 7
  LOCATE 11, 27: PRINT a1$
  LOCATE 12, 27: PRINT a2$
  LOCATE 13, 27: PRINT a3$
  LOCATE 14, 33, 0: PRINT "Error code:"; ERR
RETURN
ClearBox:
  COLOR 7, 0
    FOR x = 5 TO 18
      LOCATE x, 1: PRINT STRING$(80, " ")
      LOCATE x, 1, 0: PRINT Text$(x)
    NEXT x
RETURN
'------------------error ending-------------
QuitErr:
   WIDTH 80, 25: COLOR 7, 0
   CLS : TIMER OFF
PRINT "ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
PRINT "บ  Thank you for Trying UnderWARE Labs!   Please EMail for error correction.   บ"
PRINT "ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
   LOCATE 4, 1, 1
SYSTEM
'----------------------------------------------------------------------------
'               -- Screen Saver --
SSaver:
   LOCATE row, col, 0
   GOSUB TimeUpdate
   LastTime = LastTime + 15

   TimeToWait = 300       '  <----------<<<<  No. of seconds for Screen saver.

   IF LastTime < TimeToWait THEN LOCATE row, col, 1: RETURN
  
   TIMER OFF
   COLOR 0, 7
   CLS
        A = 0
        b = 7
        q = 1
        w = 1
   DO WHILE INKEY$ = ""
        x = x + q
          IF x > 70 THEN q = -1
          IF x < 2 THEN q = 1
        y = y + w
          IF y > 20 THEN w = -1
          IF y < 2 THEN w = 1
        COLOR A, b
          LOCATE y, x, 0:     PRINT "ษออออออออป"
          LOCATE y + 1, x, 0: PRINT "บ        บ"
          LOCATE y + 2, x, 0: PRINT "ศออ1995ออผ"
          COLOR 3
          LOCATE y + 1, x + 2, 0: PRINT "UWLABS"
        SLEEP 1
          COLOR b, b
          LOCATE y, x, 0:     PRINT "          "
          LOCATE y + 1, x, 0: PRINT "          "
          LOCATE y + 2, x, 0: PRINT "          "
        cnt = cnt + 1
        dnt = dnt + 1
           IF cnt > 300 THEN cnt = 1: x = x + .1: y = y + .1
           IF dnt > 30 THEN dnt = 1: SWAP A, b: COLOR A, b: CLS
   LOOP
 TIMER ON
   COLOR 0, 15
   LOCATE 25, 1: PRINT "                         QBWrite ver 2.2 for Mono (1995)                        "
   LOCATE 1, 1: PRINT STRING$(160, " ")
 COLOR 7, 8
     FOR x = 3 TO 23
        LOCATE x, 1, 0: PRINT STRING$(80, " ")
        LOCATE x, 1, 0: PRINT Text$(x)
     NEXT x
   COLOR 8, 15
   LOCATE 1, 3: PRINT DATE$
GOSUB TimeUpdate
   COLOR 8, 15
   LOCATE 1, 60: PRINT " Press [Esc] to Exit "
 LOCATE 3, 1, 0
  GOSUB StatusBar
RETURN

SUB SaveIt (KeyPress)
 LastTime = 0
 IF KeyPress = 10 AND Npath$ <> "" THEN GOTO SaveIt
  Dpath$ = "C:\UWLABS\"
  Dfile$ = "SAMPLE01.QBW"
  COLOR 8, 15
  LOCATE 5, 25, 0: PRINT "ษออออออออออออออออออออออออออออป"
    FOR x = 6 TO 12
       LOCATE x, 25: PRINT "บ                            บ"
    NEXT x
  LOCATE 13, 25: PRINT "ศออออออออออออออออออออออออออออผ"

  IF KeyPress = 3 THEN GOTO ShowFileMenu
  IF KeyPress = 5 THEN GOTO OpenNew
  IF KeyPress = 11 THEN GOTO SaveAs
  IF KeyPress = 12 THEN GOTO SaveAs

ShowFileMenu:
  COLOR 0, 15: LOCATE 6, 35: PRINT "FILE  MENU"
  COLOR 8
  LOCATE 8, 27: PRINT " [F 5] = New               "
  LOCATE 9, 27: PRINT " [F10] = Save              "
  LOCATE 10, 27: PRINT " [F11] = Save As...        "
  LOCATE 11, 27: PRINT " [F12] = Open...           "
  LOCATE 12, 27: PRINT " [Esc] = Clear This Menu   "
  UU$ = ""
  LastTime = 0
     DO
      UU$ = INKEY$
       IF UU$ = CHR$(0) + CHR$(63) THEN KeyPress = 5: GOTO OpenNew
       IF UU$ = CHR$(0) + CHR$(68) AND Npath$ <> "" THEN KeyPress = 10: GOTO SaveIt
       IF UU$ = CHR$(0) + CHR$(68) AND Npath$ = "" THEN KeyPress = 10: GOTO SaveAs
       IF UU$ = CHR$(0) + CHR$(133) THEN KeyPress = 11: GOTO SaveAs
       IF UU$ = CHR$(0) + CHR$(134) THEN KeyPress = 12: GOTO SaveAs
       IF UU$ = CHR$(27) THEN GOTO EndOfSub
    LOOP
OpenNew:
   LastTime = 0
    COLOR 0, 15
    LOCATE 6, 27: PRINT "                           "
    LOCATE 7, 27: PRINT "Any work that has not been "
    LOCATE 8, 27: PRINT "   saved will be lost!     "
    LOCATE 9, 27: PRINT "                           "
    LOCATE 10, 27: PRINT "    Open New anyway?       "
    LOCATE 11, 27: PRINT "                           "
    LOCATE 12, 27: PRINT "                           "
   COLOR 8, 7
    LOCATE 12, 29: PRINT "[ Yes ]"
    LOCATE 12, 44: PRINT "[ No  ]"
   COLOR 0, 15
    LOCATE 12, 31: PRINT "Y"
    LOCATE 12, 46: PRINT "N"
     DO
        A$ = UCASE$(INKEY$)
        IF A$ = "Y" THEN NewFile = 1: Npath$ = "": EXIT SUB
        IF A$ = CHR$(27) THEN NewFile = 0: GOTO EndOfSub
        IF A$ = "N" THEN NewFile = 0: GOTO EndOfSub
     LOOP
OpenAFile:
  LastTime = 0
  LOCATE 8, 27: PRINT "                           "
  LOCATE 9, 27: PRINT "                           "
  LOCATE 10, 27: PRINT "      Opening File...      "
  LOCATE 11, 27: PRINT "                           "
        count = 0
      NoOfPages = 0
  IF RIGHT$(Npath$, 3) = "QBW" THEN FileMode = 3
  OPEN Npath$ FOR INPUT AS #1
        DO WHILE NOT EOF(1)
          LINE INPUT #1, A$
          count = count + 1
        LOOP
  CLOSE #1
  FullPg = INT(count / 21)
  ExtraLines = count MOD 21
        IF ExtraLines > 0 THEN
           NoOfPages = FullPg + 1
         ELSE NoOfPages = FullPg
        END IF
  DocLen = (NoOfPages * 21) + 1
 REDIM Doc$(DocLen)
 REDIM Text$(25)
  count = 1
 OPEN Npath$ FOR INPUT AS #1
        DO WHILE NOT EOF(1)
           LINE INPUT #1, A$
           Doc$(count) = A$
           IF count < 22 THEN Text$(count + 2) = A$
           count = count + 1
        LOOP
 CLOSE #1
 count = 1: PageNo = 1
 COLOR 7, 0
   FOR x = 3 TO 23
        LOCATE x, 1, 0: PRINT STRING$(80, " ")
        LOCATE x, 1, 0: PRINT Text$(x)
   NEXT x
 GOTO EndOfSub
SaveAs:
  LastTime = 0
  IF KeyPress = 12 THEN Title$ = "OPEN A FILE"
  IF KeyPress = 10 OR KeyPress = 11 THEN Title$ = "SAVE A FILE"
  COLOR 0, 15: LOCATE 6, 35: PRINT Title$
  COLOR 8
  LOCATE 8, 27: PRINT "Please enter the complete  "
  LOCATE 9, 27: PRINT "path and file name below.  "
  LOCATE 10, 27: PRINT "or leave blank for the     "
  LOCATE 11, 27: PRINT "default.     (Esc to quit) "
  LOCATE 12, 27: PRINT "                           "
  COLOR 0
     LOCATE 12, 29: PRINT Dpath$ + Dfile$
ReEnter:
    COLOR 0
    TIMER OFF
    DO
      d$ = INKEY$
       IF d$ = CHR$(27) THEN GOTO EndOfSub
       IF d$ <> "" AND d$ <> CHR$(27) THEN EXIT DO
    LOOP
    TIMER ON
    LastTime = 0
    LOCATE 11, 39: PRINT "(Enter File)  "
    COLOR 0: LOCATE 12, 27: PRINT ">                       "
    LOCATE 12, 29: INPUT "", Npath$
    IF Npath$ = "" THEN Npath$ = Dpath$ + Dfile$: GOTO AcceptIt
    SS = INSTR(Npath$, "\")
     FOR g = 1 TO 15
       tempath$ = MID$(Npath$, SS + 1)
       SS = INSTR(tempath$, "\") + SS
     NEXT g
  IF SS = 0 THEN Npath$ = Dpath$ + Npath$
    dd = INSTR(Npath$, ".")
    NN = LEN(Npath$)
       IF dd = 1 THEN GOTO ReEnter
    SUF$ = RIGHT$(Npath$, NN - dd)
       IF dd = 0 THEN SUF$ = ".QBW": Npath$ = Npath$ + SUF$
   IF LEN(SUF$) > 3 AND dd > 0 THEN
        SUF$ = RIGHT$(SUF$, 3)
        Npath$ = LEFT$(Npath$, dd) + SUF$
   END IF
    IF SS > 0 THEN
        PRE$ = LEFT$(Npath$, SS)
        IF dd - SS > 8 THEN
            Part$ = MID$(Npath$, SS + 1, 8)
            Npath$ = PRE$ + Part$ + "." + SUF$
        END IF
    END IF
AcceptIt:
  COLOR 0, 15
  Npath$ = UCASE$(Npath$)
  LOCATE 12, 29: PRINT STRING$(25, " ")
  LOCATE 12, 29: PRINT Npath$
     IF KeyPress = 12 GOTO OpenAFile
  LOCATE 9, 27: PRINT "                           "
  LOCATE 10, 27: PRINT "                           "
  LOCATE 11, 27: PRINT "      Saving File...       "
  LOCATE 12, 27: PRINT "                           "
SaveIt:
  LastTime = 0
   IF FFName$ = Npath$ THEN KILL Npath$     'DEL the existing file to rewrite
       
        ' Scan last lines of file for characters & remove last blank lines to
        ' Keep files from having extra "blank" lines on the last page.

   FOR x = UBOUND(Doc$, 1) TO 1 STEP -1
        IF LEN(Doc$(x)) > 0 THEN
                DO WHILE w < 80
                 w = w + 1
                 y = INSTR(w, Doc$(x), CHR$(0))
                 IF y <> w THEN EXIT FOR
                LOOP
        END IF
   NEXT x
   IF x < 1 THEN GOTO NoSaveIt
   OPEN Npath$ FOR RANDOM AS #1           'Keeps quotes off files
        FIELD #1, 81 AS z$                '80 for text + 1 for Carriage Return
         FOR w = 1 TO x                   'Loop to last line with text on it
          NulSpaces = 80 - LEN(Doc$(w))   'Count the trailing blanks for nuls
                                          'New$ adds a C. Return and then
                                          'places nul characters after the
                                          'last character on each line.

          New$ = Doc$(w) + CHR$(13) + STRING$(NulSpaces, 0)

          LSET z$ = New$                  'left justify text
          PUT #1, w                       'place record
         NEXT w
    CLOSE       '    -- ^ The above example WILL NOT create ASCII files! ^ --
 GOTO EndOfSub
NoSaveIt:
  COLOR 0
  LOCATE 8, 27: PRINT "                           "
  LOCATE 9, 27: PRINT " There is no text to save. "
  COLOR 16
  LOCATE 11, 27: PRINT "       Returning...        "
  LOCATE 10, 27: PRINT "                           "
  LOCATE 12, 27: PRINT "                           "
  COLOR 7, 0
    SLEEP 2
   LastTime = 0
  FOR x = 3 TO 23
        LOCATE x, 1: PRINT Text$(x)
  NEXT x
EndOfSub:
  LastTime = 0
  COLOR 7, 0
  FOR x = 5 TO 13
   LOCATE x, 25: PRINT STRING$(32, " ")
  NEXT x
END SUB

