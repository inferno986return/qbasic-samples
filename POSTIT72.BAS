DEFINT A-Z
'--- PostIt! subroutines.
DECLARE SUB ParseCmdLine (cmd$, Params$(), Found%)
DECLARE SUB SepPath (a$, Drive$, path$, tName$)
DECLARE FUNCTION Decode% (oSwitch%, InSpec$, OutSpec$)
DECLARE FUNCTION Encode% (Op%, iSwitch%, cSwitch%, aSwitch%, tSwitch%, sSwitch%, pSwitch%, lSwitch%, oSwitch%, bSwitch%, InSpec$, OutSpec$)
DECLARE SUB ExpandLine (a$, Lines$(), LineLength%, NumLines%)
DECLARE FUNCTION FASC% (a$)
DECLARE FUNCTION GrabNum& (a$, Lower&, Upper&, Default&)
DECLARE FUNCTION UnTab$ (B$, TabStops%)
'--- ImportIt! subroutines.
DECLARE SUB ImportIt (BBSID$, MsgFiles$(), toname$, fromname$, TitleFile$, conference%)
DECLARE SUB CreateRep (BBSID$, ArcCommand$)
DECLARE SUB AddToRep (BBSID$, MsgFiles$(), toname$, fromname$, TitleFile$, conference%, ErrorCode$)
DECLARE SUB IIParse (cmd$, toname$, fromname$, conference%, BBSID$)
'
' --- beta test release --- released by Calvin French, August 1993 ---
'
' This SHOULD work perfectly. Please test it, tangle it, and report any
' bugs you find in it to Victor, Me, or (lastly only because he is very
' very busy), Rich.
'
' - Calvin -
'
' --------------------------------------------------------------------
'
' PostIt! v7.2 Script Encoder/Decoder-Public Domain-August 1993
' By Rich Geldreich & Victor Yiu. Many  contributions,   fixups,  and
' features by Mark H. Butler,  Quinn Tyler Jackson, and Scott Wunsch.
' QWK compatable .REP file support by Calvin French.
'
' PostIt! can  encode   any  binary   file  into  a  series  of self-
' extracting  script  files  that  can  be  reliably  distributed  on
' text-only  conferences  or  networks.   The  script  files  can  be
' extracted with  this  program,  or  with  any  Microsoft QuickBASIC
' language (DOS 5's QBASIC, QB4.5, PDS, VB-DOS) because  each  script
' contains its own small QuickBASIC decoder.
'
' PostIt!   can  also  format  QuickBASIC  source  code  suitable for
' distribution on conferences, and reconstruct source code  formatted
' by  this  program.   This  allows  QuickBASIC programmers to easily
' exchange BASIC source code without worrying about the annoying line
' length and message limitations of most networks.
'
' ImportIt!, a new part of PostIt!, can toss the output files created
' by PostIt! into a QWK compatable .REP file.
'
' New 7.2 Features:
'
' o  QWK  compatable  .REP file support  has been  included! No  more
'    importing tons of files into your reply packets via your offline
'    mail reader!
'
' New 7.1 Features:
'
' o  Totally rewritten source code!
' o  Much  more efficient  encoding algorithm (MOD 86 encoding)  with
'    a smaller and faster self extractor!
' o  Huge binary scripts now supported, up to 150k!
' o  The  script decoding & unfiltering functions  are now automated!
'    As  long  as a few  simple rules are followed (see the  notes on
'    the Decode command), no  user intervention  is needed to extract
'    multiple scripts from the same capture file.
' o  PostIt!  is  finally  a command line utility! Error codes can be
'    returned  to batch  files if  you're compiling   with  VBDOS  or
'    QBX.   Look  at the source to  find out  which error  code means
'    which.
' o  The format of PostIt!'s   message  headers has finally been well
'    thought out and (hopefully) finalized.  Although   compatibility
'    with  previous versions of PostIt!  has been sacrificed, scripts
'    created by  newer versions  of  PostIt!   should be decodable by
'    this version because of a common message header format.
'
' Explanation of Commands
'
' E = Encodes  any binary  file less than 150k into a self-extracting
'     text-only script.  If the -s  option is used with this command,
'     the entire script will be written to one output file; otherwise
'     the script will be split into multiple output files, where each
'     output file contains one message.   (Note:  Scripts created  by
'     this  command  cannot  be  extracted  by  previous  versions of
'     PostIt!.)
'
' F = Filters QuickBASIC source code for  posting  on  a  conference.
'     This  command  actually  performs  two filtering functions.  It
'     splits very long  lines  with  continuation characters (special
'     precautions are taken to ensure  quoted strings and remarks are
'     split correctly), and chops the source code into multiple files
'     so each file corresponds to one message  (unless the -s  option
'     is used).The filtered file can still be executed or compiled by
'     QuickBASIC, just as the original could.  (Note: DATA statements
'     split by filtering cannot be unsplit correctly by QB! This will
'     hopefully  be fixed  soon...   Files  filtered  by this command
'     cannot by unfiltered by previous versions of PostIt!.)
'
' D = Decodes binary/text scripts.  Multiple scripts can  be  decoded
'     from the same  input  file  with  this  function.  The decoding
'     algorithm  automatically  decides  which  method  was  used  to
'     encode the source file(binary script or source code filtering).
'
'     If  any  errors  are  encountered during decoding the script is
'     skipped  and the  partly decoded  file is deleted.
'
'     Binary and text scripts created by previous versions of PostIt!
'     cannot be decoded with this command, because of the new  header
'     format employed by this version of PostIt!.
'
'     (Notes:  Pages of a script MUST appear in increasing order.  In
'     other words, page 2 must follow page 1, page 3 must follow page
'     2, etc.  When posting  files  created  by  the E or F commands,
'     don't modify or remove the message headers because the decoding
'     algorithm expects these to indicate the beginning and ending of
'     each page.  (All message headers begin with a "'>>>" sequence.)
'     Finally, if an output file is specified on  the  command  line,
'     for  example "POSTIT D capture.txt c:\q\coolcode.zip", only the
'     specified output file  (COOLCODE.ZIP  in  the  example) will be
'     decoded if its script can be  located.   The  pathname  of  the
'     output  file  will  be  the  destination  path specified on the
'     command line.  In the  example,  the  file COOLCODE.ZIP will be
'     written to the C:\Q directory.)
'
' -Q  This  switch  will cause  PostIt!  to invoke  ImportIt!, a  new
'     feature available with version 7.2. ImportIt! will toss all the
'     files that PostIt! creates  into a QWK compatable reply  packet
'     (.REP file.) You  MUST specify  at least three  more paramaters
'     for this capability, however. They are:
'
'     [to:to_name] (optional)
'     This is the name that you would like in the "to" field (who you
'     are sending the message to.)  If it is not specified, ImportIt!
'     will substitute the name "ALL".
'
'     from:from_name
'     This is the name that you would like in the "from" field (which
'     is, more often than not, your own name)
'
'     NOTE: With  both names, if a  space is needed, use a period  in
'     the command  line (e.g.,  to:Victor.Yiu from:Calvin.French) and
'     ImportIt! will translate it to a space.
'
'     conf:conf_number
'     This is the number of the  FidoNet echomail conference that you
'     would like the  the messages to be  tossed into. This is really
'     the only very important  thing you need to remember in order to
'     use  ImportIt!  NOTE:  This  is NOT  the  NAME of the  echomail
'     conference (e.g., QUIK_BAS), but  rather the NUMBER (e.g., 32).
'     It should also  be mentioned that  sometimes this number is not
'     the same number as may appear  on your BBS's Message Base list.
'     It is suggested that  you check this  number carefully via your
'     offline  mail reader  as the  wrong  number will  toss  all the
'     messages into the wrong area.
'
'     bbsid:BBSID
'     This is  the BBS  identification  name of the  BBS you will  be
'     uploading  your  reply  packet  to.  According  to  the  naming
'     conventions outlined in the QWK format (version 1.6), this will
'     be the file name (not including the extention) of your .QWK and
'     .REP file  (QWK mail packet and reply  packet).  ImportIt! will
'     use this name to access the reply packet, so it is important to
'     get it right.
'
' Completely Stupid and Irrelevant Examples for the Average Fool
'
' postit e maim.zip -p95 -b20 c:\scripts\mc
' (Encodes a binary script of MAIM.ZIP. All output file(s) are written
'  to the C:\SCRIPTS directory and begin with the "MC" suffix. The
'  message length is 95 lines, and 20 blank lines are reserved on the
'  first message.)
' postit -a f x-ray.bas -o -s
' (Filters the file X-RAY.BAS for posting. All blank lines are padded
'  with a space, no prompting is done for file overwrites, and no
'  message splitting is performed.)
' postit d zebra.txt q\
' (Decodes all scripts from the file ZEBRA.TXT to the Q directory.)
' postit e graphics.zip -p95 -b0 -q to:You from:Me conf:32 bbsid:MYBBS
' (Encodes a binary script of GRAPHICS.ZIP. Output files are then
'  attached, or rather merged into MYBBS.REP. The messages will be from
'  YOU to ME in fidonet conference are #32. If to: was not specified,
'  it would be from YOU to ALL.) Tip: Since ImportIt! tosses files
'  directly into the .REP file, there is usually no need to reserve
'  blank lines on the first message.
'
TYPE MsgHeaderType
  Status          AS STRING * 1
  ConfNumASCII    AS STRING * 7
  MsgDate         AS STRING * 8
  MsgTime         AS STRING * 5
  ToField         AS STRING * 25
  FromField       AS STRING * 25
  SubjectField    AS STRING * 25
  PassWord        AS STRING * 12
  MsgRefNumber    AS STRING * 8
  NumBlocks       AS STRING * 6
  Flag            AS STRING * 1
  ConfNum         AS INTEGER          ' should be UNSIGNED INTEGER
  PacketMsgNumber AS STRING * 2
  NetworkTag      AS STRING * 1
END TYPE
' change the following to the name of the archiver you would like
' to use. Must be ZIP, ARJ or LHA
CONST PreferredArchiveMethod$ = "ZIP"
'CONST PreferredArchiveMethod$ = "ARJ"
'CONST PreferredArchiveMethod$ = "LHA"
DIM SHARED OutPutFile$(1 TO 256)
DEFINT A-Z
CONST true = -1, false = 0, Debug% = false
DIM SHARED GERR%: ON ERROR GOTO ErrHandler
LOCATE , , 1
PRINT "PostIt! v7.2 QuickBASIC Compatible Encoder/Decoder"
PRINT "Public Domain by Rich Geldreich and Victor Yiu"
PRINT
IF FRE(-1) < 65536 THEN ErrLvl% = 1: PRINT "Not enough memory": GOTO AllDone
DIM Params$(1 TO 10)
'The following line must be modified for DOS 5 QBASIC.
ParseCmdLine COMMAND$, Params$(), NumParams%
IF NumParams% = 0 THEN ErrLvl% = 2: GOTO ShowHelp
FOR I% = 1 TO NumParams%
  q$ = Params$(I%)
  IF LEFT$(q$, 1) <> "-" AND LEN(q$) = 1 THEN
    Command% = INSTR("EFD", q$)
    IF Command% <> 0 THEN
      Params$(I%) = "": EXIT FOR
    ELSE
      PRINT "Bad command: "; q$: PRINT : ErrLvl% = 3: GOTO ShowHelp
    END IF
  END IF
NEXT
IF Command% = 0 THEN PRINT "No command specified.": PRINT : ErrLvl% = 4: GOTO ShowHelp
IF Command% = 2 THEN DefaultLineLength% = 72 ELSE DefaultLineLength% = 65
sSwitch% = false: pSwitch% = 85: lSwitch% = DefaultLineLength%
tSwitch% = 4: oSwitch% = false: bSwitch% = 0: aSwitch% = false
iSwitch% = false: cSwitch% = false: qSwitch = 0
FOR I% = 1 TO NumParams%
  q$ = Params$(I%): Z$ = MID$(q$, 3)
  IF LEN(q$) THEN
    IF LEFT$(q$, 1) = "-" OR LEFT$(q$, 3) = "TO:" OR LEFT$(q$, 5) = "FROM:" OR LEFT$(q$, 5) = "CONF:" OR LEFT$(q$, 6) = "BBSID:" THEN
      IF LEFT$(q$, 3) <> "TO:" AND LEFT$(q$, 5) <> "FROM:" AND LEFT$(q$, 5) <> "CONF:" AND LEFT$(q$, 6) <> "BBSID:" THEN
        SELECT CASE MID$(q$, 2, 1)
          CASE "S": sSwitch% = true
          CASE "P": pSwitch% = GrabNum&(Z$, 45, 1000, 85)
          CASE "L": lSwitch% = GrabNum&(Z$, 60, 80, CLNG(DefaultLineLength%))
          CASE "T": tSwitch% = GrabNum&(Z$, 1, 8, 4)
          CASE "O": oSwitch% = true
          CASE "B": bSwitch% = GrabNum&(Z$, 0, 30, 0)
          CASE "A": aSwitch% = true
          CASE "I": iSwitch% = true
          CASE "C": cSwitch% = true
          CASE "Q"
            qSwitch% = true
            IIParse COMMAND$, toname$, fromname$, conference%, BBSID$
            qError$ = ""
            IF fromname$ = "" THEN
              qError$ = "From name not specified! "
            ELSEIF conference% = 0 THEN
              qError$ = qError$ + "Conference not specified! "
            ELSEIF BBSID$ = "" THEN
              qError$ = qError$ + "BBSID not specified! "
            END IF
            IF qError$ <> "" THEN
              PRINT LTRIM$(qError$)
              ErrLvl = 3
              qSwitch = false
              GOTO ShowHelp
            END IF
          CASE ELSE: PRINT "Bad switch: "; q$: PRINT : ErrLvl% = 3: GOTO ShowHelp
        END SELECT
      END IF
    ELSE
      SELECT CASE J%
      CASE 0: InputSpec$ = q$
      CASE 1: OutputSpec$ = q$
      CASE ELSE: PRINT "Too many filenames.": PRINT : ErrLvl% = 5: GOTO ShowHelp
      END SELECT: J% = J% + 1
    END IF
  END IF
NEXT
IF J% < 1 THEN PRINT "Must specify input file.": PRINT : ErrLvl% = 5: GOTO ShowHelp
SepPath InputSpec$, InputDrive$, InputPath$, InputName$
IF INSTR(InputName$, ".") = 0 THEN
  IF Command% = 1 THEN     'encoding  .ZIP
    InputSpec$ = InputSpec$ + ".ZIP"
  ELSEIF Command% = 2 THEN 'filtering .BAS
    InputSpec$ = InputSpec$ + ".BAS"
  ELSEIF Command% = 3 THEN 'decoding  .TXT
    InputSpec$ = InputSpec$ + ".TXT"
  END IF
ELSE
  IF Command% = 1 THEN
    SELECT CASE MID$(InputName$, INSTR(InputName$, ".") + 1, 3)
    CASE "ZIP", "LZH", "ARJ", "GIF", "SQZ", "ZOO", "ARC", "HAP", "JPG"
    CASE ELSE: PRINT "Warning: Uncompressed files should not be" + " encoded" + " into binary scripts!": PRINT
  END SELECT
  END IF
END IF
OPEN InputSpec$ FOR INPUT AS #1: CLOSE #1
IF GERR% THEN PRINT "Can't open "; InputSpec$: ErrLvl% = 6: GOTO AllDone
SepPath OutputSpec$, OutDrive$, OutPath$, OutName$
TestFile$ = OutDrive$ + OutPath$ + "pi742875.2yz"
OPEN TestFile$ FOR OUTPUT AS #1: CLOSE #1
IF GERR% THEN PRINT "Bad output specification.": ErrLvl% = 7: GOTO AllDone
KILL TestFile$
SELECT CASE Command%
CASE 1: Status% = Encode%(0, iSwitch%, cSwitch%, aSwitch%, tSwitch%, sSwitch%, pSwitch%, lSwitch%, oSwitch%, bSwitch%, InputSpec$, OutputSpec$)
CASE 2: Status% = Encode%(1, iSwitch%, cSwitch%, aSwitch%, tSwitch%, sSwitch%, pSwitch%, lSwitch%, oSwitch%, bSwitch%, InputSpec$, OutputSpec$)
CASE 3: Status% = Decode%(oSwitch%, InputSpec$, OutputSpec$)
END SELECT
IF Status% < 0 THEN ErrLvl% = 8 ELSE IF Status% > 0 THEN ErrLvl% = 9 ELSE ErrLvl% = 0
GOTO AllDone
ShowHelp:
PRINT "Usage: POSTIT [switch] command inputfile [outputfile] [-q" + " options]"
PRINT
PRINT "Commands:"
PRINT "e [E]ncode any file <150k into a self extracting binary script"
PRINT "f [F]ilter QB source into a text script"
PRINT "d [D]ecode captured text or binary script(s)"
PRINT
PRINT "Switches:"
PRINT "-s  Don't split output file into multiple messages"
PRINT "-o  Don't prompt for file overwrites"
PRINT "-b# Reserve # blank lines on first message (0-30, default=0)"
PRINT "-t# Set tab stops to # characters (1-8, default=4)"
PRINT "-l# Set line length to # characters (60-80, default=65 or 72)"
PRINT "-p# Set message length to # lines (45-1000, default=85)"
PRINT "-a  Padd blank lines with a space when filtering"
PRINT "-i  Ignore blank lines when filtering"
PRINT "-c  Crush space characters from start of lines when filtering"
PRINT
PRINT "ImportIt! (QWK compatable .REP file support):"
PRINT "-q [to:to_name] from:from_name conf:conf_num bbsid:BBSID"
AllDone:
IF qSwitch = true THEN
  IF GERR < 0 THEN
    IF Debug% THEN PRINT "Exiting with an errorlevel of"; ErrLvl%
    END
  END IF
  FOR n = 1 TO 256
    a$ = OutPutFile$(n)
    IF a$ = "" THEN EXIT FOR
  NEXT n
  NumFiles = n - 1
  DIM MsgFiles$(1 TO NumFiles)
  FOR n = 1 TO NumFiles
    MsgFiles$(n) = OutPutFile$(n)
  NEXT n
  FOR n = LEN(InputSpec$) TO 1 STEP -1
    IF MID$(InputSpec$, n, 1) = "\" THEN StartFname = n + 1
  NEXT n
  IF StartFname <> 0 THEN
    TitleFile$ = MID$(InputSpec$, StartFname, 1)
  ELSE
    TitleFile$ = InputSpec$
  END IF
  FOR n = 1 TO LEN(toname$)
    IF MID$(toname$, n, 1) = "." THEN MID$(toname$, n, 1) = " "
  NEXT n
  FOR n = 1 TO LEN(fromname$)
    IF MID$(fromname$, n, 1) = "." THEN MID$(fromname$, n, 1) = " "
  NEXT n
  ImportIt BBSID$, MsgFiles$(), toname$, fromname$, TitleFile$, conference%
END IF
IF Debug% THEN PRINT "Exiting with an errorlevel of"; ErrLvl%
END
ErrHandler: GERR% = ERR
  IF Debug% THEN IF GERR% <> 53 THEN PRINT "Global error #"; GERR%
RESUME NEXT

SUB AddToRep (BBSID$, MsgFiles$(), toname$, fromname$, TitleFile$, conference, ErrorCode$)
  DIM MsgHeader     AS MsgHeaderType
  DIM QWKRecBuff    AS STRING * 128
  DIM QWKByteBuff   AS STRING * 1
  DIM ArcHeader     AS STRING * 3
  ' test for file
  OPEN BBSID$ + ".REP" FOR BINARY AS #1
  IF LOF(1) = 0 THEN
    CLOSE #1
    KILL BBSID$ + ".REP"
    ErrorCode$ = "Reply packet (.REP file) not found!"
    EXIT SUB
  END IF
  ' test for messages
  NumMessages = UBOUND(MsgFiles$)
  IF NumMessages = 0 THEN
    CLOSE #1
    ErrorCode$ = "No files to add to reply (.REP) packet!"
    EXIT SUB
  END IF
  ' check ToName$
  IF toname$ = "" THEN
    toname$ = "ALL"
  END IF
  ' check FromName$
  IF fromname$ = "" THEN
    CLOSE #1
    ErrorCode$ = "No from field (name) specified!"
    EXIT SUB
  END IF
  CLOSE #1
  ' process mail packet
  PRINT
  PRINT "Unarchiving "; BBSID$ + ".REP";
  ' determine archive type
  OPEN BBSID$ + ".REP" FOR BINARY AS #1
  ' PKZIP file?
  SEEK 1, 1
  GET #1, , ArcHeader
  IF ArcHeader = "PK" + CHR$(3) THEN
    DeArcCommand$ = "PKUNZIP"
    ArcCommand$ = "PKZIP"
    ArcType$ = "ZIP"
  END IF
  ' LZH file?
  SEEK 1, 3
  GET #1, , ArcHeader
  IF ArcHeader = "-lh" THEN
    DeArcCommand$ = "LHA E"
    ArcCommand$ = "LHA A /M"
    ArcType$ = "LZH"
  END IF
  ' ARJ file?
  SEEK 1, 1
  GET #1, , ArcHeader
  IF LEFT$(ArcHeader, 2) = "'" + CHR$(234) THEN
    DeArcCommand$ = "ARJ E"
    ArcCommand$ = "ARJ A -Y"
    ArcType$ = "ARJ"
  END IF
  ' dearchive file
  PRINT " using "; ArcType$
  SHELL DeArcCommand$ + " " + BBSID$ + ".REP"
  CLOSE #1
  ' test for file
  OPEN BBSID$ + ".MSG" FOR BINARY AS #1
  IF LOF(1) = 0 THEN
    ErrorCode$ = "Error occured during DeArchiving. File " + BBSID$ + ".MSG not found in archive"
    CLOSE #1
    KILL BBSID$ + ".MSG"
    EXIT SUB
  END IF
  ' read messages
  PRINT
  PRINT "Reading Messages from "; BBSID$; ".MSG..."
  SEEK 1, 1
  GET #1, , QWKRecBuff
  DO
    GET #1, , MsgHeader
    NewHighest = VAL(MsgHeader.MsgRefNumber)
    IF NewHighest > Highest THEN Highest = NewHighest
    ' read until next message
    FOR n = 1 TO VAL(MsgHeader.NumBlocks) - 1
      GET #1, , QWKRecBuff
    NEXT n
  LOOP UNTIL SEEK(1) >= LOF(1)
  PRINT
  PRINT "Writing new messages..."
  PRINT
  PRINT "To:     "
  PRINT "From:   "
  PRINT "Subj:   "
  PRINT "Conf:   "
  PRINT "Date:   "
  PRINT "Time:   "
  PRINT "Number: "
  StartLin = CSRLIN - 7
  FOR msg = 1 TO NumMessages
    LOCATE StartLin, 1
    Subj$ = "[" + LTRIM$(STR$(msg)) + "/" + LTRIM$(STR$(NumMessages)) + "] " + TitleFile$
    conf$ = LTRIM$(STR$(conference))
    num$ = LTRIM$(STR$(msg + Highest - 1))
    dat$ = LEFT$(DATE$, 6) + RIGHT$(DATE$, 2)
    tim$ = LEFT$(TIME$, 5)
    PRINT "To:     "; toname$
    PRINT "From:   "; fromname$
    PRINT "Subj:   "; Subj$
    PRINT "Conf:   "; conf$
    PRINT "Date:   "; dat$
    PRINT "Time:   "; tim$
    PRINT "Number: "; num$
    PRINT "Writing File: "; MsgFiles$(msg);
    TotalLen& = 0
    OPEN MsgFiles$(msg) FOR INPUT AS #2
    OPEN "~IIBETA.TMP" FOR BINARY AS #3
    DO WHILE NOT EOF(2)
      LINE INPUT #2, text$
      text$ = text$ + CHR$(227)
      PUT #3, , text$
    LOOP
    TotalLen& = SEEK(3)
    TotalLen& = TotalLen& + 128       ' for tagline
    QWKRecBuff = CHR$(227) + " * ImportIt! v1.0b [BETA] * ImportIt!" + " [PD] by Calvin French, August 1993" + CHR$(227) + CHR$(227)
    PUT #3, , QWKRecBuff
    ExtraString$ = SPACE$(128 - (TotalLen& MOD 128))
    TotalLen& = TotalLen& + LEN(ExtraString$)
    PUT #3, , ExtraString$
    Blocks$ = LTRIM$(STR$((TotalLen& / 128) + 1))
    MsgHeader.Status = "-"            ' public, read
    MsgHeader.ConfNumASCII = conf$    ' conference (.REP only)
    MsgHeader.MsgDate = dat$          ' date
    MsgHeader.MsgTime = tim$          ' time
    MsgHeader.ToField = toname$       ' to
    MsgHeader.FromField = fromname$   ' from
    MsgHeader.SubjectField = Subj$    ' subject
    MsgHeader.PassWord = SPACE$(12)   ' password
    MsgHeader.MsgRefNumber = num$     ' message number
    MsgHeader.NumBlocks = Blocks$     ' blocks in message
    MsgHeader.Flag = CHR$(225)        ' active flag
    MsgHeader.ConfNum = conference    ' conference (.REP and .QWK)
    MsgHeader.PacketMsgNumber = " "   ' not sure what this is.
    MsgHeader.NetworkTag = " "        ' network tagline
    PUT #1, , MsgHeader
    SEEK 3, 1
    FOR n = 1 TO TotalLen& / 128
      GET #3, , QWKRecBuff
      PUT #1, , QWKRecBuff
    NEXT n
    CLOSE #3
    CLOSE #2
    KILL "~IIBETA.TMP"
  NEXT msg
  CLOSE #1
  PRINT
  PRINT
  PRINT "Rearchiving Packet..."
  SHELL ArcCommand$ + " " + BBSID$ + ".REP " + BBSID$ + ".MSG"
  PRINT
  PRINT "Deleting " + BBSID$ + ".MSG..."
  PRINT
  KILL BBSID$ + ".MSG"
  ErrorCode$ = "Packet Successfully Processed!"
END SUB

SUB CreateRep (BBSID$, ArcCommand$)

DIM QWKRecBuff AS STRING * 128

PRINT
PRINT "Creating message data file (.MSG file)..."
PRINT

OPEN BBSID$ + ".MSG" FOR BINARY AS #1

QWKRecBuff = UCASE$(BBSID$)

PUT #1, , QWKRecBuff

CLOSE #1

PRINT "Archiving file..."

SHELL ArcCommand$ + " " + BBSID$ + ".REP " + BBSID$ + ".MSG"

PRINT
PRINT "Deleting message data file (.MSG file)..."

KILL BBSID$ + ".MSG"

END SUB

FUNCTION Decode% (oSwitch%, InSpec$, OutSpec$)
  DIM Lines$(1 TO 256), ValidChar%(255)
  FOR q% = 0 TO 85 'Valid encoding characters
    IF q% = 27 THEN
      ValidChar%(ASC("#")) = true
    ELSEIF q% = 59 THEN
      ValidChar%(ASC("$")) = true
    ELSE
      ValidChar%(q% + 37) = true
    END IF
  NEXT
  GERR% = 0: Z$ = "OPEN " + CHR$(34) + "O" + CHR$(34) + ",1," + CHR$(34)
  SepPath OutSpec$, OutDrive$, OutPath$, OutName$
  OutPath$ = OutDrive$ + OutPath$
  InputHandle% = FREEFILE
  OPEN InSpec$ FOR INPUT AS InputHandle% LEN = 8192
  OutputHandle% = FREEFILE
  DO
    IF FoundNewScript% = false THEN
      DO UNTIL EOF(InputHandle%)
        M% = M% + 1: IF M% = 16 THEN GOSUB AbortCheck
        LineNum& = LineNum& + 1
        LINE INPUT #1, a$: a$ = LTRIM$(RTRIM$(UCASE$(a$)))
        IF GERR% THEN PRINT "Error while reading from input file!": GOTO DecodeExit
        IF LEFT$(a$, 14) = "'>>> PAGE 1 OF" AND INSTR(a$, "BEGINS" + " HERE") > 0 AND INSTR(a$, "TYPE:") > 0 THEN EXIT DO
      LOOP
      IF EOF(InputHandle%) THEN EXIT DO
    END IF
    FoundNewScript% = false
    OutFile$ = LTRIM$(MID$(a$, 15))
    OutFile$ = RTRIM$(LEFT$(OutFile$, INSTR(OutFile$, "BEGINS") - 1))
    IF LEN(OutFile$) = 0 THEN GOTO FindNext
    IF LEN(OutName$) = 0 OR OutFile$ = OutName$ THEN
      FilesCRC% = -1: FilesLength& = -1: ScrDone% = false
      BadScript% = false: NumLines% = 0: K% = 0: s% = 0: B& = 0
      q% = INSTR(a$, "TYPE:") + 5
      SELECT CASE MID$(a$, q%, 3)
      CASE "BAS": ScriptType% = 0
      CASE "BIN"
        ScriptType% = 1
        EncodeVer% = FASC%(MID$(a$, q% + 3, 1)) - 65
        ExtractVer% = FASC%(MID$(a$, q% + 4, 1)) - 65
        IF ExtractVer% <> 0 THEN PRINT "Unsupported encoding algorithm" + "" + " for file "; OutFile$: PRINT : GOTO FindNext
      CASE ELSE: PRINT "Unsupported script type for file "; OutFile$: PRINT : GOTO FindNext
      END SELECT
      GOSUB CheckLine
      OPEN OutPath$ + OutFile$ FOR INPUT AS OutputHandle%: CLOSE OutputHandle%
      IF GERR% = 0 THEN
        IF oSwitch% = false THEN
          PRINT OutPath$ + OutFile$; " already exists. [O]verwrite, or" + "" + " [A]bort(o/a)? ";
          DO: DO: a$ = INKEY$: LOOP UNTIL LEN(a$): a$ = UCASE$(a$)
          LOOP UNTIL INSTR("OA" + CHR$(27), a$)
          LOCATE , 1: PRINT SPACE$(78); : LOCATE , 1
          SELECT CASE a$
          CASE "A", CHR$(27): GERR% = -1: PRINT "Aborted by user!": GOTO DecodeExit
          END SELECT
        END IF
      END IF
      GERR% = 0: OPEN OutPath$ + OutFile$ FOR OUTPUT AS OutputHandle%
      IF GERR% THEN PRINT "Error while opening "; OutPath$ + OutFile$; "!": GOTO DecodeExit
      OutSpecOpened% = true
      IF ScriptType% = 0 THEN PRINT "Unfiltering ";  ELSE PRINT "Decoding ";
      PRINT OutPath$ + OutFile$; "... ";
      LookingForNextPage% = false
      CurrentPage% = 1
      DO UNTIL EOF(InputHandle%)
        IF GERR% THEN PRINT "Error #"; STR$(GERR%); " while processing" + "" + " file!": GOTO DecodeExit
        M% = M% + 1: IF M% = 16 THEN GOSUB AbortCheck
        LineNum& = LineNum& + 1
        LINE INPUT #InputHandle%, a$: a$ = RTRIM$(a$)
        IF ScriptType% = 1 THEN a$ = LTRIM$(a$)
        IF LEFT$(a$, 4) = "'>>>" THEN
          GOSUB CheckLine
          IF UCASE$(LEFT$(a$, 10)) = "'>>> PAGE " THEN
            a$ = UCASE$(a$)
            IF LEFT$(a$, 15) = "'>>> PAGE 1 OF " AND INSTR(a$, "BEGINS" + "" + " HERE") > 0 THEN
              PRINT "Premature end of script on line"; LineNum&
              FoundNewScript% = true: BadScript% = true: EXIT DO
            END IF
            IF GrabNum&(MID$(a$, 11), 1, 256, -1) <> CurrentPage% THEN PRINT "Page out of sync on line"; LineNum&: BadScript% = true: EXIT DO
            IF INSTR(a$, "BEGINS HERE") THEN
              IF LookingForNextPage% = false THEN PRINT "Page"; CurrentPage%; " was encountered more than once on line"; LineNum&: BadScript% = true: EXIT DO
              LookingForNextPage% = false
            ELSEIF INSTR(a$, "ENDS HERE") THEN
              IF LookingForNextPage% = true THEN PRINT "Page"; CurrentPage%; "was terminated prematurely on line"; LineNum&: BadScript% = true: EXIT DO
              LookingForNextPage% = true
              CurrentPage% = CurrentPage% + 1
              IF INSTR(a$, "LAST PAGE") THEN ScrDone% = true: EXIT DO
            ELSE
              PRINT "Bad page header on line"; LineNum&: BadScript% = true: EXIT DO
            END IF
          END IF
        ELSE
          IF LookingForNextPage% = false THEN
            IF ScriptType% = 0 THEN
              GOSUB ShrinkLine
            ELSE
              IF LEFT$(a$, 1) = "U" AND LEFT$(LTRIM$(MID$(a$, 2)), 1) = CHR$(34) THEN GOSUB DecodeLine
            END IF
          END IF
        END IF
      LOOP
      IF BadScript% = false THEN
        IF ScrDone% = false THEN PRINT "Premature end of script on" + " line"; LineNum&: BadScript% = true: GOTO DecodeDone
        GoodScripts% = GoodScripts% + 1
        IF ScriptType% = 0 THEN
          IF NumLines% > 0 THEN a$ = "": GOSUB ShrinkLine
          PRINT "Ok"
        ELSE
          IF FilesLength& = -1 THEN
            PRINT "Warning: File's length could not be located!"
          ELSEIF FilesLength& <> B& THEN
            PRINT "Warning: Decoded file's length is incorrect."
          ELSEIF FilesCRC% = -1 THEN
            PRINT "Warning: File's checksum could not be located!"
          ELSEIF FilesCRC% <> s% THEN
            PRINT "Warning: Decoded file's checksum is incorrect."
          ELSE
            PRINT "Ok"
          END IF
        END IF
      END IF
DecodeDone:
      CLOSE OutputHandle%
      IF GERR% THEN PRINT "Error while writing to output file!": GOTO DecodeExit
      IF BadScript% THEN KILL OutPath$ + OutFile$
      OutSpecOpened% = false
      PRINT : IF OutFile$ = OutName$ THEN EXIT DO
    END IF
FindNext:
  LOOP UNTIL EOF(InputHandle%)
'----------------------------------------------------------
DecodeExit:
  q% = GERR%: CLOSE InputHandle%: CLOSE OutputHandle%
  IF q% = 0 THEN PRINT LTRIM$(STR$(GoodScripts%)); " script(s) decoded" + "" + " successfully."
  IF q% <> 0 AND OutSpecOpened% THEN KILL OutPath$ + OutFile$
  Decode% = q%
EXIT FUNCTION
'----------------------------------------------------------
ShrinkLine:
  FoundIt% = FASC(RIGHT$(a$, 1)) = 95
  IF FoundIt% THEN
    InQuote% = false
    FOR I% = 1 TO LEN(a$)
      IF MID$(a$, I%, 1) = CHR$(34) THEN InQuote% = NOT InQuote%
    NEXT
    'Don't combine lines that are part of binary scripts
    IF InQuote% THEN FoundIt% = false
  END IF
  IF FoundIt% OR NumLines% > 0 THEN
    IF NumLines% = 256 THEN
      PRINT "Too many line continuations!": BadScript% = true: GOTO DecodeDone
    END IF
    NumLines% = NumLines% + 1: Lines$(NumLines%) = a$
    IF FoundIt% = false THEN 'last line?
      a$ = ""
      FOR a% = 1 TO NumLines%
        B$ = Lines$(a%)
        'can we combine two quoted strings together?
        CombineQuote% = false
        IF RIGHT$(a$, 2) = "+_" AND LEN(a$) > 3 THEN
          IF RIGHT$(RTRIM$(LEFT$(a$, LEN(a$) - 2)), 1) = CHR$(34) THEN
            IF FASC(LTRIM$(B$)) = 34 THEN CombineQuote% = true
          END IF
        END IF
        IF CombineQuote% THEN
          a$ = RTRIM$(LEFT$(a$, LEN(a$) - 2))
          a$ = LEFT$(a$, LEN(a$) - 1) + MID$(LTRIM$(B$), 2)
        ELSE
          InQuote% = false
          'can we combine two remarks together?
          FOR I% = 1 TO LEN(a$)
            q$ = MID$(a$, I%, 1)
            IF q$ = CHR$(34) THEN
              InQuote% = NOT InQuote%
            ELSEIF InQuote% = false THEN
              IF q$ = "'" OR UCASE$(MID$(a$, I%, 4)) = "REM " THEN
                IF LEFT$(LTRIM$(B$), 1) = "'" THEN B$ = MID$(B$, 2)
                EXIT FOR
              END IF
            END IF
          NEXT
          'eradicate trailing "_" character
          IF LEN(a$) THEN a$ = LEFT$(a$, LEN(a$) - 1)
          a$ = a$ + B$
        END IF
      NEXT
      PRINT #OutputHandle%, a$: NumLines% = 0
    END IF
  ELSE
    PRINT #OutputHandle%, a$
  END IF
  IF GERR% THEN PRINT "Error while writing to output file!": GOTO DecodeExit
RETURN
'----------------------------------------------------------
DecodeLine: '**MOD 86 Decoder**
  a$ = MID$(LTRIM$(MID$(a$, 2)), 2)
  IF RIGHT$(a$, 1) = CHR$(34) THEN a$ = LEFT$(a$, LEN(a$) - 1)
  FOR a% = 1 TO LEN(a$)
    C% = ASC(MID$(a$, a%, 1))
    IF ValidChar%(C%) = false THEN PRINT "Illegal character found on" + " line"; LineNum&: BadScript% = true: GOTO DecodeDone
    C% = C% - 37: IF C% < 0 THEN C% = 91 + C% * 32
    IF K% < 4 THEN
      IF C% > 80 THEN PRINT "Decode out of sync/illegal character" + " found" + " on line"; LineNum&: BadScript% = true: GOTO DecodeDone
      K% = C% + 243
    ELSE
      T% = C% + (K% MOD 3) * 86: IF T% > 255 THEN PRINT "Illegal" + " character found on line"; LineNum&: BadScript% = true: GOTO DecodeDone
      PRINT #OutputHandle%, CHR$(T%);
      IF GERR% THEN PRINT "Error while writing to output file!": GOTO DecodeExit
      B& = B& + 1: K% = K% \ 3
    END IF
    s% = (s% + C%) AND 255
  NEXT
RETURN
'----------------------------------------------------------
CheckLine:
  q% = INSTR(a$, "TLEN:")
  IF q% THEN FilesLength& = GrabNum&(MID$(a$, q% + 5), 1, 153600, -1)
  q% = INSTR(a$, "TCHK:")
  IF q% THEN FilesCRC% = GrabNum&(MID$(a$, q% + 5), 0, 255, -1)
RETURN
'----------------------------------------------------------
AbortCheck: M% = 0: K$ = INKEY$
  IF K$ = CHR$(27) OR K$ = CHR$(0) + CHR$(0) THEN GERR% = -1: PRINT "Aborted by user!": GOTO DecodeExit
RETURN
END FUNCTION

FUNCTION Encode% (Op%, iSwitch%, cSwitch%, aSwitch%, tSwitch%, sSwitch%, pSwitch%, lSwitch%, oSwitch%, bSwitch%, InSpec$, OutSpec$)
  ' following SHARED is for ImportIt!
  DIM Bucket%(1 TO 4), Lines$(64)
  GERR% = 0: q$ = CHR$(34)
'----------------------------------------------------------
  SepPath InSpec$, OutDrive$, OutPath$, InName$
  SepPath OutSpec$, OutDrive$, OutPath$, OutName$
  IF LEN(OutName$) = 0 THEN
    OutName$ = InName$
    IF INSTR(OutName$, ".") THEN OutName$ = LEFT$(OutName$, INSTR(OutName$, ".") - 1)
  END IF
  IF INSTR(OutName$, ".") THEN
    OutExt$ = MID$(OutName$, INSTR(OutName$, "."))
    OutName$ = LEFT$(OutName$, INSTR(OutName$, ".") - 1)
  END IF
  IF LEN(OutExt$) = 0 THEN IF Op% THEN OutExt$ = ".PST" ELSE OutExt$ = ".PI"
'----------------------------------------------------------
  InputHandle% = FREEFILE
  IF Op% THEN
    OPEN InSpec$ FOR INPUT AS InputHandle% LEN = 8192
  ELSE
    OPEN InSpec$ FOR BINARY AS InputHandle%
  END IF
  InputFileSize& = LOF(InputHandle%)
  IF Op% = 0 AND InputFileSize& > (150 * 1024&) THEN
    PRINT "Can't encode files larger than 150k."
    GERR% = -1: GOTO EncodeExit
  ELSEIF InputFileSize& = 0 THEN
    PRINT "Input file is null.": GERR% = -2: GOTO EncodeExit
  END IF
'----------------------------------------------------------
  IF Op% THEN PRINT "Filtering ";  ELSE PRINT "Encoding ";
  PRINT InSpec$; " ("; LTRIM$(STR$((InputFileSize& + 1023) \ 1024)); "k)"
  PRINT
'----------------------------------------------------------
  OutputHandle% = FREEFILE: LinesInPage% = 0
'----------------------------------------------------------
  IF Op% = 0 THEN
    Work$ = "U" + q$ + SPACE$(lSwitch% - 2): WorkPos% = 3
    CurrentSub% = 0: LinesInSub% = 0: FlagScaler% = 1
    GOSUB PrintDecodeHeader
    BytesLeft& = InputFileSize&: BufferSize% = 4096
    Buffer$ = SPACE$(BufferSize)
    DO
      IF GERR% THEN PRINT "- Error #"; STR$(GERR%); " while encoding" + " file!": GOTO EncodeExit
      IF BytesLeft& < BufferSize% THEN Buffer$ = SPACE$(BytesLeft&): BufferSize% = BytesLeft&
      GET InputHandle%, , Buffer$
      IF GERR% THEN PRINT "- Error while reading from input file!": GOTO EncodeExit
      GOSUB EncodeBlock
    LOOP WHILE BytesLeft&
    IF NumCodes% THEN GOSUB FlushCodeBuffer
    IF WorkPos% > 3 THEN Work$ = LEFT$(Work$, WorkPos% - 1): GOSUB PutSubLine
    IF LinesInSub% THEN L$ = "END SUB": GOSUB PutLine
    FOR a% = 2 TO CurrentSub%: L$ = "V" + HEX$(a%): GOSUB PutLine: NEXT
    GOSUB PrintDecodeTrailer
  ELSE
    BytesLeft& = InputFileSize&
    DO UNTIL EOF(InputHandle)
      IF GERR% THEN PRINT "- Error #"; STR$(GERR%); " while filtering" + "" + " file!": GOTO EncodeExit
      LINE INPUT #InputHandle, a$: a$ = RTRIM$(UnTab$(a$, tSwitch%))
      IF GERR% THEN PRINT "- Error while reading from input file!": GOTO EncodeExit
      IF cSwitch% THEN a$ = LTRIM$(a$)
      BytesLeft& = BytesLeft& - LEN(a$) - 2
      IF LEN(a$) > 0 OR iSwitch% = false THEN
        ExpandLine a$, Lines$(), lSwitch%, NumLines%
        'Don't let split lines cross page boundries, because QB won't
        'put them back together.
        IF sSwitch% = false AND (NumLines% > 1) AND (LinesInPage% + 1 + NumLines%) > pSwitch% THEN
          PRINT #OutputHandle%, "'>>> Page"; STR$(NumOutputFiles%); " of "; InName$; " ends here. Continued on next page."
          LinesInPage% = LinesInPage% + 1: GOSUB CloseOutputFile
        END IF
        FOR a% = 1 TO NumLines%
          L$ = Lines$(a%)
          'Don't let blank lines proceed the first page header.
          IF LinesInPage% <> 0 OR LEN(RTRIM$(L$)) > 0 THEN
            'The padding option is for those unfortunates that post
            'source online in RBBS's grubby line oriented text editor...
            IF aSwitch% THEN IF LEN(L$) = 0 THEN L$ = " "
            GOSUB PutLine
          END IF
        NEXT
      END IF
    LOOP
  END IF
'----------------------------------------------------------
  L$ = "'>>> Page" + STR$(NumOutputFiles%) + " of " + InName$ + " ends" + "" + " here. Last page."
  IF Op% = 0 THEN L$ = L$ + " TCHK:" + LTRIM$(STR$(CheckSum%))
  GOSUB PutLine: GOSUB CloseOutputFile: PRINT
  PRINT LTRIM$(STR$(TotalLines%)); " lines in"; STR$(NumOutputFiles%); " message(s) written."
'----------------------------------------------------------
EncodeExit:
  q% = GERR%
  CLOSE InputHandle%: CLOSE OutputHandle%
  IF q% <> 0 THEN FOR a% = 1 TO NumOutputFiles%: KILL OutPutFile$(a%): NEXT
  Encode% = q%
EXIT FUNCTION
'----------------------------------------------------------
EncodeBlock: '**MOD 86 Encoder**
  FOR I% = 1 TO BufferSize%
    Byte% = ASC(MID$(Buffer$, I%, 1)): BytesLeft& = BytesLeft& - 1
    CurrentFlag% = CurrentFlag% + (Byte% \ 86) * FlagScaler%
    FlagScaler% = FlagScaler% * 3: NumCodes% = NumCodes% + 1
    Bucket%(NumCodes%) = Byte% MOD 86
    IF NumCodes% = 4 THEN GOSUB FlushCodeBuffer
  NEXT
RETURN
'----------------------------------------------------------
FlushCodeBuffer:
  q% = CurrentFlag%: GOSUB PutByte
  FOR J% = 1 TO NumCodes%: q% = Bucket%(J%): GOSUB PutByte: NEXT
  NumCodes% = 0: CurrentFlag% = 0: FlagScaler% = 1
RETURN
'----------------------------------------------------------
PutByte:
  CheckSum% = (CheckSum% + q%) AND 255
  IF q% = 27 THEN
    MID$(Work$, WorkPos%) = "#"
  ELSEIF q% = 59 THEN
    MID$(Work$, WorkPos%) = "$"
  ELSE
    MID$(Work$, WorkPos%) = CHR$(q% + 37)
  END IF
  WorkPos% = WorkPos% + 1: IF WorkPos% > lSwitch% THEN GOSUB PutSubLine
RETURN
'----------------------------------------------------------
PutSubLine:
  IF LinesInSub% = 0 THEN
    CurrentSub% = CurrentSub% + 1
    IF CurrentSub% = 1 THEN
      L$ = "SUB V1:OPEN " + q$ + "O" + q$ + ",1," + q$ + InName$ + q$ + ",4^6:Z&=" + LTRIM$(STR$(LOF(1))) + ":?STRING$(50,177);"
    ELSE
      L$ = "SUB V" + HEX$(CurrentSub%)
    END IF
    GOSUB PutLine
  END IF
  L$ = Work$: GOSUB PutLine
  LinesInSub% = LinesInSub% + 1
  IF LinesInSub% = 200 THEN L$ = "END SUB": GOSUB PutLine: LinesInSub% = 0
  WorkPos% = 3
RETURN
'----------------------------------------------------------
PutLine:
  IF LinesInPage% = 0 THEN GOSUB OpenNewOutputFile
  PRINT #OutputHandle%, L$
  IF GERR% THEN PRINT "- Error writing to output file!": GOTO EncodeExit
  LinesInPage% = LinesInPage% + 1
  IF sSwitch% = false THEN
    'make sure last page has some meat on it
    IF LinesInPage% = (pSwitch% - 1) OR (BytesLeft& < 256 AND LinesInPage% > (pSwitch% - 10)) THEN
      PRINT #OutputHandle%, "'>>> Page"; STR$(NumOutputFiles%); " of "; InName$; " ends here. Continued on next page."
      LinesInPage% = LinesInPage% + 1: GOSUB CloseOutputFile
    END IF
  END IF
  'Check the blower for contol+c and escape every few lines...
  IF (LinesInPage% AND 7) = 1 THEN
    a$ = INKEY$: IF a$ = CHR$(27) OR a$ = CHR$(0) + CHR$(0) THEN GERR% = -3: PRINT "- Aborted by user!": GOTO EncodeExit
  END IF
RETURN
'----------------------------------------------------------
OpenNewOutputFile:
  IF NumOutputFiles% = 256 THEN GERR% = -4: PRINT "Too many output" + " files!": GOTO EncodeExit
  NumOutputFiles% = NumOutputFiles% + 1
  IF sSwitch% = true THEN
    J$ = OutName$
  ELSE
    J$ = LTRIM$(STR$(NumOutputFiles%))
    J$ = LEFT$(OutName$, 8 - LEN(J$)) + J$
  END IF
  OutFile$ = OutDrive$ + OutPath$ + J$ + OutExt$: GERR% = 0
  OPEN OutFile$ FOR INPUT AS OutputHandle%: CLOSE OutputHandle%
  IF GERR% = 0 THEN
    IF oSwitch% = false THEN
      PRINT OutFile$; " already exists. [O]verwrite, overwrite [R]est," + "" + " or [A]bort(o/r/a)? ";
      DO: DO: a$ = INKEY$: LOOP UNTIL LEN(a$): a$ = UCASE$(a$)
      LOOP UNTIL INSTR("ORA" + CHR$(27), a$)
      LOCATE , 1: PRINT SPACE$(78); : LOCATE , 1
      SELECT CASE a$
      CASE "A", CHR$(27): GERR% = -3: PRINT "Aborted by user!"
        NumOutputFiles% = NumOutputFiles% - 1: GOTO EncodeExit
      CASE "R": oSwitch% = true
      END SELECT
    END IF
  END IF
  PRINT "Now writing: "; OutFile$; " ";
  GERR% = 0: OPEN OutFile$ FOR OUTPUT AS OutputHandle% LEN = 4096
  OutPutFile$(NumOutputFiles%) = OutFile$
  IF GERR% THEN
    PRINT "- Error opening output file!"
    NumOutputFiles% = NumOutputFiles% - 1: GOTO EncodeExit
  END IF
  LinesInPage% = 1
  IF NumOutputFiles% = 1 THEN
    FOR I% = 1 TO bSwitch%
      IF aSwitch% THEN PRINT #OutputHandle, " " ELSE PRINT #OutputHandle,
    NEXT
    LinesInPage% = LinesInPage% + bSwitch%
  END IF
  PRINT #OutputHandle%, "'>>> Page"; STR$(NumOutputFiles%); " of "; InName$; " begins here.";
  IF NumOutputFiles% > 1 THEN
    PRINT #OutputHandle%,
  ELSE
    IF Op% = 0 THEN
      'The first letter after "BIN" is  which algorithm was used
      'to encode the file. The second letter is the minimum decoding
      'algorithm required to extract the file. Both range from A-Z.
      PRINT #OutputHandle%, " TYPE:BINAA";
      'TLEN stands for "total length".
      PRINT #OutputHandle%, " TLEN:"; LTRIM$(STR$(InputFileSize&))
      'In the future, other information may be put onto this line,
      'such as the file's date and time. (Actually, any line
      'starting will "'>>>" will be scanned for information by
      'the Decode function.)
    ELSE
      PRINT #OutputHandle%, " TYPE:BAS"
    END IF
  END IF
  GERR% = 0
RETURN
'----------------------------------------------------------
CloseOutputFile:
  CLOSE OutputHandle%
  IF GERR% THEN PRINT "- Error while writing to output file!": GOTO EncodeExit
  PRINT : TotalLines% = TotalLines% + LinesInPage%: LinesInPage% = 0
RETURN
'----------------------------------------------------------
PrintDecodeHeader:
  L$ = "DEFINT A-Z:DIM SHARED K,S,B&,Z&:V1 'Created by PostIt! 7.2"
  GOSUB PutLine
RETURN
'----------------------------------------------------------
PrintDecodeTrailer:
  L$ = "CLOSE:IF S=" + LTRIM$(STR$(CheckSum%))
  L$ = L$ + "AND B&=Z&THEN?" + q$ + " :) Ok!" + q$ + "ELSE?" + q$ + " " + "" + ":( Bad!"
  GOSUB PutLine
  L$ = "SUB U(A$):FOR A=1TO LEN(A$):C=ASC(MID$(A$,A))-37:IF C<0THEN" + " C=91+C*32"
  GOSUB PutLine
  L$ = "IF K<4THEN K=C+243ELSE?#1,CHR$(C+(K MOD 3)*86);:K=K\3:B&=B&+1"
  GOSUB PutLine
  L$ = "S=(S+C)AND 255:NEXT:LOCATE,1:?STRING$(B&*50\Z&,219);:END SUB"
  GOSUB PutLine
RETURN
END FUNCTION

'This self containted subroutine for splitting QB lines was made by
'Victor Yiu and a few other folks on the QUIK_BAS echo.
SUB ExpandLine (a$, Lines$(), LineLength%, NumLines%)
  NumLines% = 0
  'check to see if the line has already been split
  FOR I% = LEN(a$) TO 1 STEP -1
    SELECT CASE MID$(a$, I%, 1)
    CASE "_": NoSplit% = true
    CASE " "
    CASE ELSE: EXIT FOR
    END SELECT
  NEXT
  DO WHILE NoSplit% = false AND LEN(a$) > LineLength%
    'locate a place to split the line
    WrapPoint% = 0
    FOR I% = LineLength% TO LineLength% - 20 STEP -1
      SELECT CASE MID$(a$, I%, 1)
      CASE " ", ".", ",", ":", ";": WrapPoint% = I%: EXIT FOR
      END SELECT
    NEXT
    IF WrapPoint% = 0 THEN WrapPoint% = LineLength%
    'avoid wrapping on quote chars
    IF MID$(a$, WrapPoint% - 1, 1) = CHR$(34) THEN WrapPoint% = WrapPoint% - 1
    InQuote% = false: HasComment% = false
    'check to see if the line contains a remark
    FOR I% = 1 TO WrapPoint% - 1
      q$ = MID$(a$, I%, 1)
      IF q$ = CHR$(34) THEN
        InQuote% = NOT InQuote%
      ELSEIF InQuote% = false THEN
        IF q$ = "'" OR UCASE$(MID$(a$, I%, 4)) = "REM " THEN
          HasComment% = true: EXIT FOR
        END IF
      END IF
    NEXT
    NumLines% = NumLines% + 1
    IF InQuote% THEN
      Lines$(NumLines%) = LEFT$(a$, WrapPoint% - 1) + CHR$(34) + "+_"
    ELSE
      Lines$(NumLines%) = LEFT$(a$, WrapPoint% - 1) + "_"
    END IF
    a$ = MID$(a$, WrapPoint%)
    IF HasComment% THEN
      a$ = "'" + a$
    ELSEIF InQuote% THEN
      a$ = CHR$(34) + a$
    END IF
  LOOP
  NumLines% = NumLines% + 1: Lines$(NumLines%) = a$
END SUB

FUNCTION FASC% (a$)
  IF LEN(a$) = 0 THEN FASC% = -1 ELSE FASC% = ASC(a$)
END FUNCTION

FUNCTION GrabNum& (a$, Lower&, Upper&, Default&)
  FOR I% = 1 TO LEN(a$)
    q$ = MID$(a$, I%, 1): IF (q$ < "0" OR q$ > "9") THEN EXIT FOR
    J& = J& * 10& + ASC(q$) - 48
    IF J& > Upper& THEN GrabNum& = Default&: EXIT FUNCTION
  NEXT
  GrabNum& = J&: IF LEN(a$) = 0 OR J& < Lower& OR J& > Upper& THEN GrabNum& = Default&
END FUNCTION

SUB IIParse (cmd$, toname$, fromname$, conference%, BBSID$)
  ' this short sub parses cmd$ and returns values for use with ImportIt!
  FOR n = 1 TO LEN(cmd$)
    IF MID$(cmd$, n, 4) = " -Q " THEN
      qLoc = n + 4
    END IF
  NEXT n
  FOR n = qLoc TO LEN(cmd$)
    IF MID$(cmd$, n, 3) = "TO:" THEN
      toname$ = LTRIM$(RTRIM$(MID$(cmd$, n + 3, INSTR(n, cmd$, "FROM:") - (n + 3))))
    ELSEIF MID$(cmd$, n, 5) = "FROM:" THEN
      fromname$ = LTRIM$(RTRIM$(MID$(cmd$, n + 5, INSTR(n, cmd$, "CONF" + ":") - (n + 5))))
    ELSEIF MID$(cmd$, n, 5) = "CONF:" THEN
      conference% = VAL(LTRIM$(RTRIM$(MID$(cmd$, n + 5, INSTR(n, cmd$, "BBSID:") - (n + 5)))))
    ELSEIF MID$(cmd$, n, 6) = "BBSID:" THEN
      BBSID$ = LTRIM$(RTRIM$(LTRIM$(RTRIM$(MID$(cmd$, n + 6)))))
    END IF
  NEXT n
END SUB

SUB ImportIt (BBSID$, MsgFiles$(), toname$, fromname$, TitleFile$, conference)
  PRINT
  PRINT "ImportIt! v1.0"; CHR$(225); " QuickBASIC Compatable QWK format" + " file importer."
  PRINT "For use with PostIt! QuickBASIC Compatable Encoder/Decoder."
  PRINT "Public Domain by Calvin French, August 1993"
  PRINT
  PRINT "Adding encoded files to reply packet (.REP file)"
  AddToRep BBSID$, MsgFiles$(), toname$, fromname$, TitleFile$, conference, ErrorCode$
  PRINT
  PRINT "Status returned: "; ErrorCode$
  IF ErrorCode$ = "Reply packet (.REP file) not found!" THEN
    ArcMethod$ = PreferredArchiveMethod$
    SELECT CASE ArcMethod$
      CASE "ARJ"
        ArcCommand$ = "ARJ A"
      CASE "LHA"
        ArcCommand$ = "LHA A"
      CASE "ZIP"
        ArcCommand$ = "PKZIP"
    END SELECT
    CreateRep BBSID$, ArcCommand$
    AddToRep BBSID$, MsgFiles$(), toname$, fromname$, TitleFile$, conference, ErrorCode$
    PRINT "Status returned: "; ErrorCode$
    PRINT
  END IF
END SUB

'This parsing sub does NOT mistake filenames like "F-14G.ZIP" as
'containing a switch. That's why it looks so big.
SUB ParseCmdLine (cmd$, Params$(), Found%)
  Found% = 0: Sep$ = "-/": Temp$ = LTRIM$(RTRIM$(cmd$)): InParam% = 0
  FOR p% = 1 TO LEN(Temp$)
    C$ = MID$(Temp$, p%, 1)
    IF InParam% = -1 THEN 'Inside of a switch?
      IF INSTR(Sep$, C$) THEN 'Found another switch?
        'Terminate current switch, then start parsing the next one.
        GOSUB MakeParam: MID$(Temp$, p%, 1) = LEFT$(Sep$, 1)
        ParamStart% = p%
      ELSEIF ASC(C$) = 32 OR ASC(C$) = 9 THEN
        GOSUB MakeParam: InParam% = 0 'Terminate current switch.
      END IF
    ELSEIF InParam% = -2 THEN 'Inside of a parameter?
      IF ASC(C$) = 32 OR ASC(C$) = 9 THEN 'Terminate parameter with
        GOSUB MakeParam: InParam% = 0     'space or TAB.
      END IF
    ELSE
      IF INSTR(Sep$, C$) THEN 'Found start of a switch?
        'Make sure all switches start with "-".
        MID$(Temp$, p%, 1) = LEFT$(Sep$, 1): InParam% = -1
        ParamStart% = p%
      ELSEIF ASC(C$) <> 32 AND ASC(C$) <> 9 THEN 'If char isn't a
        InParam% = -2: ParamStart% = p% 'space or TAB it's a parameter.
      END IF
    END IF
  NEXT
 IF InParam% THEN GOSUB MakeParam
  EXIT SUB
MakeParam:
  Found% = Found% + 1
  Params$(Found%) = MID$(Temp$, ParamStart%, p% - ParamStart%)
  IF Found% = UBOUND(Params$) THEN EXIT SUB
RETURN
END SUB

SUB SepPath (a$, Drive$, path$, tName$)
  FOR I% = LEN(a$) TO 1 STEP -1
    IF INSTR("\:", MID$(a$, I%, 1)) THEN EXIT FOR
  NEXT
  IF I% > 0 THEN
    path$ = UCASE$(MID$(a$, 1, I%)): tName$ = UCASE$(MID$(a$, I% + 1))
  ELSE
    path$ = "": tName$ = UCASE$(a$)
  END IF
  Temp% = INSTR(path$, ":"): Drive$ = ""
  IF Temp% THEN Drive$ = LEFT$(path$, Temp%): path$ = MID$(path$, Temp% + 1)
END SUB

FUNCTION UnTab$ (B$, TabStops%)
  a$ = B$: T% = INSTR(a$, CHR$(9))
  IF T% THEN
    DO: Temp% = (T% - 1) MOD TabStops%
    a$ = LEFT$(a$, T% - 1) + SPACE$(TabStops% - Temp%) + MID$(a$, T% + 1)
    T% = INSTR(T%, a$, CHR$(9)): LOOP WHILE T%
  END IF
  UnTab$ = a$
END FUNCTION '(last subroutine)

