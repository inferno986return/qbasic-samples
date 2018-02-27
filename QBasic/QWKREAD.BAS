'============================================================================
'     To: Robert Kohl                   Number: 59       Refer: 0023 
'   From: Isaac Grover              Conference: FidoQBasic   (139)
'   Date: 02-04-97 14:23              BBS Name: Exec-PC
'Subject: QWKREAD.BAS (1/3)             BBS ID: EXECPC
'----------------------------------------------------------------------------

'Douglas Lusher @ (1:282/7)
 DECLARE SUB Unarchive (FileName$)
 DECLARE FUNCTION FileExists% (FileName$)
 DECLARE FUNCTION TempDirectory$ ()
 DECLARE FUNCTION FileSize& (FileName$)
'$INCLUDE: 'qb.bi'
 TYPE MessageHeaderType
	MsgStatus AS STRING * 1
	MsgNumber AS STRING * 7
	MsgDate AS STRING * 8
	MsgTime AS STRING * 5
	MsgTo AS STRING * 25
	MsgFrom AS STRING * 25
	MsgSubject AS STRING * 25
	Password AS STRING * 12      'seldom/never used
	Reference AS STRING * 8
	NumBlocks AS STRING * 6      'number of 128 byte blocks in msg
	KillFlag AS STRING * 1
	Conference AS INTEGER        'Conference number
	NotUsed AS STRING * 2
	NetworkTag AS STRING * 1
 END TYPE
 DIM Header AS MessageHeaderType
 DEFINT A-Z
 QwkFile$ = RTRIM$(COMMAND$)
 IF LEN(QwkFile$) = 0 THEN
	PRINT "Enter the .QWK file to read: "; : LINE INPUT QwkFile$
	QwkFile$ = LTRIM$(RTRIM$(UCASE$(QwkFile$)))
	IF LEN(QwkFile$) = 0 THEN END
 END IF
 IF RIGHT$(QwkFile$, 4) <> ".QWK" THEN
	QwkFile$ = QwkFile$ + ".QWK"
 END IF
 IF NOT FileExists(QwkFile$) THEN
	PRINT "Unable to locate: "; QwkFile$: END
 END IF
 TempDir$ = TempDirectory$
 MKDIR TempDir$
 CHDIR TempDir$
 CALL Unarchive(QwkFile$)
 MsgFile$ = "MESSAGES.DAT"
 IF NOT FileExists(MsgFile$) THEN
	PRINT "Error while unpacking file": GOTO ProgramEnd
 END IF
 IF FileSize&(MsgFile$) = 0 THEN
	PRINT "Error while unpacking file": GOTO ProgramEnd
 END IF
 OPEN MsgFile$ FOR BINARY AS #1
	SEEK #1, 129
	DO
		GET #1, , Header
		IF SEEK(1) >= LOF(1) - 127 THEN EXIT DO
		OPEN "TEMP-MSG.FIL" FOR OUTPUT AS #2
			PRINT #2, "Conference: "; Header.Conference,
			PRINT #2, "Message #"; Header.MsgNumber
			PRINT #2, "Date: "; Header.MsgDate,
			PRINT #2, "Time: "; Header.MsgTime
			PRINT #2, "  To: "; Header.MsgTo
			PRINT #2, "From: "; Header.MsgFrom
			PRINT #2, "Subj: "; Header.MsgSubject
			PRINT #2, STRING$(80, 205)
			MsgLength% = (VAL(Header.NumBlocks) - 1) * 128
			MsgBody$ = INPUT$(MsgLength%, #1)
			MsgBody$ = RTRIM$(MsgBody$)
			StartPtr% = 1
			DO
				EndPtr% = INSTR(StartPtr%, MsgBody$, CHR$(227))
				IF EndPtr% THEN
					PRINT #2, MID$(MsgBody$, StartPtr%, EndPtr% - StartPtr%)
					StartPtr% = EndPtr% + 1
				ELSE
					EXIT DO
				END IF
			LOOP
		CLOSE #2
		SHELL "TYPE TEMP-MSG.FIL | MORE"
		PRINT "Continue? (Y/N)"
		A$ = INPUT$(1)
		IF UCASE$(A$) <> "Y" THEN EXIT DO
	LOOP
 CLOSE #1
ProgramEnd:
 KILL "*.*"
 CHDIR ".."
 RMDIR TempDir$
 END

'Isaac Grover (1:214/80) <isaac.grover@dinuba.com>
'Official QUIKC:\>PROMPT $la href="mailto:isaac.grover@dinuba.com"$gMe!$l/a$g
'--- QScan/PCB v1.17b / 01-0154
' * Origin: MicroLink BBS * Dinuba, CA 209-591-8753 (1:214/80)

 FUNCTION FileExists% (FileName$)
 DIM XRegister AS RegTypeX
 'save the current DTA
 XRegister.ax = &H2F00
 CALL INTERRUPTX(&H21, XRegister, XRegister)
 OldDTASeg% = XRegister.es
 OldDTAOff% = XRegister.bx
 'set up a new DTA
 DTA$ = SPACE$(43)
 XRegister.ax = &H1A00
 XRegister.ds = VARSEG(DTA$)
 XRegister.dx = SADD(DTA$)
 CALL INTERRUPTX(&H21, XRegister, XRegister)
 'get first matching file
 Temp$ = FileName$ + CHR$(0)
 XRegister.ax = &H4E00
 XRegister.cx = &H6
 XRegister.ds = VARSEG(Temp$)
 XRegister.dx = SADD(Temp$)
 CALL INTERRUPTX(&H21, XRegister, XRegister)
 'if the carry flag is clear then the file exists
 FileExists% = ((XRegister.flags AND 1) = 0)
 'restore the old DTA
 XRegister.ax = &H1A00
 XRegister.ds = OldDTASeg%
 XRegister.dx = OldDTAOff%
 CALL INTERRUPTX(&H21, XRegister, XRegister)
END FUNCTION

 FUNCTION FileSize& (FileName$)
 File% = FREEFILE
 OPEN FileName$ FOR BINARY AS #File
	 FileSize& = LOF(File%)
 CLOSE #File
 END FUNCTION

 FUNCTION TempDirectory$
 RANDOMIZE TIMER
 Work$ = SPACE$(12)
 DO
	FOR Ptr% = 1 TO 12
		Char$ = CHR$(65 + INT(RND * 26))
		MID$(Work$, Ptr%, 1) = Char$
	NEXT
	MID$(Work$, 9, 1) = "."
 LOOP WHILE FileExists(Work$ + "\NUL")
 TempDirectory$ = Work$
 END FUNCTION

SUB Unarchive (FileName$)
 File% = FREEFILE
 OPEN FileName$ FOR BINARY AS #File
	Marker1$ = SPACE$(5)
	Marker2$ = SPACE$(1)
	GET #File, 1, Marker1$
	GET #File, LOF(File), Marker2$
 CLOSE #File
 UnarchiveCmd$ = ""
 IF LEFT$(Marker1$, 4) = "PK" + CHR$(3) + CHR$(4) THEN
	UnarchiveCmd$ = "PKUNZIP -o "
 ELSEIF RIGHT$(Marker1$, 3) = "-lh" THEN
	UnarchiveCmd$ = "LHA e "
 ELSEIF LEFT$(Marker1$, 2) = CHR$(39) + CHR$(234) THEN
	UnarchiveCmd$ = "ARJ e "
 ELSEIF LEFT$(Marker1$, 3) = "ZOO" THEN
	UnarchiveCmd$ = "ZOO e "
 ELSEIF Marker2$ = CHR$(254) THEN
	UnarchiveCmd$ = "PAK e /wa "
 ELSEIF LEFT$(Marker1$, 1) = CHR$(26) THEN
	UnarchiveCmd$ = "PKUNPAK /r "
 END IF
 IF LEN(UnarchiveCmd$) THEN
	SHELL UnarchiveCmd$ + FileName$
 ELSE
	PRINT "Unable to identify archive type"
 END IF
END SUB

