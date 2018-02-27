'_|_|_|   EXE_APND.BAS
'_|_|_|   Code to append data files to an existing .EXE
'_|_|_|   No warrantee or guarantee given or implied.
'_|_|_|   PUBLIC DOMAIN   by Kurt Kuzba.  (7/11/1997)
ON ERROR GOTO BooBoo
INPUT "Enter Program File name => ", ExeName$
IF ExeName$ = "" THEN SYSTEM
FError$ = "OK": exe% = FREEFILE: OPEN ExeName$ FOR INPUT AS #exe%
IF FError$ <> "OK" THEN
   PRINT "unable to open "; ExeName$; : SYSTEM
END IF
CLOSE exe%: OPEN ExeName$ FOR BINARY AS #exe%
INPUT "Enter File to Append => ", AppName$
IF AppName$ = "" THEN CLOSE : SYSTEM
app% = FREEFILE: OPEN AppName$ FOR INPUT AS #app%
IF FError$ <> "OK" THEN
   CLOSE exe%: PRINT "unable to open "; AppName$; : SYSTEM
END IF
CLOSE app%: OPEN AppName$ FOR BINARY AS #app%
FileEnd& = LOF(exe%) + 1: AppSize% = INT(LOF(app%) \ 1024)
Iteration% = 0: PRINT "DONE";
WHILE NOT EOF(app%)
   LOCATE , 5: PRINT STR$(INT((Iteration% / AppSize%) * 100)); "%";
   dat$ = INPUT$(1024, app%)
   PUT #exe%, FileEnd&, dat$
   FileEnd& = FileEnd& + LEN(dat$)
   Iteration% = Iteration% + 1
WEND
SOUND 1000, 1
CLOSE
SYSTEM
BooBoo: FError$ = STR$(ERR): RESUME NEXT
'_|_|_|   end   EXE_APND.BAS

