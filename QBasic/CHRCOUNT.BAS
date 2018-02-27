' [=-----=]   -begin-   CHRCOUNT.BAS   [=-----=]
' [  This program will search any file for instances of
' [  a given character and report the number of times
' [  the number was found. (Includes a progress bar SUB :)
' [  Released to the Public Domain    by     Kurt Kuzba
' [=-----=] [=-----=] [=-----=] [=-----=] [=-----=] [=-----=]
DECLARE SUB Progress (Total&, Left&)
ON ERROR GOTO OOps
name$ = "": COLOR 2, 0: CLS : LOCATE 3, 1
INPUT " Enter file name => ", name$
IF name$ = "" THEN END
INPUT " ASCII value of character to find => ", char%
IF char% = 0 THEN END:  ELSE char$ = CHR$(char%)
OPEN name$ FOR BINARY AS #1
flen& = LOF(1)
PRINT flen&; "bytes in "; name$
bytesread& = 0
Hits& = 0
PRINT TIME$
WHILE NOT EOF(1)
   test$ = INPUT$(4096, 1): GOSUB TestString
WEND
CLOSE 1: PRINT " Hits ="; Hits&: PRINT TIME$: END
TestString:
   pointer% = 0
   WHILE INSTR(pointer% + 1, test$, char$) <> 0
      pointer% = INSTR(pointer% + 1, test$, char$)
      Hits& = Hits& + 1
   WEND
   bytesread& = bytesread& + LEN(test$)
   Progress flen&, bytesread&
   RETURN
OOps:
 PRINT " Disk error or bad file name ["; ERR; "]"
 CLOSE 1: END
' [=-----=]   --end--   CHRCOUNT.BAS   [=-----=]

SUB Progress (Total&, BRead&)
x = CSRLIN: y = POS(0)
COLOR 15, 5: LOCATE 1, 1: PRINT "*";
done% = FIX((BRead& / Total&) * 70)
display$ = STRING$(70, " ")
MID$(display$, 1) = STRING$(done%, "°")
COLOR 14, 4: PRINT display$; : COLOR 15, 5
PRINT "*"; RIGHT$("   " + STR$(FIX(done% * 1.438)), 4); "%";
COLOR 2, 0: LOCATE x, y
END SUB

