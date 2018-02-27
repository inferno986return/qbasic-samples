DECLARE FUNCTION FindDigit% (Text AS STRING)
DECLARE FUNCTION FindNonDigit% (Text AS STRING)
TestString$ = "abc12345Hello World!56789"
digBgn% = FindDigit(TestString$)
digEnd% = FindNonDigit(MID$(TestString$, digBgn%)) + digBgn% - 1
PRINT VAL(MID$(TestString$, digBgn%, digEnd% - digBgn%))
txtEnd% = FindDigit(MID$(TestString$, digEnd%)) + digEnd% - 1
PRINT MID$(TestString$, digEnd%, txtEnd% - digEnd%)
digEnd% = FindNonDigit(MID$(TestString$, txtEnd%)) + txtEnd% - 1
PRINT VAL(MID$(TestString$, txtEnd%, digEnd% - txtEnd%))


 

FUNCTION FindDigit% (Text AS STRING)
   TextLength% = LEN(Text)
   FOR T% = 1 TO TextLength%
      IF INSTR("1234567890", MID$(Text, T%, 1)) > 0 THEN EXIT FOR
   NEXT
   FindDigit% = T%
END FUNCTION

FUNCTION FindNonDigit% (Text AS STRING)
   TextLength% = LEN(Text)
   FOR T% = 1 TO TextLength%
      IF INSTR("1234567890", MID$(Text, T%, 1)) = 0 THEN EXIT FOR
   NEXT
   FindNonDigit% = T%
END FUNCTION

