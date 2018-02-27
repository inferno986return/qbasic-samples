'_|_|_|   QUIKVIEW.BAS
'_|_|_|   A program to read and display ANSI files. Requires ANSI.SYS
'_|_|_|   No warrantees or guarantees are given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (8/2/96)
COLOR 14, 0: CLS
DO
   DIM buf(160) AS STRING * 1: LOCATE 1, 1: DEF SEG = &HB800
   FOR T% = 0 TO 159: buf(T%) = CHR$(PEEK(T%)): NEXT
   INPUT "Name of ANSI file to view => ", AnsiFile$
   FOR T% = 0 TO 159: POKE T%, ASC(buf(T%)): NEXT
   IF AnsiFile$ = "" THEN END
   ans% = FREEFILE: OPEN AnsiFile$ FOR BINARY AS #ans%
   scr% = FREEFILE: OPEN "CONS:" FOR OUTPUT AS #scr%
   WHILE NOT EOF(ans%)
      ansi$ = INPUT$(512, ans%): PRINT #scr%, ansi$;
      'it is important to make this 2 separate steps because
      'printing input directly to output voids the ANSI codes
   WEND
   CLOSE ans%: CLOSE scr%
   SOUND 999, 2               'notify that file has ended
   WHILE INKEY$ <> "": WEND   'clear keyboard buffer
   WHILE INKEY$ = "": WEND    'wait for a keypress
LOOP
'_|_|_|   end   QUIKVIEW.BAS
