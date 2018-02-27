'_|_|_|   KBARR.BAS
'_|_|_|   A program to scan the keyboard, as for game programs.
'_|_|_|   No guarantee or warrantee is given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (3/28/96)
DECLARE SUB kbarray ()
DIM SHARED KB(129) AS INTEGER
COLOR 15, 1: CLS : COLOR 14, 4: LOCATE 12, 3, 0
PRINT " Test keys now. Hit ESC to exit program"
COLOR 15, 1: LOCATE 1, 4
FOR t% = 1 TO 10: PRINT (t% MOD 10); " "; : NEXT: PRINT
FOR t% = 0 TO 9: PRINT t%: NEXT
WHILE KB(1) = 0
   LOCATE 2, 1
   FOR x% = 0 TO 9
      LOCATE , 5
      FOR y% = 1 TO 10
         kbarray
         PRINT MID$(".*", KB(x% * 10 + y%) + 1, 1) + "   ";
      NEXT: PRINT
   NEXT
WEND: END
'_|_|_|   end   KBARR.BAS

SUB kbarray
  x% = INP(96)
  IF x% AND 128 THEN KB(x% AND 127) = 0:  ELSE KB(x%) = 1
  WHILE INKEY$ <> "": WEND
END SUB

