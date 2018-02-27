SCREEN 7
FOR t% = 0 TO 6
   SCREEN , , t%, 0   'set to write to t% and view 0
   COLOR t% + 9
   PRINT " This is page"; t%
   PRINT " Hit a key for page"; (t% + 1) MOD 7
   PRINT " Hit ESC to exit "
NEXT
DO
   DO: k$ = INKEY$: LOOP WHILE k$ = ""
   IF k$ = CHR$(27) THEN EXIT DO
   t% = (t% + 1) MOD 7
   SCREEN , , , t%   'set to view t%
LOOP
SCREEN , , 0, 0      'set 0 active for both write and view
CLS :
PRINT "Now we will copy pages to page 0."
PRINT "Hit any key to begin..."
DO: k$ = INKEY$: LOOP WHILE k$ = ""
FOR t% = 1 TO 6
   FOR dla% = 1 TO 70
      WAIT &H3DA, 8: WAIT &H3DA, 8, 8
   NEXT
   PCOPY t%, 0   'PageCopy t% to 0
NEXT
LOCATE 10, 1: PRINT " Hit any key to exit"
DO: k$ = INKEY$: LOOP WHILE k$ = ""
SCREEN 0: WIDTH 80, 25

