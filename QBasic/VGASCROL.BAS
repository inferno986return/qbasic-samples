'_|_|_|   VGASCROL.BAS   PUBLIC DOMAIN   by Kurt Kuzba.  (5/21/1997)
'_|_|_|   A SUB to provide smoothly scrolling text in VGA.
'_|_|_|   If other modes are used, screen coordinates must be
'_|_|_|   adjusted to the different screen dimensions.
DECLARE SUB MsgScroll (M$, c%, l%, s%, b%)
SCREEN 12
DIM SHARED ScrWide AS INTEGER, ScrHigh AS INTEGER, CharHigh AS INTEGER
ScrWide = 640: ScrHigh = 480: CharHigh = 16
LINE (0, 0)-(ScrWide - 1, ScrHigh - 1), 1, BF
msg$ = "This is only a test message. It really doesn't say much."
msg$ = msg$ + " It only serves to illustrate the working of the "
msg$ = msg$ + "smooth text scroll in mode 12h. You could also use"
msg$ = msg$ + " it in mode 9h, 11h, 13h, or any supported mode."
msg$ = msg$ + "     ***   hit any key to exit   ***     "
MsgScroll msg$, 14, 3, 1, 4
msg$ = "This message is only here to illustrate that the scroll "
msg$ = msg$ + "function will remove any trace of itself upon exi"
msg$ = msg$ + "ting the subroutine. You just feed it the proper "
msg$ = msg$ + "color to fill in with. You also provide the borde"
msg$ = msg$ + "r color for the scrolling display."
msg$ = msg$ + "     ***   hit any key to exit   ***     "
MsgScroll msg$, 10, 20, 1, 15
WIDTH , 60: CharHigh = 8
MsgScroll msg$, 15, 40, 0, 13
SCREEN 9: ScrHigh = 350: CharHigh = 14
MsgScroll msg$, 9, 10, 0, 11
WIDTH , 43: CharHigh = 8
MsgScroll msg$, 9, 10, 0, 11
SCREEN 0: WIDTH 80, 25

SUB MsgScroll (M$, c%, l%, s%, b%)
   DIM buf(2600) AS INTEGER, msglen AS INTEGER, pointer AS INTEGER
   msglen = LEN(M$)
   scroll = 0
   msgPointer = 0
   x% = l% * CharHigh - CharHigh
   IF x% > 0 THEN LINE (0, x% - 1)-(ScrWide - 1, x% - 1), b%
   IF x% < 624 THEN
      LINE (0, x% + CharHigh)-(ScrWide - 1, x% + CharHigh), b%
   END IF
   COLOR c%
   DO
      scroll% = (scroll% + 1) MOD 4
      GET (2, x%)-(ScrWide - 1, x% + CharHigh - 1), buf
      WAIT &H3DA, 8: WAIT &H3DA, 8, 8
      PUT (0, x%), buf, PSET
      IF scroll% = 0 THEN
         LOCATE l%, 80
         PRINT MID$(M$, pointer + 1, 1);
         pointer = (pointer + 1) MOD msglen
      ELSE
         LINE (ScrWide - 2, x%)-(ScrWide - 1, x% + CharHigh - 1), 0, BF
      END IF
   LOOP WHILE INKEY$ = ""
   c% = x%
   IF x% > 0 THEN c% = x% - 1
   l% = x% + CharHigh - 1
   IF (x% + CharHigh) < ScrHigh THEN l% = x% + CharHigh
   LINE (0, c%)-(ScrWide - 1, l%), s%, BF
END SUB

