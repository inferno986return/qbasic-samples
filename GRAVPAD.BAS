'_|_|_|   GRAVPAD.BAS
'_|_|_|   A short program to demontrate reading and responding
'_|_|_|   to data obtained from the Gravis PC GamePad.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (3/7/1998)
DECLARE SUB Calibrate ()
DECLARE SUB Gamepad ()
DECLARE SUB Dpad (x%, y%, c%)
SCREEN 12: DLA& = 0: Calibrate
JYX% = 320: JYY% = 240: Dpad JYX%, JYY%, 5: COLOR 12: over$ = ""
LOCATE 2, 14: PRINT "Test Gamepad now. Pressing any key quits program."
LOCATE , 15: PRINT "Use Left and Right Arrows to adjust Read Delay."
WHILE over$ = ""
   LOCATE 1, 1: PRINT DLA&; "  "
   B1% = 0: B2% = 0: B3% = 0: B4% = 0: Gamepad
   IF (Buttons% AND 1) THEN B1% = 4
   IF (Buttons% AND 2) THEN B2% = 1
   IF (Buttons% AND 4) THEN B3% = 14
   IF (Buttons% AND 8) THEN B4% = 2
   CIRCLE (160, 240), 50, B1%: CIRCLE (320, 120), 50, B2%
   CIRCLE (320, 360), 50, B3%: CIRCLE (480, 240), 50, B4%
   IF Gravis% <> 0 THEN
      Dpad JYX%, JYY%, 0
      IF Gravis% AND 1 THEN IF JYX% > 10 THEN JYX% = JYX% - 2
      IF Gravis% AND 2 THEN IF JYY% > 10 THEN JYY% = JYY% - 2
      IF Gravis% AND 4 THEN IF JYX% < 629 THEN JYX% = JYX% + 2
      IF Gravis% AND 8 THEN IF JYY% < 469 THEN JYY% = JYY% + 2
      Dpad JYX%, JYY%, 5
   END IF
   D$ = INKEY$
   IF (D$ <> CHR$(0) + CHR$(75)) AND (D$ <> CHR$(0) + CHR$(77)) THEN
      IF D$ <> "" THEN over$ = "over"
   ELSE
      IF (D$ = CHR$(0) + CHR$(75)) THEN DLA& = DLA& + (DLA& > 0)
      IF (D$ = CHR$(0) + CHR$(77)) THEN DLA& = DLA& - (DLA& < 200)
   END IF
WEND
SCREEN 0
SUB Calibrate
   SHARED Gravis%, DLA&
   DLA& = 0: CumDLA& = 0: c% = 1
   COLOR 15: LOCATE 12, 27: PRINT "[][][][][][][][][][][][][][]"
   PRINT : LOCATE , 27: PRINT "[][][][][][][][][][][][][][]"
   FOR redundanceCheck% = 1 TO 10
      COLOR c%: LOCATE 13, 28: PRINT "Calibrating Gravis GamePad"
      CumDLA& = CumDLA& + DLA&: DLA& = 0
      DO
         DLA& = DLA& + 1: G% = 0
         FOR L% = 1 TO 10: Gamepad: G% = G% + Gravis%: NEXT
      LOOP WHILE G% > 0: c% = (c% MOD 14) + 1
   NEXT: DLA& = CumDLA& \ 5: CLS
END SUB
SUB Dpad (x%, y%, c%)
   FOR t% = 4 TO 10 STEP 2: CIRCLE (x%, y%), t%, c%: NEXT
END SUB
SUB Gamepad
   '_|_|_|   RETURNS: Buttons%
   '_|_|_|   button 1 (AND 1): button 2 (AND 2)
   '_|_|_|   button 3 (AND 4): button 4 (AND 8)
   '_|_|_|   RETURNS: Gravis%
   '_|_|_|   left (AND 1): up (AND 2): right (AND 4): down (AND 8)
   SHARED Buttons%, Gravis%, DLA&
   OUT &H201, 0: Buttons% = 15 - (INP(&H201) \ 16)
   FOR t& = 0 TO (DLA& \ 2): NEXT
   Gravis% = 3 - (INP(&H201) AND 3)
   '_|_|_|   A short delay is needed to read up/left, and then
   '_|_|_|   another is needed to read down/right. These values
   '_|_|_|   are arrived at by the Calibrate SUB
   FOR t& = 0 TO DLA&: NEXT
   Gravis% = Gravis% + 4 * (INP(&H201) AND 3)
END SUB
'_|_|_|   end   GRAVPAD.BAS
