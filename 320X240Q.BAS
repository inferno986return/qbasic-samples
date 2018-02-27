'From: DAVE SHEA                 Conference: QUIK_BAS -Qui (298)
'      Digital Encounters * Kamloops BC Canada 250/374-6168 (1:353/710)
'Example of how to achieve ModeX in QuickBasic, from Douglas Lusher
'Modified to show QBasic example by Kurt Kuzba, (5/23/1997)
DECLARE SUB XCLS (Page%)
DECLARE SUB ShowPage (Page%)
DECLARE SUB Set320x240mode ()
DECLARE SUB PutPixel (X%, Y%, Culler%, Page%)
DIM BitMask%(7): FOR Bit% = 0 TO 7: BitMask%(Bit%) = 2 ^ Bit%: NEXT
SCREEN 0: WIDTH 80, 25: COLOR 10, 0: LOCATE 5, 10
PRINT "Hit any key to enter ModeX. 0-1-2 for pages. ESC exits."
WHILE INKEY$ = "": WEND: Set320x240mode: SLEEP 1
HMax% = 320: VMax% = 240: Pg% = 0
FOR Scrpage% = 0 TO 2
   C% = 10: IF Scrpage% > 0 THEN C% = 12: IF Scrpage% > 1 THEN C% = 13
   XCLS Scrpage%: ShowPage Scrpage%
   FOR Y% = Scrpage% * 9 TO VMax% - 1 - Scrpage% * 9
	  PutPixel Scrpage% * 9, Y%, C%, Scrpage%
	  PutPixel HMax% - 1 - Scrpage% * 9, Y%, C%, Scrpage%
   NEXT
   FOR X% = Scrpage% * 9 TO HMax% - 1 - Scrpage% * 9
	  CALL PutPixel(X%, Scrpage% * 9, C%, Scrpage%)
	  CALL PutPixel(X%, VMax% - 1 - Scrpage% * 9, C%, Scrpage%)
   NEXT
NEXT
DO
   DO: k$ = INKEY$: LOOP WHILE k$ = ""
   SELECT CASE k$
	  CASE "0": ShowPage 0
	  CASE "1": ShowPage 1
	  CASE "2": ShowPage 2
	  CASE CHR$(27): EXIT DO
	  CASE ELSE: SOUND 999, .5
   END SELECT
LOOP: SCREEN 13: SCREEN 0: WIDTH 80, 25: END

SUB GetPixel (X%, Y%, Culler%, Page%)
   SELECT CASE Page%
	  CASE 0: VidSegment% = &HA000
	  CASE 1: VidSegment% = &HA4F0
	  CASE 2: VidSegment% = &HA9E0
	  CASE ELSE: ERROR 5
   END SELECT
   OUT &H3CE, 4: OUT &H3CF, X% AND 3
   DEF SEG = VidSegment%
   Culler% = PEEK((Y% * 80) + (X% \ 4))
END SUB

SUB PutPixel (X%, Y%, Culler%, Page%)
   SHARED BitMask%()
   SELECT CASE Page%
	  CASE 0: VidSegment% = &HA000
	  CASE 1: VidSegment% = &HA4F0
	  CASE 2: VidSegment% = &HA9E0
	  CASE ELSE: ERROR 5
   END SELECT
   OUT &H3C4, 2: OUT &H3C5, BitMask%(X% AND 3)
   DEF SEG = VidSegment%
   POKE (Y% * 80) + (X% \ 4), Culler%
END SUB

SUB Set320x240mode
   SCREEN 13                          'begin with standard mode 13h
   OUT &H3C4, &H4: OUT &H3C5, &H6     'disable "chain4" mode
   OUT &H3C4, &H2: OUT &H3C5, &HF     'enable writes to all four planes
   CLS                                'clear video memory
   OUT &H3C4, 0: OUT &H3C5, &H1       'synchronous reset while
									  'switching clocks
   OUT &H3C2, &HE3                    'select 25 Mhz dot clock and
									  '60 hz scanning rate
   OUT &H3C4, 0: OUT &H3C5, &H3       'restart the sequencer
   OUT &H3D4, &H11: OUT &H3D5, INP(&H3D5) AND &H7F
	  'to reprogram the CRT controller, remove write protect from
	  'the registers
   OUT &H3D4, &H6: OUT &H3D5, &HD     'total vertical pixels
   OUT &H3D4, &H7: OUT &H3D5, &H3E    'overflow
   OUT &H3D4, &H9: OUT &H3D5, &H41    'turn off double double-scan
   OUT &H3D4, &H10: OUT &H3D5, &HEA   'vertical sync start
   OUT &H3D4, &H11: OUT &H3D5, &HAC   'vertical sync end,
									  'reprotect_registers
   OUT &H3D4, &H12: OUT &H3D5, &HDF   'vertical pixels displayed
   OUT &H3D4, &H14: OUT &H3D5, 0      'turn off dword mode
   OUT &H3D4, &H15: OUT &H3D5, &HE7   'vertical blank start
   OUT &H3D4, &H16: OUT &H3D5, &H6    'vertical blank end
   OUT &H3D4, &H17: OUT &H3D5, &HE3   'turn on byte mode
END SUB

SUB ShowPage (Page%)
   SELECT CASE Page%
	  CASE 0: OUT &H3D4, &HC: OUT &H3D5, 0
	  CASE 1: OUT &H3D4, &HC: OUT &H3D5, &H4F
	  CASE 2: OUT &H3D4, &HC: OUT &H3D5, &H9E
	  CASE ELSE: ERROR 5          'illegal function call
   END SELECT
END SUB

SUB XCLS (Page%)
   SELECT CASE Page%
	  CASE 0: VidSegment% = &HA000
	  CASE 1: VidSegment% = &HA4F0
	  CASE 2: VidSegment% = &HA9E0
	  CASE ELSE: ERROR 5
   END SELECT
   OUT &H3C4, &H2: OUT &H3C5, &HF
   DEF SEG = VidSegment%
   FOR Address% = 0 TO 19199: POKE Address%, 0: NEXT
END SUB

