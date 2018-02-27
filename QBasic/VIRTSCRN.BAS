'============================================================================
'     To: All                           Number: 10689    Refer: 0     
'   From: Douglas Lusher            Conference: Quik_Bas (3)
'   Date: 10-04-96 09:41              BBS Name: The Programmer's Mark BBS
'Subject: panning the VGA       1/2     BBS ID: progmark
'----------------------------------------------------------------------------
'
' Greetings, fellow BASIC programmers.
' I just finished working out how to do pixel-by-pixel scrolling
' on a VGA card and I thought that this might interest some of
' you, so here's a quick demo. What this code does is this: it
' creates a virtual screen much larger than the usual SCREEN 12
' 640x480, then prints graphics to this larger virtual screen.
' When you press a key, the graphics stop being printed and you
' can use the arrow keys to pan around and observe different
' parts of the virtual screen.
'
' When the VGA card sets up a screen mode, the Offset Register
' &H3D5, index &H13 contains the amount of display memory to be
' used per horizontal scan line. The virtual screen is made wider
' than the ordinary screen by increasing this value. The Start
' Address High and Start Address Low registers, &H3D5 index &HC
' & &HD contain the MSB and LSB respectively of the address in
' video memory where the displayed screen begins. Increasing or
' decreasing this value by 1 pans the screen left or right by
' 8 pixels. Changing this value by the amount of memory per scan
' line pans the screen up or down. Notice that horizontal panning
' by this method alone moves the screen 8 pixels at a time. To
' pan horizontally a pixel at a time involves the Horizontal
' PEL Panning register &H3C0 index &H13. Rotating the values of
' this register from 0 through 7 pans the screen pixel-column
' by pixel-column. Note that this register must only be changed
' during a vertical retrace and that an INP(&H3DA) command must
' preceed a change to this register. Also, bit 5 must always be
' set.
'
' It is worth noting also that the normal BASIC graphics commands
' will not work on the larger virtual screen, so I have provided
' my own code to create the graphics in this demo. While the code
' for these graphics commands is good, it is just BASIC and is
' not necessarily optimised, so I realized that it is slow. That
' particular aspect is not the focus of this demo.

 DECLARE SUB QPrint (X%, Y%, Text$, Culler%)
 DECLARE SUB DrawCircle (X%, Y%, Radius%, Culler%)
 DECLARE SUB PutPixel (X%, Y%, Culler%)
 DECLARE SUB DrawLine (X1%, Y1%, X2%, Y2%, Culler%)
 DEFINT A-Z
 REDIM BitMask%(0 TO 7)
 Mask% = 128
 FOR Bit% = 0 TO 7
   BitMask%(Bit%) = Mask%: Mask% = Mask% \ 2
 NEXT

 RANDOMIZE TIMER
 SCREEN 12: ScrnWid% = 640: ScrnHgt% = 480

 'the width of the virtual screen in pixels:
 VScrnWid% = 832
 'In SCREEN 12, the following must be true:
 '  VScrnWid% must be equal to or greater than 640
 '  VScrnWid% must be less than or equal to 1088
 '  VScrnWid% must be evenly divisible by 16
 HPanLimit% = VScrnWid% - ScrnWid%
 BytesPerLine% = VScrnWid% \ 8

 'the height of the virtual screen in pixels:
 VScrnHgt% = 624
 'In SCREEN 12, the following must be true:
 '  VScrnHgt% must be equal to or greater than 480
 '  VScrnHgt% must be less than or equal to 819
 '  The product of VScrnHgt% multiplied by VScrnWid% must be less
 '      than or equal to 524288
 VPanLimit% = VScrnHgt% - ScrnHgt%

 'create the virtual screen:
 OUT &H3D4, &H13: OUT &H3D5, VScrnWid% \ 16

 'put up some graphics
 MinX% = 0: MaxX% = VScrnWid% - 1
 MinY% = 0: MaxY% = VScrnHgt% - 1
 CALL DrawLine(MinX%, MinY%, MaxX%, MinY%, 1)
 CALL DrawLine(MaxX%, MinY%, MaxX%, MaxY%, 2)
 CALL DrawLine(MaxX%, MaxY%, MinX%, MaxY%, 3)
 CALL DrawLine(MinX%, MaxY%, MinX%, MinY%, 4)

 RR% = VScrnHgt%: IF VScrnWid% < VScrnHgt% THEN RR% = VScrnWid%
 DO UNTIL LEN(INKEY$)
   X1% = INT(RND * VScrnWid%)
   Y1% = INT(RND * VScrnHgt%)
   X2% = INT(RND * VScrnWid%)
   Y2% = INT(RND * VScrnHgt%)
   C% = INT(RND * 14) + 1
   CALL DrawLine(X1%, Y1%, X2%, Y2%, C%)
   R% = INT(RND * (RR% \ 4))
   X1% = INT(RND * (VScrnWid% - R% * 2)) + R%
   Y1% = INT(RND * (VScrnHgt% - R% * 2)) + R%
   C% = INT(RND * 14) + 1
   CALL DrawCircle(X1%, Y1%, R%, C%)
 LOOP
 CALL QPrint(8, 8, "Upper Left", 15)
 CALL QPrint(VScrnWid% - 96, 8, "Upper Right", 15)
 CALL QPrint(8, VScrnHgt% - 16, "Lower Left", 15)
 CALL QPrint(VScrnWid% - 96, VScrnHgt% - 16, "Lower Right", 15)

 'allow the user to pan around the virtual screen
 X% = 0: Y% = 0
 DO
  DO WHILE LEN(INKEY$): LOOP
  DO: KP$ = INKEY$: LOOP UNTIL LEN(KP$)
  KP% = ASC(KP$): IF KP% = 0 THEN KP% = -ASC(MID$(KP$, 2))
  SELECT CASE KP%
    CASE 27
      EXIT DO
    CASE 52, -75  'Four, LArrow
      IF X% > 0 THEN
        X% = X% - 1
        DO: LOOP WHILE (INP(&H3DA) AND 8) = 0
        OUT &H3C0, &H33: OUT &H3C0, X% MOD 8
      ELSE
        BEEP
      END IF
    CASE 54, -77   'Six, RArrow
      IF X% < HPanLimit% THEN
        X% = X% + 1
        DO: LOOP WHILE (INP(&H3DA) AND 8) = 0
        OUT &H3C0, &H33: OUT &H3C0, X% MOD 8
      ELSE
        BEEP
      END IF
    CASE 56, -72  'Eight, UpArrow
      IF Y% > 0 THEN
        Y% = Y% - 1
      ELSE
        BEEP
      END IF
    CASE 50, -80  'Two, DnArrow
      IF Y% < VPanLimit% THEN
        Y% = Y% + 1
      ELSE
        BEEP
      END IF
  END SELECT
  Ptr% = (Y% * BytesPerLine%) + (X% \ 8)
  OUT &H3D4, &HD: OUT &H3D5, Ptr% MOD 256
  OUT &H3D4, &HC: OUT &H3D5, Ptr% \ 256
 LOOP
 SCREEN 0: WIDTH 80
 END

 SUB DrawCircle (X%, Y%, Radius%, Culler%)
 'a routine to draw circles using only integers and integer math
 'an implementation of Bresenham's algorithm
 'by Douglas H. Lusher, 05-09-1996
 a% = 0
 B% = Radius%
 D% = (1 - Radius%) * 2
 XX1% = X%: YY1% = Y% + B%
 XX2% = X%: YY2% = Y% - B%
 XY1% = X% + B%: YX1% = Y%
 XY2% = X% - B%: YX2% = Y%
 DO WHILE B% >= a%
   CALL PutPixel(XX1%, YY1%, Culler%)
   CALL PutPixel(XX1%, YY2%, Culler%)
   CALL PutPixel(XX2%, YY1%, Culler%)
   CALL PutPixel(XX2%, YY2%, Culler%)
   CALL PutPixel(XY1%, YX1%, Culler%)
   CALL PutPixel(XY1%, YX2%, Culler%)
   CALL PutPixel(XY2%, YX1%, Culler%)
   CALL PutPixel(XY2%, YX2%, Culler%)
   IF D% + B% > 0 THEN
     B% = B% - 1
     D% = D% - (B% * 2) + 1
     YY1% = YY1% - 1: YY2% = YY2% + 1
     XY1% = XY1% - 1: XY2% = XY2% + 1
   END IF
   IF a% > D% THEN
     a% = a% + 1
     D% = D% + (a% * 2) + 1
     XX1% = XX1% + 1: XX2% = XX2% - 1
     YX1% = YX1% + 1: YX2% = YX2% - 1
   END IF
 LOOP
 END SUB

 SUB DrawLine (X1%, Y1%, X2%, Y2%, Culler%)
 'a routine to draw lines using only integers and integer math
 'an implementation of Bresenham's algorithm
 'by Douglas H. Lusher, 05-08-1996
 a% = X2% - X1%
 B% = Y2% - Y1%
 DX2% = 1: DY2% = 1
 IF a% < 0 THEN a% = -a%: DX2% = -1
 IF B% < 0 THEN B% = -B%: DY2% = -1
 DX1% = DX2%: DY1% = 0
 IF a% < B% THEN SWAP a%, B%: DX1% = 0: DY1% = DY2%
 I1% = B% * 2
 D% = I1% - a%
 I2% = D% - a%
 X% = X1%: Y% = Y1%
 FOR I% = 0 TO a%
  CALL PutPixel(X%, Y%, Culler%)
  IF D% < 0 THEN
    X% = X% + DX1%
    Y% = Y% + DY1%
    D% = D% + I1%
  ELSE
    X% = X% + DX2%
    Y% = Y% + DY2%
    D% = D% + I2%
  END IF
 NEXT
 END SUB

 SUB PutPixel (X%, Y%, Culler%)
 SHARED BitMask%(), BytesPerLine%
 DEF SEG = &HA000
 Offset& = (CLNG(Y%) * BytesPerLine%) + (X% \ 8)
 OUT &H3CE, 5: OUT &H3CF, 2
 OUT &H3CE, 8: OUT &H3CF, BitMask%(X% AND 7)
 Byte% = PEEK(Offset&): POKE Offset&, Culler%
 END SUB

 SUB QPrint (X%, Y%, Text$, Culler%)
 'this routine uses the VGA hardware to print text
 '  in 16 color graphics modes
 'by Douglas H. Lusher, 06-08-1996
 SHARED BytesPerLine%

 ' 8 x 8 char box, CGA
 CharSegment% = &HFFA6: CharOffset% = &HE: CharHgt% = 8

 ' 8 x 16 char box, VGA
 'DIM Regs AS RegTypeX
 'Regs.AX = &H1130
 'Regs.BX = &H600
 'CALL InterruptX(&H10, Regs, Regs)
 'CharSegment% = Regs.ES: CharOffset% = Regs.BP: CharHgt% = 16

 REDIM BitPattern%(1 TO CharHgt%)
 Temp& = (CLNG(Y%) * BytesPerLine%) + (X% \ 8)
 VideoSegment% = &HA000 + (Temp& \ 16)
 VideoOffset% = (Temp& MOD 16) - 1
 OUT &H3CE, 5: OUT &H3CF, 2: OUT &H3CE, 8
 FOR Char% = 1 TO LEN(Text$)
   Ptr% = CharHgt% * ASC(MID$(Text$, Char%, 1)) + CharOffset% - 1
   DEF SEG = CharSegment%
   FOR Ln% = 1 TO CharHgt%
     BitPattern%(Ln%) = PEEK(Ptr% + Ln%)
   NEXT
   VideoPtr% = VideoOffset% + Char%
   DEF SEG = VideoSegment%
   FOR Ln% = 1 TO CharHgt%
     OUT &H3CF, BitPattern%(Ln%)
     Byte% = PEEK(VideoPtr%): POKE VideoPtr%, Culler%
     VideoPtr% = VideoPtr% + BytesPerLine%
   NEXT
 NEXT
 END SUB

