'============================================================================
'     To: ALL                           Number: 49       Refer: 0     
'   From: DAVE MORROW               Conference: QUIK_BAS -Qui (298)
'   Date: 08-06-99 03:13              BBS Name: YE OLDE PAWN SHOPPE ]I[
'Subject: Colored Balls                 BBS ID: YOPSIII
'----------------------------------------------------------------------------

 'here are some colorful bouncy balls... :)
SCREEN 13
DEFINT A-Z
RANDOMIZE TIMER
 FOR PAL = 0 TO 63
  PALETTE PAL, PAL
 NEXT PAL
 RED = 0
 PAL = 64
 FOR BLUE = 0 TO 63
  B& = (65536 * BLUE) + (256 * GREEN) + RED
  PALETTE PAL, B&
  PAL = PAL + 1
 NEXT BLUE
 RED = 0: BLUE = 0
 PAL = 127
 FOR GREEN = 0 TO 63
  G& = (65536 * BLUE) + (256 * GREEN) + RED
  PALETTE PAL, G&
  PAL = PAL + 1
 NEXT GREEN
 RED = 0: BLUE = 0: GREEN = 0
 PAL = 191
 FOR BLACK = 0 TO 63
  RED = BLACK: GREEN = BLACK: BLUE = BLACK
  G& = (65536 * BLUE) + (256 * GREEN) + RED
  PALETTE PAL, G&
  PAL = PAL + 1
 NEXT BLACK
 BCLR = 64: GCLR = 127
 RX = RND * 319: RY = RND * 199: BX = RND * 319: BY = RND * 199
 GX = RND * 319: GY = RND * 199: IX = RND * 319: IY = RND * 199
 DO
  RXM = RND * 3: RYM = RND * 3
  BXM = RND * 3: BYM = RND * 3
  GXM = RND * 3: GYM = RND * 3
  IXM = RND * 3: IYM = RND * 3
  IF RXD = 0 THEN RX = RX + RXM
  IF RXD = 1 THEN RX = RX - RXM
  IF RYD = 0 THEN RY = RY + RYM
  IF RYD = 1 THEN RY = RY - RYM
  IF BXD = 0 THEN BX = BX + BXM
  IF BXD = 1 THEN BX = BX - BXM
  IF BYD = 0 THEN BY = BY + BYM
  IF BYD = 1 THEN BY = BY - BYM
  IF GXD = 0 THEN GX = GX + GXM
  IF GXD = 1 THEN GX = GX - GXM
  IF GYD = 0 THEN GY = GY + GYM
  IF GYD = 1 THEN GY = GY - GYM
  IF IXD = 0 THEN IX = IX + GXM
  IF IXD = 1 THEN IX = IX - GXM
  IF IYD = 0 THEN IY = IY + GYM
  IF IYD = 1 THEN IY = IY - GYM
  IF RX > 319 THEN RXD = 1
  IF RX < 0 THEN RXD = 0
  IF RY > 199 THEN RYD = 1
  IF RY < 0 THEN RYD = 0
  IF BX > 319 THEN BXD = 1
  IF BX < 0 THEN BXD = 0
  IF BY > 199 THEN BYD = 1
  IF BY < 0 THEN BYD = 0
  IF GX > 319 THEN GXD = 1
  IF GX < 0 THEN GXD = 0
  IF GY > 199 THEN GYD = 1
  IF GY < 0 THEN GYD = 0
  IF IX > 319 THEN IXD = 1
  IF IX < 0 THEN IXD = 0
  IF IY > 199 THEN IYD = 1
  IF IY < 0 THEN IYD = 0
  MAX.RADIUS = 15
  CIRCLE (RX, RY), MAX.RADIUS, RCLR
  PAINT (RX, RY), RCLR, RCLR
  CIRCLE (BX, BY), MAX.RADIUS, BCLR
  PAINT (BX, BY), BCLR, BCLR
  CIRCLE (GX, GY), MAX.RADIUS, GCLR
  PAINT (GX, GY), GCLR, GCLR
  CIRCLE (IX, IY), MAX.RADIUS, ICLR
  PAINT (IX, IY), ICLR, ICLR
  RCLR = RCLR + 1
  IF RCLR > 63 THEN RCLR = 0
  BCLR = BCLR + 1
  IF BCLR > 126 THEN BCLR = 64
  GCLR = GCLR + 1
  IF GCLR > 190 THEN GCLR = 130
  ICLR = ICLR + 1
  IF ICLR > 255 THEN ICLR = 191
 LOOP WHILE INKEY$ = ""
SYSTEM

'--- Maximus 3.01
' * Origin: BitByters BBS, Jim Brown (Sysop) Rockland, ON, Can. (1:163/215)    
 

