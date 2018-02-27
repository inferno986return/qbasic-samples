SCREEN 13: CLS
RANDOMIZE TIMER
WINDOW (0, 0)-(1024, 800)
DO
  blue% = INT(RND * 22)
  green% = INT(RND * 22)
  red% = INT(RND * 22)
  SELECT CASE (RND * 32767) MOD 3 + 1
    CASE 1  'mostly blue
      FOR n% = 1 TO 122
        shade% = shade% + 1
        IF shade% >= 63 THEN shade% = 0
        nGreen% = green% - shade%
        IF nGreen% <= 0 THEN nGreen% = green%
        nRed% = red% - shade%
        IF nRed% <= 0 THEN nRed% = red%
        colr& = 65536 * shade% + 256 * nGreen% + nRed%
        PALETTE n%, colr&
      NEXT n%
    CASE 2   'mostly green
      FOR n% = 1 TO 122
        shade% = shade% + 1
        IF shade% >= 63 THEN shade% = 0
        nBlue% = blue% - shade%
        IF nBlue% <= 0 THEN nBlue% = blue%
        nRed% = red% - shade%
        IF nRed% <= 0 THEN nRed% = red%
        colr& = 65536 * nBlue% + 256 * shade% + nRed%
        PALETTE n%, colr&
      NEXT n%
    CASE 3   'mostly red
      FOR n% = 1 TO 122
        shade% = shade% + 1
        IF shade% >= 63 THEN shade% = 0
        nGreen% = green% - shade%
        IF nGreen% <= 0 THEN nGreen% = green%
        nBlue% = blue% - shade%
        IF nBlue% <= 0 THEN nBlue% = blue%
        colr& = 65536 * nBlue% + 256 * nGreen% + shade%
        PALETTE n%, colr&
      NEXT n%
    CASE ELSE
  END SELECT
  FOR t% = 1 TO 5
    x1% = INT(RND * 1024)
    y1% = INT(RND * 780)
    x2% = INT(RND * 1024)
    y2% = INT(RND * 780)
    FOR j% = 0 TO 122
      WAIT &H3DA, 8, 8
      WAIT &H3DA, 8, 0
      LINE (x1% + j%, y1% - j%)-(x2% - j%, y2% + j%), j%, B
      IF INKEY$ > "" THEN EXIT DO
    NEXT j%
  NEXT t%
LOOP
SCREEN 0, 0, 0, 0: WIDTH 80, 25: CLS : END

