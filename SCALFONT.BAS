'_|_|_|   SCALFONT.BAS
'_|_|_|   An example of using scalable fonts in Quick Basic.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (11/25/96)
DECLARE SUB PutChar (ch%, c%, x%, y%)
DECLARE FUNCTION Characters$ (c%)
DIM CHigh AS INTEGER, CWide AS INTEGER
CHigh = 14: CWide = 8
SCREEN 12
PAINT (0, 0), 7
PutChar 65, 0, 15, 25
PutChar 66, 0, 24, 25
PutChar 67, 0, 32, 25
CHigh = 15: CWide = 20
PutChar 65, 1, 5, 43
PutChar 66, 1, 15, 43
PutChar 67, 1, 25, 43
CHigh = 30: CWide = 12
PutChar 65, 4, 10, 32
PutChar 66, 4, 22, 32
PutChar 67, 4, 34, 32
CHigh = 42: CWide = 24
PutChar 65, 5, 5, 36
PutChar 66, 5, 17, 36
PutChar 67, 5, 29, 36
SOUND 999, 1: WHILE INKEY$ = "": WEND
SCREEN 0: WIDTH 80, 25
'_|_|_|   end   SCALFONT.BAS
 

FUNCTION Characters$ (c%)
  SELECT CASE c%
     CASE 65
        Characters$ = "LAMCA LGMCA LBHFH"
     CASE 66
        c$ = "LAMAA LAACA LAMEM LAEEE CCCBD CCCBA CEIDD "
        Characters$ = c$ + "CEIDA"
     CASE 67
        Characters$ = "CCEEA CCEEB CCHEC CCHED LADAH"
  END SELECT
END FUNCTION

SUB PutChar (ch%, c%, x%, y%)
   SHARED CWide AS INTEGER, CHigh AS INTEGER
   CharDef$ = Characters$(ch%): LN% = ASC("L"): CR% = ASC("C")
   asp! = (4 * (CHigh / CWide)) / 3
   WHILE CharDef$ <> ""
      x1% = ((ASC(MID$(CharDef$, 2)) - 64 + x%) * CWide) \ 8
      y1% = ((ASC(MID$(CharDef$, 3)) - 64 + y%) * CHigh) \ 14
      SELECT CASE ASC(CharDef$)
         CASE CR%
            x2% = (ASC(MID$(CharDef$, 4)) - 64) * CHigh \ 14
            R! = (ASC(MID$(CharDef$, 5)) - 64) * 1.57
            CIRCLE (x1%, y1%), x2%, c%, (R! - 1.57), R!, asp!
         CASE LN%
            x2% = ((ASC(MID$(CharDef$, 4)) - 64 + x%) * CWide) \ 8
            y2% = ((ASC(MID$(CharDef$, 5)) - 64 + y%) * CHigh) \ 14
            LINE (x1%, y1%)-(x2%, y2%), c%
      END SELECT
      CharDef$ = MID$(CharDef$, 7)
   WEND
END SUB

