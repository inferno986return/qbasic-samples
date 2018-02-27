'_|_|_|   KEYDEFS.BAS
'_|_|_|   A simple program to demonstrate using extended keys.
'_|_|_|   No warrantee or guarantee given or implied.
'_|_|_|   PUBLIC DOMAIN   by Kurt Kuzba.  (7/4/1997)
DIM ESC AS STRING * 1, UP AS STRING * 2, DOWN AS STRING * 2
DIM LEFT AS STRING * 2, RIGHT AS STRING * 2, PGUP AS STRING * 2
DIM PGDN AS STRING * 2, HOME AS STRING * 2, ENND AS STRING * 2
DIM INS AS STRING * 2, DEL AS STRING * 2, BS AS STRING * 1
ESC = CHR$(27): BS = CHR$(8)
INS = CHR$(0) + CHR$(82): DEL = CHR$(0) + CHR$(83)
PGUP = CHR$(0) + CHR$(73): PGDN = CHR$(0) + CHR$(81)
HOME = CHR$(0) + CHR$(71): ENND = CHR$(0) + CHR$(79)
UP = CHR$(0) + CHR$(72): DOWN = CHR$(0) + CHR$(80)
LEFT = CHR$(0) + CHR$(75): RIGHT = CHR$(0) + CHR$(77)
insert% = 1: ch$ = CHR$(219) + "_"
CLS
x% = 12: y% = 40
COLOR 14, 0
DO
   LOCATE x%, y%, 0
   PRINT MID$(ch$, insert% + 1, 1);
   LOCATE x%, y%, 0
   k$ = INKEY$
   SELECT CASE k$
      CASE INS
         insert% = (insert% + 1) AND 1: PRINT " ";
      CASE DEL
         SOUND 999, 1
      CASE HOME
         PRINT " "; : y% = 1
      CASE ENND
         PRINT " "; : y% = 80
      CASE PGUP
         PRINT " "; : x% = 1
      CASE PGDN
         PRINT " "; : x% = 25
      CASE UP
         PRINT " "; : x% = x% + (x% > 1)
      CASE DOWN
         PRINT " "; : x% = x% - (x% < 25)
      CASE LEFT
         PRINT " "; : y% = y% + (y% > 1)
      CASE RIGHT
         PRINT " "; : y% = y% - (y% < 80)
   END SELECT
LOOP WHILE k$ <> ESC
SYSTEM
'_|_|_|   end   KEYDEFS.BAS
 
