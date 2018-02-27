'_|_|_|   SHOWKEYS.BAS
'_|_|_|   Shows the CHR$() values for the keyboard.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba  (3/8/97)
ESC$ = CHR$(27)
PRINT "Press Any Key to test. Hit ESCAPE when done."
DO
   DO: K$ = INKEY$: LOOP WHILE K$ = ""
   PRINT "CHR$("; MID$(STR$(ASC(K$)), 2); ")";
   IF LEN(K$) > 1 THEN
      PRINT " + CHR$("; MID$(STR$(ASC(MID$(K$, 2))), 2); ") ";
   END IF
   PRINT "    "
LOOP WHILE K$ <> ESC$
'_|_|_|   end   SHOWKEYS.BAS

