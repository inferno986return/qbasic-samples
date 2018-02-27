CLS
COLOR 10, 0: PRINT "Test keys now. Hit ESC to exit.": COLOR 9
DO
   K$ = INKEY$
   IF K$ <> "" THEN
      PRINT "CHR$("; MID$(STR$(ASC(K$)), 2); ")";
      IF LEN(K$) = 2 THEN
         PRINT " + CHR$("; MID$(STR$(ASC(MID$(K$, 2))), 2); ")";
      END IF
      PRINT
   END IF
LOOP WHILE K$ <> CHR$(27)
 
 

