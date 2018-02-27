'_|_|_|   FILECOMP.BAS
'_|_|_|   A simple program to compare the content of 2 files.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (8/3/96)
DECLARE FUNCTION GetStr$ (Prompt$, L AS INTEGER)
F1$ = GetStr$(" First File =>", 32)
F2$ = GetStr$("Second File =>", 32)
IF (F1$ = "") OR (F2$ = "") THEN END
h1% = FREEFILE: OPEN F1$ FOR BINARY AS h1%
h2% = FREEFILE: OPEN F2$ FOR BINARY AS h2%
Identical% = (LOF(h1%) = LOF(h2%))
WHILE Identical% AND (NOT EOF(h1%)) AND (NOT EOF(h2%))
   Identical% = (INPUT$(512, h1%) = INPUT$(512, h2%))
WEND
CLOSE
IF Identical% THEN
   PRINT "These files are identical."
ELSE
   PRINT "These files are different."
END IF
END
FUNCTION GetStr$ (Prompt$, L%)
   R$ = "": PRINT Prompt$; " "; : LOCATE , , 1
   DO: DO: K$ = INKEY$: LOOP WHILE K$ = ""
      SELECT CASE K$
         CASE " " TO "z": RL% = LEN(R$)
            IF RL% = L% THEN
               MID$(R$, L%) = K$: PRINT CHR$(29); K$;
            ELSE R$ = R$ + K$: PRINT K$;
            END IF
         CASE CHR$(8): RL% = LEN(R$)
            IF RL% > 0 THEN
               IF RL% = 1 THEN R$ = "":  ELSE R$ = LEFT$(R$, RL% - 1)
               PRINT CHR$(29); " "; CHR$(29);
            END IF
         CASE CHR$(13): PRINT : GetStr$ = R$: EXIT FUNCTION
      END SELECT
   LOOP
END FUNCTION
'_|_|_|   end   FILECOMP.BAS
