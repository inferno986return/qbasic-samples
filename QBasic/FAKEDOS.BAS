'_|_|_|   FAKEDOS.BAS
'_|_|_|   A silly little security program for DOS users.
'_|_|_|   If ANSI.SYS is not installed, then the changing of
'_|_|_|   colors using output to CONS: is not necessary
'_|_|_|   No guarantee or warrantee given or implied.
'_|_|_|   PUBLIC DOMAIN   by Kurt Kuzba   (10/26/1997)
DECLARE FUNCTION GetStr$ ()
OPEN "CONS:" FOR OUTPUT AS #1
PRINT #1, CHR$(27) + "[0;37m";
CLOSE 1
COLOR 7, 0
DO
   Password$ = GetStr$
   IF Password$ = "quit" THEN SYSTEM   'change this as you like
   OkCmd% = 0
   Password$ = LCASE$(Password$)
   OkCmd% = OkCmd% OR INSTR(LEFT$(Password$, 3), "dir")
   OkCmd% = OkCmd% OR INSTR(LEFT$(Password$, 2), "cd")
   OkCmd% = OkCmd% OR INSTR(LEFT$(Password$, 2), ":")
   IF OkCmd% = 0 THEN
      PRINT "bad command or file name"
   ELSE
      SHELL Password$
   END IF
LOOP
'_|_|_|   end   FAKEDOS.BAS

FUNCTION GetStr$
   MyStr$ = "": PRINT CHR$(13); ">";
   LOCATE CSRLIN, POS(0), 1
   DO
      DO: k$ = INKEY$: LOOP WHILE k$ = ""
      k$ = k$
      SELECT CASE k$
         CASE CHR$(13): GetStr$ = MyStr$: PRINT CHR$(13); : EXIT DO
         CASE CHR$(8)
            IF LEN(MyStr$) > 0 THEN
               IF LEN(MyStr$) = 1 THEN
                  MyStr$ = ""
               ELSE
                  MyStr$ = LEFT$(MyStr$, LEN(MyStr$) - 1)
               END IF
               PRINT CHR$(29); " "; CHR$(29);
            END IF
         CASE CHR$(1) TO CHR$(26): k$ = "^" + CHR$(64 + ASC(k$))
            MyStr$ = MyStr$ + k$: PRINT k$;
         CASE CHR$(32) TO CHR$(127):
            PRINT k$; : MyStr$ = MyStr$ + k$
      END SELECT
   LOOP
END FUNCTION

