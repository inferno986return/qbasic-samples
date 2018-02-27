'_|_|_|   FILEBAG.BAS
'_|_|_|   Simple file bundler routines for QB and QBasic.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   PUBLIC DOMAIN   by Kurt Kuzba.  (5/28/1997)
DECLARE SUB BAGit ()
DECLARE SUB UnBAGit ()
ON ERROR GOTO BooBoo
DO
   CLS : COLOR 15, 1
   LOCATE 5, 29: PRINT "  __----~~~~~~~~~~~~~~----__  "
   LOCATE 6, 29: PRINT " |  [ I ]=> Include files.  | "
   LOCATE 7, 29: PRINT " |  [ E ]=> Extract files.  | "
   LOCATE 8, 29: PRINT " |  [ Q ]=> Quit Program.   | "
   LOCATE 9, 29: PRINT "  ~~----______________----~~  "
   DO: k% = INSTR(" IEQ" + CHR$(27), UCASE$(INKEY$)): LOOP WHILE k% < 2
   SELECT CASE k%
      CASE 4, 5: EXIT DO
      CASE 2: BAGit
      CASE 3: UnBAGit
   END SELECT
LOOP
SYSTEM
BooBoo: Ferror$ = STR$(ERR): RESUME NEXT
'_|_|_|   end   FILEBAG.BAS

SUB BAGit
   SHARED Ferror$
   COLOR 10, 0: CLS
   INPUT "File to include => ", BagItem$
   INPUT ".BAG to hold it => ", Bagfile$
   Bag% = FREEFILE: Ferror$ = "ok": OPEN Bagfile$ FOR APPEND AS #Bag%
   IF Ferror$ <> "ok" THEN
      PRINT "File error. Unable to open "; Bagfile$
      WHILE INKEY$ <> "": WEND
      WHILE INKEY$ = "": WEND
      EXIT SUB
   END IF
   Itm% = FREEFILE: OPEN BagItem$ FOR INPUT AS #Itm%
   IF Ferror$ <> "ok" THEN
      PRINT "File error. Unable to open "; BagItem$
      WHILE INKEY$ <> "": WEND
      WHILE INKEY$ = "": WEND
      CLOSE Bag%: EXIT SUB
   END IF
   CLOSE Itm%: OPEN BagItem$ FOR BINARY AS #Itm%
   DO: slash% = INSTR(BagItem$, "\")
      IF slash% > 0 THEN BagItem$ = MID$(BagItem$, slash% + 1)
   LOOP WHILE slash% > 0
   PRINT #Bag%, LEFT$(BagItem$ + "            ", 12);
   PRINT #Bag%, LEFT$(STR$(LOF(Itm%)) + "          ", 12);
   WHILE NOT EOF(Itm%)
      it$ = INPUT$(512, Itm%): PRINT #Bag%, it$;
   WEND
   CLOSE Bag%: CLOSE Itm%
END SUB

SUB UnBAGit
   SHARED Ferror$
   COLOR 10, 0: CLS
   INPUT ".BAG to unpack => ", Bagfile$
   Bag% = FREEFILE: Ferror$ = "ok": OPEN Bagfile$ FOR INPUT AS #Bag%
   IF Ferror$ <> "ok" THEN
      PRINT "File error. Unable to open .BAG "; Bagfile$
      WHILE INKEY$ <> "": WEND
      WHILE INKEY$ = "": WEND
      EXIT SUB
   END IF
   CLOSE Bag%: OPEN Bagfile$ FOR BINARY AS #Bag%
   IF Ferror$ <> "ok" THEN
      PRINT "File error. Unable to open .BAG "; Bagfile$
      WHILE INKEY$ <> "": WEND
      WHILE INKEY$ = "": WEND
      EXIT SUB
   END IF
   PRINT "Unpacking "; Bagfile$
   PRINT "=============================="
   PRINT "File name      length     ===="
   DO
      BagItem$ = INPUT$(12, Bag%)
      IL& = VAL(INPUT$(12, Bag%))
      IF LOC(Bag%) >= LOF(Bag%) THEN EXIT DO
      PRINT BagItem$, IL&
      Itm% = FREEFILE: OPEN BagItem$ FOR OUTPUT AS #Itm%
      IF Ferror$ <> "ok" THEN
         PRINT "File error. Unable to open "; BagItem$
         WHILE INKEY$ <> "": WEND
         WHILE INKEY$ = "": WEND
         CLOSE Bag%: EXIT SUB
      END IF
      FOR t& = 1 TO IL& \ 512
         it$ = INPUT$(512, Bag%): PRINT #Itm%, it$;
      NEXT
      it$ = INPUT$(IL& MOD 512, Bag%): PRINT #Itm%, it$;
      CLOSE Itm%
   LOOP WHILE NOT EOF(Bag%)
   CLOSE Bag%
   PRINT "==== done ===================="
   WHILE INKEY$ <> "": WEND
   WHILE INKEY$ = "": WEND
END SUB

