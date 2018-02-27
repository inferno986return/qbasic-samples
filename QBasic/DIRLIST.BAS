'_|_|_|   DIRLIST.BAS   (part 1 of 2)
'_|_|_|   Produces a scrolling directory list. Requires QB.LIB
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (12/11/96)
TYPE DirectoryEntry
   reserved AS STRING * 21: attribute AS STRING * 1
   filetime AS INTEGER: filedate AS INTEGER
   filesize AS LONG: filename AS STRING * 13: END TYPE
TYPE DirectoryStrings
   filetype AS STRING * 4: filetime AS STRING * 9
   filedate AS STRING * 10: filesize AS STRING * 11
   filename AS STRING * 13: END TYPE
DECLARE SUB FindFirst (filespec$, D AS DirectoryStrings, mask%)
DECLARE SUB FindNext (D AS DirectoryStrings)
DECLARE SUB SetDTA ()
DECLARE SUB BuildEntry (D AS ANY)
' $INCLUDE: 'qb.bi'
'$DYNAMIC
CLEAR : DIM SBD AS INTEGER, VOL AS INTEGER, SYS AS INTEGER
DIM HID AS INTEGER, ARC AS INTEGER
SBD = 16: VOL = 8: SYS = 4: HID = 2: ARC = 0
DIM SHARED DTA AS DirectoryEntry, DIR(1000)  AS DirectoryStrings
DIM inregs AS RegTypeX, outregs AS RegTypeX, DE AS INTEGER
DO: COLOR 10, 0: CLS
   LOCATE 3, 8: INPUT "Enter Filespec => ", Fspec$
   IF Fspec$ = "" THEN COLOR 10, 0: CLS : END
   COLOR 3: LOCATE 4, 8: PRINT STRING$(63, 177): B$ = STRING$(2, 177)
   FOR T% = 5 TO 19
      LOCATE T%, 8: PRINT B$; : LOCATE , 69: PRINT B$; : NEXT
   LOCATE 20, 8: PRINT STRING$(63, 177): B$ = STRING$(2, 177)
   LOCATE 5, 10: COLOR 11: PRINT "Searching directory...  ";
   DE = 0: FindFirst Fspec$, DIR(DE), (SBD + SYS + HID + ARC)
   WHILE (DE < 999) AND (RTRIM$(DIR(DE).filename) <> "")
      DE = DE + 1: FindNext DIR(DE): WEND
   Top% = 0: Sel% = 0
   IF DE = 0 THEN
      PRINT "No Files Found.  Hit any key."
      WHILE INKEY$ <> "": WEND: WHILE INKEY$ = "": WEND
   ELSE
      DO: FOR T% = 0 TO 14
            LOCATE T% + 5, 10
            IF Top% + T% < DE THEN
               IF Top% + T% = Sel% THEN
                  COLOR 1, 7
                  PRINT "  "; DIR(T% + Top%).filename; "  ";
                  PRINT DIR(T% + Top%).filesize; "  ";
                  PRINT DIR(T% + Top%).filetime; "  ";
                  PRINT DIR(T% + Top%).filedate; "  ";
                  PRINT DIR(T% + Top%).filetype; "  ";
               ELSE
                  COLOR 14, 0: PRINT "  ";
                  PRINT DIR(T% + Top%).filename; "  ";
                  COLOR 11: PRINT DIR(T% + Top%).filesize; "  ";
                  COLOR 9: PRINT DIR(T% + Top%).filetime; "  ";
                  COLOR 10: PRINT DIR(T% + Top%).filedate; "  ";
                  COLOR 15: PRINT DIR(T% + Top%).filetype; "  ";
               END IF
            ELSE
               COLOR 10, 0: PRINT SPACE$(57);
            END IF
         NEXT: LOCATE 3, 8: COLOR 15, 0
         PRINT MID$(STR$(Sel% + 1) + "    ", 2, 5)
         DO: k$ = INKEY$: LOOP WHILE k$ = ""
         k% = ASC(k$): IF k% = 0 THEN k% = -ASC(MID$(k$, 2))
         SELECT CASE k%
            CASE -71: Top% = 0: Sel% = 0
            CASE -72
               IF Sel% = 0 THEN
                  SOUND 999, 1
               ELSE
                  Sel% = Sel% - 1: IF Sel% < Top% THEN Top% = Sel%
               END IF
            CASE -73
               IF Sel% = 0 THEN
                  SOUND 999, 1
               ELSE
                  IF Sel% < 16 THEN
                     Sel% = 0: Top% = 0
                  ELSE
                     Sel% = Sel% - 15: Top% = Top% - 15
                     IF Top% < 0 THEN Top% = 0
                  END IF
               END IF
            CASE -79
               IF Sel% = DE THEN
                  SOUND 999, 1
               ELSE
                  Sel% = DE - 1: Top% = DE - 15
                  IF Top% < 0 THEN Top% = 0
               END IF
'_|_|_|   end   DIRLIST.BAS   (part 1 of 2)
'_|_|_|   DIRLIST.BAS   (part 2 of 2)
'_|_|_|   Produces a scrolling directory list. Requires QB.LIB
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (12/11/96)
            CASE -80
               IF Sel% >= DE - 1 THEN
                  SOUND 999, 1
               ELSE
                  Sel% = Sel% + 1
                  IF Sel% > Top% + 14 THEN Top% = Top% + 1
               END IF
            CASE -81
               IF Sel% = DE THEN
                  SOUND 999, 1
               ELSE
                  IF Sel% + 16 > DE THEN
                     Sel% = DE - 1: Top% = DE - 15
                     IF Top% < 0 THEN Top% = 0
                  ELSE
                     Sel% = Sel% + 15: Top% = Top% + 15
                     IF Top% + 16 > DE THEN Top% = DE - 15
                  END IF
               END IF
         END SELECT
      LOOP WHILE (k% <> 13) AND (k% <> 27)
   END IF
LOOP
 
SUB BuildEntry (D AS DirectoryStrings)
   D.filetype = "ASHR": T% = ASC(DTA.attribute)
   IF (T% AND 1) = 0 THEN MID$(D.filetype, 4) = " "
   IF (T% AND 2) = 0 THEN MID$(D.filetype, 3) = " "
   IF (T% AND 4) = 0 THEN MID$(D.filetype, 2) = " "
   IF (T% AND 32) = 0 THEN MID$(D.filetype, 1) = " "
   IF (T% AND 16) = 16 THEN D.filetype = "<DIR"
   T& = DTA.filetime: IF T& < 0 THEN T& = 65536 + T&
   H& = T& \ 2048: M$ = "a": IF H& > 11 THEN M$ = "p"
   H& = H& MOD 12: IF H& = 0 THEN H& = 12
   F$ = RIGHT$(STR$(H&), 2) + ":": M& = (T& \ 32) AND 63
   F$ = F$ + RIGHT$("0" + LTRIM$(STR$(M&)), 2) + ":"
   S& = (T& AND 31) * 2
   D.filetime = F$ + RIGHT$("0" + LTRIM$(STR$(S&)), 2) + M$
   T& = DTA.filedate: IF T& < 0 THEN T& = 65536 + T&
   M& = (T& \ 32) AND 15: F$ = RIGHT$(STR$(M&), 2) + "-"
   DY& = T& AND 31
   F$ = F$ + RIGHT$("0" + LTRIM$(STR$(DY&)), 2) + "-"
   Y& = 1980 + (T& \ 512): D.filedate = F$ + RIGHT$(STR$(Y&), 4)
   F$ = LTRIM$(STR$(DTA.filesize)): comma% = LEN(F$): S$ = ""
   FOR T% = comma% TO 1 STEP -1: S$ = S$ + MID$(F$, T%, 1): NEXT
   comma% = 0: F$ = ""
   WHILE S$ <> "": F$ = LEFT$(S$, 1) + F$: S$ = MID$(S$, 2)
      comma% = (comma% + 1) MOD 3
      IF (comma% = 0) AND S$ <> "" THEN F$ = "," + F$
   WEND
   D.filesize = RIGHT$("           " + F$, 11)
   D.filename = DTA.filename
END SUB
 
SUB FindFirst (filespec$, D AS DirectoryStrings, mask%)
   SetDTA
   filespec$ = filespec$ + CHR$(0)
   DIM inregs AS RegTypeX, outregs AS RegTypeX
   inregs.ax = &H4E00: inregs.dx = SADD(filespec$)
   inregs.ds = VARSEG(filespec$): inregs.cx = mask%
   CALL INTERRUPTX(&H21, inregs, outregs)
   IF (outregs.flags AND 1) = 0 THEN
      BuildEntry D
   ELSE D.filename = ""
   END IF
END SUB
 
SUB FindNext (D AS DirectoryStrings)
   DIM inregs AS RegTypeX, outregs AS RegTypeX
   DTA.filename = "": inregs.ax = &H4F00
   CALL INTERRUPTX(&H21, inregs, outregs)
   IF (outregs.flags AND 1) = 0 THEN
      BuildEntry D
   ELSE D.filename = ""
   END IF
END SUB
 
SUB SetDTA
   DIM inregs AS RegTypeX, outregs AS RegTypeX
   inregs.dx = VARPTR(DTA): inregs.ds = VARSEG(DTA)
   inregs.ax = &H1A00: CALL INTERRUPTX(&H21, inregs, outregs)
END SUB
'_|_|_|   end   DIRLIST.BAS   (part 2 of 2)
 
