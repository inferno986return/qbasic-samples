'_|_|_|   LONGFN.BAS   PUBLIC DOMAIN   Kurt Kuzba   (9/9/1998)
'_|_|_|   Use the DOS SHELL to read long file names in QBasic.
INPUT "Directory to read => ", d$
cmd$ = "dir /b " + d$ + "\*.* > dir.txt"
IF d$ = "" THEN cmd$ = "dir /b *.* > dir.txt"
SHELL cmd$
OPEN "dir.txt" FOR INPUT AS #1
DIM Directory(1000) AS STRING, Entries AS INTEGER
DIM Top AS INTEGER, display AS INTEGER, Entry AS INTEGER
DIM qbUP AS STRING * 2, qbDN AS STRING * 2
DIM qbPU AS STRING * 2, qbPD AS STRING * 2
DIM qbHM AS STRING * 2, qbND AS STRING * 2
DIM qbCR AS STRING * 1, qbESC AS STRING * 1, Allowed AS STRING
qbHM = CHR$(0) + CHR$(71): qbND = CHR$(0) + CHR$(79)
qbUP = CHR$(0) + CHR$(72): qbDN = CHR$(0) + CHR$(80)
qbPU = CHR$(0) + CHR$(73): qbPD = CHR$(0) + CHR$(81)
qbCR = CHR$(13): qbESC = CHR$(27)
Allowed = qbUP + qbDN + qbPU + qbPD + qbHM + qbND + qbCR + qbESC
Entries = 0: Entry = 0
WHILE (NOT EOF(1)) AND (Entries < 1000)
   LINE INPUT #1, DirEntry$
   Directory(Entries) = DirEntry$
   Entries = Entries + 1
WEND
CLOSE 1
CLS
Top% = 0
COLOR 0, 3
DO
   FOR display = 0 TO 23
      LOCATE display + 1, 1
      COLOR 0, 3: IF Entry = display + Top THEN COLOR 1, 7
      IF (display + Top) < Entries THEN
         PRINT LEFT$(Directory(display + Top) + SPACE$(80), 80);
      ELSE
         PRINT SPACE$(80);
      END IF
   NEXT
   COLOR 15, 3: LOCATE 25, 1
   dat$ = "File" + STR$(Entry + 1) + " of" + STR$(Entries)
   PRINT LEFT$(dat$ + SPACE$(80), 80);
   DO: DO: k$ = INKEY$: LOOP WHILE k$ = ""
   LOOP WHILE INSTR(Allowed, k$) < 1
   SELECT CASE k$
      CASE qbUP: Entry = Entry + (Entry > 0)
         IF Entry < Top THEN Top = Top - 1
      CASE qbDN: Entry = Entry - (Entry < (Entries - 1))
         IF Entry > (Top + 23) THEN Top = Top + 1
      CASE qbCR, qbESC: EXIT DO
      CASE qbHM: Entry = 0: Top = 0
      CASE qbND: Entry = Entries - 1
         Top = Entries - 24: IF Top < 0 THEN Top = 0
      CASE qbPU
         Entry = Entry - 23: IF Entry < 0 THEN Entry = 0
         Top = Top - 23: IF Top < 0 THEN Top = 0
      CASE qbPD
         Entry = Entry + 23
         IF Entry >= Entries THEN Entry = Entries - 1
         Top = Top + 23
         IF Top > (Entries - 24) THEN Top = Entries - 24
   END SELECT
LOOP
COLOR 10, 0: CLS : SYSTEM
'_|_|_|   END   LONGFN.BAS
