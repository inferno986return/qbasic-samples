'_|_|_|   STARRUSH.BAS
'_|_|_|   Yet another starfield simulation, with 20 levels of motion
'_|_|_|   and full 360 degree directional movement. 20-3000 stars.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (12/19/96)
TYPE stardata
   xypref AS STRING * 1
   xystep AS INTEGER
   xyprog AS INTEGER
   xylim AS INTEGER
   count AS INTEGER
   speed AS INTEGER
   xdir AS INTEGER
   ydir AS INTEGER
   xpos AS INTEGER
   ypos AS INTEGER
END TYPE
DECLARE SUB starinit (S AS stardata)
DECLARE SUB starmove (S AS stardata)
N% = 300  'N% = VAL(COMMAND$)
RANDOMIZE TIMER
IF N% < 20 THEN
   PRINT "USAGE: starrush N[number of stars 20-3000]": SYSTEM
END IF
IF N% > 3000 THEN N% = 3000
REDIM starfield(1 TO N%) AS stardata
FOR t% = 1 TO N%: starinit starfield(t%): NEXT
starcolour% = 0
SCREEN 12
DO
   IF INKEY$ <> "" THEN EXIT DO
   FOR S% = 1 TO N%
      C% = starfield(S%).count + 1
      IF C% >= starfield(S%).speed THEN
         PSET (starfield(S%).ypos, starfield(S%).xpos), 0
         starmove starfield(S%)
         PSET (starfield(S%).ypos, starfield(S%).xpos), starcolour% + 9
         starcolour% = (starcolour% + 1) MOD 7
      ELSE
         starfield(S%).count = C%
      END IF
   NEXT
LOOP
SCREEN 0
'_|_|_|   end   STARRUSH.BAS

SUB starinit (S AS stardata)
   S.count = 0: S.speed = RND * 19 + 1
   S.xpos = 239: S.ypos = 319: dest% = RND * 659
   IF dest% >= 200 THEN x% = 239: y% = RND * 319
   IF dest% <= 200 THEN y% = 319: x% = RND * 239
   S.xypref = "Y": S.xystep = x%: S.xyprog = 0: S.xylim = y%
   IF x% > y% THEN S.xypref = "X": SWAP S.xystep, S.xylim
   S.xdir = -1: IF (RND > .49) THEN S.xdir = 1
   S.ydir = -1: IF (RND > .49) THEN S.ydir = 1
   FOR t% = RND * 70 + 20 TO 0 STEP -1: starmove S: NEXT
END SUB

SUB starmove (S AS stardata)
   S.count = 0: x% = S.xpos: y% = S.ypos: ny% = y%: nx% = x%
   l% = S.xylim: i% = S.xystep: p% = S.xyprog + i%
   SELECT CASE S.xypref
      CASE "X": nx% = x% + S.xdir
         IF p% >= l% THEN ny% = y% + S.ydir: p% = p% - l%
      CASE "Y": ny% = y% + S.ydir
         IF p% >= l% THEN nx% = x% + S.xdir: p% = p% - l%
      CASE "": nx% = x% + S.xdir: ny% = y% + S.ydir
   END SELECT: S.xyprog = p%
   IF (ny% < 0) OR (ny% > 639) OR (nx% > 479) OR (nx% < 0) THEN
      starinit S: nx% = S.xpos: ny% = S.ypos
   ELSE
      S.xpos = nx%: S.ypos = ny%
   END IF
END SUB

