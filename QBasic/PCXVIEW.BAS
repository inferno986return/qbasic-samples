'_|_|_|   PCXVIEW.BAS
'_|_|_|   An example of 320x200x256 .PCX display in QBasic.
'_|_|_|   No warrantees or guarantees are given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (7/20/96)
ON ERROR GOTO OOps: '$DYNAMIC
DIM buf(32002) AS INTEGER: buf(0) = 2560: buf(1) = 200
'_|_|_|  init buffer for 2560 bits X 200 lines
BSEG& = VARSEG(buf(2)): BOFS& = VARPTR(buf(2))
PCX$ = COMMAND$
IF PCX$ = "" THEN INPUT "Name of PCX file to view => ", PCX$
IF PCX$ = "" THEN END
bt1! = TIMER: PRINT "Loading file"
OPEN PCX$ FOR INPUT AS #1: CLOSE 1: OPEN PCX$ FOR BINARY AS #1
fin& = LOF(1) - 767: SEEK #1, fin&: pal$ = INPUT$(768, 1)
p% = 1: fin& = fin& - 1: SCREEN 13
FOR T& = 0 TO 255
   OUT &H3C8, T&
   FOR hue% = 1 TO 3
      OUT &H3C9, ASC(MID$(pal$, p%)) \ 4: p% = p% + 1
   NEXT
NEXT
SEEK #1, 129: T& = BOFS&: DEF SEG = BSEG&: CLS : spin% = 1
PRINT "Loading PCX  "; : spinner$ = "//--\\||": rle% = 0
DO
   PRINT CHR$(29); MID$(spinner$, spin%, 1);
   spin% = (spin% AND 7) + 1
   p$ = INPUT$(256, 1): fpos& = SEEK(1): l% = LEN(p$)
   IF fpos& > fin& THEN
      l% = l% - (fpos& - fin&): p$ = LEFT$(p$, l%): view$ = "done"
   END IF
   FOR p% = 1 TO l%
      dat% = ASC(MID$(p$, p%))
      IF rle% = 0 THEN
         IF (dat% AND 192) = 192 THEN
            rle% = dat% AND 63
         ELSE
            POKE T&, dat%: T& = T& + 1
         END IF
      ELSE
         FOR rle% = rle% TO 1 STEP -1
            POKE T&, dat%: T& = T& + 1
         NEXT
      END IF
   NEXT
LOOP UNTIL view$ = "done"
bt2! = TIMER: CLOSE 1: PUT (0, 0), buf, PSET
WHILE INKEY$ <> "": WEND: WHILE INKEY$ = "": WEND
SCREEN 12: WIDTH 80, 25: PRINT bt2! - bt1!
OOps:
   CLOSE 1: PRINT "error"; ERR: END
'_|_|_|   end   PCXVIEW.BAS

