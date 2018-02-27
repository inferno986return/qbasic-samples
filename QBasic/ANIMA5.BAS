DECLARE SUB Attrib (Attribute%, r%, g%, b%)
DECLARE SUB InitPalette ()
DECLARE SUB DrawBackdrop ()
DECLARE SUB DrawSprite ()
DECLARE SUB PageCopyBox (x AS INTEGER, y AS INTEGER, PageNumber AS INTEGER)
DECLARE SUB Stamp2 (x AS INTEGER, y AS INTEGER, s AS INTEGER)
DECLARE SUB PagePSet3 (x AS INTEGER, y AS INTEGER, Page AS INTEGER, C AS INTEGER)
DECLARE SUB Snapshot (x AS INTEGER, y AS INTEGER, s AS INTEGER)
'From: TONY CAVE                 Conference: QUIK_BAS -Qui (298)
'Date: 07-02-97 23:51              BBS Name: YE OLDE PAWN SHOPPE ]I[
'Subject: anima5.bas (1/3)              BBS ID: YOPSIII
'The following is by no means a finished product.  I just thought
'you might want to see the no-flicker animation and page swapping
'on screen 13.  The program uses PUT to copy the page to the screen.
'Copying a whole page, though, slows down the animation considerably.
'Any suggestions would be appreciated.  Just bear in mind that the
'program is not finished, and some of the subs with "Page" in the name
'only work with page one.  The animaiton is done by:
'1 copy screen to memory pages, page1 and page2
'2 do
'3  place a sprite on page1 (color 0 is transparent).
'4  copy page1 to the screen
'5  erase the sprite from page1 by copying part of page2 (PageCopyBox)
'6 loop
'''Begin ANIMA5.BAS
COMMON SHARED snap() AS INTEGER, leaf() AS INTEGER, swp() AS INTEGER
COMMON SHARED OldX AS INTEGER, OldY AS INTEGER
COMMON SHARED Page1() AS INTEGER, Page2() AS INTEGER
COMMON SHARED CurrentSegment AS LONG
DIM Page1(32001) AS INTEGER, Page2(32001) AS INTEGER
DIM snap(15, 15, 20) AS INTEGER, x AS INTEGER, y AS INTEGER
DIM leaf(10, 10, 20) AS INTEGER, swp(10, 10, 20) AS INTEGER
DIM t AS INTEGER, T2 AS INTEGER, T3  AS INTEGER: SCREEN 13
CurrentSegment = &HA000: DrawSprite: InitPalette: DrawBackdrop
GET (0, 0)-(319, 199), Page1(0): GET (0, 0)-(319, 199), Page2(0)
step1 = 3
DO: FOR t = 0 TO 628 STEP step1
      y = COS(t / 100) * 320 / step1 + 100
      x = SIN(t / 100) * 240 / step1 + 160
      Stamp2 x, y, t / step1 MOD 5
      PUT (0, 0), Page1(0), PSET: PageCopyBox x, y, 1
      IF INKEY$ > "" THEN DEF SEG : EXIT DO
   NEXT: LOOP: SCREEN 0: WIDTH 80, 25: SYSTEM

SUB Attrib (Attribute%, r%, g%, b%)
   IF -1 < Attribute% AND Attribute% < 256 THEN
      OUT &H3C8, Attribute%: OUT &H3C9, r%
      OUT &H3C9, g%: OUT &H3C9, b%
   END IF: END SUB

SUB DrawBackdrop
   'Draw Sky
   FOR y = 0 TO 199: LINE (0, y)-(319, y), y + 56: NEXT
   'Draw Planet
   CIRCLE (160, 800), 770, 2: PAINT (160, 190), 2, 2
   FOR y = 0 TO 5: CIRCLE (160, 800), 770 + y, 50 + y: NEXT
END SUB

SUB DrawSprite : DIM t AS INTEGER
   FOR t = 0 TO 2: CIRCLE (6, 5), t, 21 - t * 3 + 16
      CIRCLE (5, 5), t, 21 - t * 3 + 16
      CIRCLE (6, 4), t, 18 - t * 3 + 16
      CIRCLE (5, 4), t, 18 - t * 3 + 16
      IF t > 4 THEN Snapshot 0, 0, t    '  X, Y, snapshot#
   NEXT
   FOR t = 3 TO 4
      LINE (-t + 6, 2 + t)-(t + 6, 2 + t), 21 - t * 3 + 16: NEXT
   FOR t = 0 TO 4
      LINE (-4 + 6, 7)-(3 + 6, 2 + 4), 21 - 4 * 3 + 16, BF
      LINE (t * 2 + 2, 6)-(t * 2 + 3, 7), 14, BF
      Snapshot 0, 0, t                  '  X, Y, snapshot#
   NEXT: END SUB

SUB InitPalette : DIM pu AS INTEGER
   FOR y = 0 TO 199
      Attrib y + 56, 0, 0, INT(y * 63 / 199): NEXT
   FOR y = 0 TO 5
      Attrib 50 + y, 0, (5 - y) / 5 * 20 + 20, y / 5 * 40 + 15
   NEXT: END SUB

SUB PageCopy (PageNumber AS INTEGER)
   IF PageNumber = 1 THEN
      FOR count% = 0 TO 32001: Page1(count%) = Page2(count%): NEXT
   ELSE
      FOR count% = 0 TO 32001: Page2(count%) = Page1(count%): NEXT
   END IF: END SUB

SUB PageCopyBox (x AS INTEGER, y AS INTEGER, PageNumber AS INTEGER)
   DIM xt AS INTEGER, yt AS INTEGER, P AS LONG, c1 AS LONG
   PCBSeg& = CurrentSegment
   FOR xt = 0 TO 10: FOR yt = 0 TO 10
      P = 320& * (y + yt) + x + xt
      IF P < 32000 THEN
         CurrentSegment = VARSEG(Page2(0)): DEF SEG = CurrentSegment
         PagePSet3 x + xt, y + yt, 1, PEEK(P + 4)
      ELSE
         CurrentSegment = VARSEG(Page2(0))
         CurrentSegment = CurrentSegment + 2000
         DEF SEG = CurrentSegment
         PagePSet3 x + xt, y + yt, 1, PEEK(P - 31996)
      END IF
   NEXT: NEXT
   CurrentSegment = PCBSeg&: DEF SEG = CurrentSegment: END SUB

SUB PagePSet3 (x AS INTEGER, y AS INTEGER, Page AS INTEGER, C AS INTEGER)
   IF (x < 0) OR (y < 0) OR (x > 319) OR (y > 199) THEN EXIT SUB
   DIM P AS LONG, c1 AS LONG: c1 = VARSEG(Page1(0)): P = 320& * y + x
   IF P < 32000 THEN
      DEF SEG = VARSEG(Page1(0)): POKE P + 4, C
   ELSE
      DEF SEG = c1 + 2000: POKE P - 31996, C
   END IF: DEF SEG = CurrentSegment: END SUB

SUB Snapshot (x AS INTEGER, y AS INTEGER, s AS INTEGER)
   DIM xt AS INTEGER, yt AS INTEGER
   FOR xt = 0 TO 10: FOR yt = 0 TO 10
      snap(xt, yt, s) = POINT(xt + x, yt + y): NEXT: NEXT: END SUB

SUB Stamp2 (x AS INTEGER, y AS INTEGER, s AS INTEGER)
   DIM xt AS INTEGER, yt AS INTEGER
   FOR xt = 0 TO 10: FOR yt = 0 TO 10
      IF snap(xt, yt, s) > 0 THEN
         PagePSet3 x + xt, y + yt, 1, snap(xt, yt, s)
      END IF
   NEXT: NEXT: END SUB

