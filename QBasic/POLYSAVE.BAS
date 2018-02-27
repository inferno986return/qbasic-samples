'_|_|_|   POLYSAVE.BAS   PUBLIC DOMAIN   Kurt Kuzba 5/18/1999
RANDOMIZE TIMER: COLOR 15, 1: SCREEN 0: WIDTH 80, 25
TYPE PolyVector
  x AS INTEGER: y AS INTEGER: xd AS INTEGER: yd AS INTEGER: END TYPE
TYPE PolyColors
  r AS INTEGER: g AS INTEGER: b AS INTEGER: rd AS INTEGER
  gd AS INTEGER: bd AS INTEGER: Pal AS INTEGER: END TYPE
DIM PolySides AS INTEGER, PolyPolys AS INTEGER, PolyTrail AS INTEGER
INPUT "Number of Trails ( 1-16 )=> ", PolyTrail
INPUT "Number of Polys  ( 1-15 )=> ", PolyPolys
INPUT "Number of Sides  ( 1-10 )=> ", PolySides
PolyTrail = PolyTrail + 2
IF PolySides < 3 THEN PolySides = 3
IF PolySides > 10 THEN PolySides = 10
IF PolyPolys < 1 THEN PolyPolys = 1
IF PolyPolys > 15 THEN PolyPolys = 15
IF PolyTrail < 2 THEN PolyTrail = 2
IF PolyTrail > 18 THEN PolyTrail = 18
SCREEN 12
DIM PolyPoints(PolySides, PolyPolys, PolyTrail) AS PolyVector
DIM PolyHue(1 TO PolyPolys) AS PolyColors, Change AS INTEGER
FOR s% = 1 TO PolySides
  FOR p% = 1 TO PolyPolys
    OUT &H3C7, p%: PolyHue(p%).r = INP(&H3C9)
    PolyHue(p%).g = INP(&H3C9): PolyHue(p%).b = INP(&H3C9)
    PolyHue(p%).rd = 1: PolyHue(p%).gd = 1: PolyHue(p%).bd = 1
    PolyPoints(s%, p%, PolyTrail).xd = RND * 3 + 2
    PolyPoints(s%, p%, PolyTrail).yd = RND * 3 + 2
    PolyPoints(s%, p%, PolyTrail).x = RND * 639
    PolyPoints(s%, p%, PolyTrail).y = RND * 479
  NEXT: NEXT
WHILE INKEY$ > "": WEND
DO
  FOR s% = 1 TO PolySides
    FOR p% = 1 TO PolyPolys
      FOR t% = 1 TO PolyTrail
        PolyPoints(s%, p%, t% - 1) = PolyPoints(s%, p%, t%): NEXT
      t% = PolyTrail: x% = PolyPoints(s%, p%, t%).x
      y% = PolyPoints(s%, p%, t%).y
      IF x% > 639 THEN PolyPoints(s%, p%, t%).xd = -(RND * 3 + 2)
      IF x% < 0 THEN PolyPoints(s%, p%, t%).xd = (RND * 3 + 2)
      IF y% > 479 THEN PolyPoints(s%, p%, t%).yd = -(RND * 3 + 2)
      IF y% < 0 THEN PolyPoints(s%, p%, t%).yd = (RND * 3 + 2)
      PolyPoints(s%, p%, t%).x = x% + PolyPoints(s%, p%, t%).xd
      PolyPoints(s%, p%, t%).y = y% + PolyPoints(s%, p%, t%).yd
    NEXT: NEXT
  FOR s% = 1 TO PolySides
    FOR p% = 1 TO PolyPolys
      PolyPoints(0, p%, 0) = PolyPoints(PolySides, p%, 0)
      x1% = PolyPoints(s%, p%, 0).x: y1% = PolyPoints(s%, p%, 0).y
      x2% = PolyPoints(s% - 1, p%, 0).x
      y2% = PolyPoints(s% - 1, p%, 0).y
      LINE (x1%, y1%)-(x2%, y2%), 0
      FOR t% = 1 TO PolyTrail - 1
        PolyPoints(0, p%, t%) = PolyPoints(PolySides, p%, t%)
        x1% = PolyPoints(s%, p%, t%).x
        y1% = PolyPoints(s%, p%, t%).y
        x2% = PolyPoints(s% - 1, p%, t%).x
        y2% = PolyPoints(s% - 1, p%, t%).y
        LINE (x1%, y1%)-(x2%, y2%), p%
      NEXT: NEXT: NEXT: WAIT &H3DA, 8: WAIT &H3DA, 8, 8
  Change = (Change + 1) AND 31
  IF Change = 0 THEN
    FOR p% = 1 TO PolyPolys
      r% = PolyHue(p%).r + PolyHue(p%).rd
      IF r% > 63 THEN PolyHue(p%).rd = -(RND * 3 + 1)
      IF r% < 0 THEN PolyHue(p%).rd = (RND * 3 + 1)
      g% = PolyHue(p%).g + PolyHue(p%).gd
      IF g% > 63 THEN PolyHue(p%).gd = -(RND * 3 + 1)
      IF g% < 0 THEN PolyHue(p%).gd = (RND * 3 + 1)
      b% = PolyHue(p%).b + PolyHue(p%).bd
      IF b% > 63 THEN PolyHue(p%).bd = -(RND * 3 + 1)
      IF b% < 0 THEN PolyHue(p%).bd = (RND * 3 + 1)
      PolyHue(p%).r = PolyHue(p%).r + PolyHue(p%).rd
      PolyHue(p%).g = PolyHue(p%).g + PolyHue(p%).gd
      PolyHue(p%).b = PolyHue(p%).b + PolyHue(p%).bd
      OUT &H3C8, p%: OUT &H3C9, PolyHue(p%).r
      OUT &H3C9, PolyHue(p%).g: OUT &H3C9, PolyHue(p%).b
    NEXT
  END IF
  IF INKEY$ > "" THEN EXIT DO
LOOP: SCREEN 0: WIDTH 80, 25: SYSTEM

