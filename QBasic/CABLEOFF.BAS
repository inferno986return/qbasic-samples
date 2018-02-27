DECLARE FUNCTION Rand% (lo%, hi%)
RANDOMIZE TIMER: wide% = 32: high% = 20: Swide% = 320: Shigh% = 200
DIM block%((((wide% + 7) \ 8) * 4 * high% + 4) \ 2)
DIM colrz(3): colrz(0) = 0: colrz(1) = 7: colrz(2) = 8: colrz(3) = 15
SCREEN 7, , 0, 0: FOR x% = 0 TO Swide% - 1: FOR y% = 0 TO Shigh% - 1
    PSET (x%, y%), colrz((x% \ 8) AND 3): NEXT: NEXT: SCREEN , , 0, 1
DO: FOR xput% = 0 TO INT((Swide% - 1) \ wide%)
    FOR yput% = 0 TO INT((Shigh% - 1) \ high%)
      x% = Rand%(0, Swide% - wide%): y% = Rand%(0, Shigh% - high%)
      GET (x%, y%)-(x% + wide% - 1, y% + high% - 1), block%
      PUT (xput% * wide%, yput% * high%), block%, PSET
    NEXT: NEXT: WAIT &H3DA, 8: WAIT &H3DA, 8, 8: PCOPY 0, 1
LOOP WHILE INKEY$ = "": SCREEN 0: WIDTH 80, 25: SYSTEM

FUNCTION Rand% (lo%, hi%) : range% = hi% - lo% + 1
  Rand% = (RND * 32767) MOD range% + lo%: END FUNCTION

