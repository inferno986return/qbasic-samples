'   From: Alex Wellerstein        Conference: FidoQBasic   (139)
'   Date: 05-21-96 23:12             Subject: Pretty subliminal...
'   this is peerrrdy!
'   5BOUNCE.BAS - Five Leaf Psychedelic Bounce
'   by General Electrik
DEFINT A-Z
SCREEN 13: CLS
pause% = 2    'speed, lower = faster, higher = slower
changes% = -1   'size changing, -1 = on, 0 = off
s! = 1          'default size
x = 0: y = 0: c = 31: xx% = 1: yy% = 1
DO
   c = c + 1: IF x < 1 THEN xx% = 1
   IF x > 320 THEN xx% = -1
   IF y < 1 THEN yy% = 1
   IF y > 200 THEN yy% = -1
   IF changes% THEN
      IF s! <= 1.1 THEN ss% = 0
      IF s! >= 16 THEN ss% = -1
      IF ss% THEN s! = s! - .1 ELSE s! = s! + .1
   END IF: x = x + xx%: y = y + yy%
   IF c = 104 THEN c = 32
   FOR i = 1 TO pause%: WAIT &H3DA, 8: WAIT &H3DA, 8, 8: NEXT
   DRAW "BM" + STR$(x) + "," + STR$(y): DRAW "C2"
   DRAW "S" + STR$(INT(s!))
   DRAW "nu50ne30nh30nr40nl40ng10nf10nd20"
   DRAW "C" + STR$(c): DRAW "S" + STR$(INT(s!))
   DRAW "nu50ne30nh30nr40nl40ng10nf10nd20"
LOOP WHILE INKEY$ = ""
END
'   dig that people! wow!
