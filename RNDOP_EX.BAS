RANDOMIZE (TIMER)
DIM opponent$(2, 10)
opponent$(0, 0) = "Gak": opponent$(0, 1) = "Borg"
opponent$(0, 2) = "Ronk": opponent$(0, 3) = "Snorf"
opponent$(0, 4) = "B'Gak": opponent$(0, 5) = "M'Borg"
opponent$(0, 6) = "Klak": opponent$(0, 7) = "MacBorg"
opponent$(0, 8) = "G'Ronk": opponent$(0, 9) = "Fluff"
opponent$(1, 0) = "Dragon": opponent$(1, 1) = "Peterbilt"
opponent$(1, 2) = "Dark-Elf": opponent$(1, 3) = "Gelatin"
opponent$(1, 4) = "Vampire": opponent$(1, 5) = "Slimemold"
opponent$(1, 6) = "Fungus": opponent$(1, 7) = "Porridge"
opponent$(1, 8) = "Elvis-Impersonator": opponent$(1, 9) = "Dustball"
level% = 1
WHILE level% <> 0
   COLOR 10, 0
   INPUT "Enter Level [1 - 2 (0 = quit)] => ", level%
   IF level% < 3 AND level% >= 0 THEN
      opp% = (RND * 999) MOD 10
      IF level% > 0 THEN
         COLOR 15, 1
         PRINT " You face a "; opponent$(level% - 1, opp%);
         hits% = level% * 100 + opp% * 10 + ((RND * 999) MOD 10)
         PRINT " With"; hits%; "Hit Points. ";
         COLOR 10, 0
         PRINT
      END IF
   END IF
WEND

