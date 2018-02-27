SHELL "dir nul > temp.txt"
OPEN "temp.txt" FOR INPUT AS #1
FOR t% = 1 TO 4: LINE INPUT #1, CurDir$: NEXT
CLOSE 1
KILL "temp.txt"
CurDir$ = MID$(CurDir$, 15)
PRINT "Your current working directory is ["; CurDir$; "]"

