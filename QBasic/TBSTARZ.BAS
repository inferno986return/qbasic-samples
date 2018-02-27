'   From: Tyler Barnes              Conference: FidoQBasic   (139)
'   Date: 02-22-96 20:58              BBS Name: Exec-PC
' Ok, here's a star field I wrote (The kind that has layered stars
' moving sideways) and it has background music. I can't say it
' couldn'y be programmed better, but it should give you an idea on
' how to do background music. By the way, when you run it, it'll ask
' for a bunch of settings. I suggest 75 star, 3 layers, layer speeds
' at 1, 1.5, and 2, Music on. You may have to adjust these settings
' according to taste, and the speed of your computer. Oh, also, make
' all the stars move left. It looks stupid if they move in different
' directions.
'By D. Tyler Barnes '95
'Tested on a 486dx2 80mhz
ON PLAY(1) GOSUB MuchMusic
CLS
INPUT "Number of stars:", StarNum%
INPUT "Number of layers:", LNum%
DIM StarX(StarNum%), StarY(StarNum%), Layer(LNum%)
FOR L = 1 TO LNum%
PRINT "Layer"; L; " speed:";
INPUT Layer(L)
PRINT "Layer"; L; " direction [L/R]";
DO: K$ = UCASE$(INKEY$): LOOP UNTIL K$ = "L" OR K$ = "R"
IF K$ = "L" THEN Layer(L) = -Layer(L)
PRINT
NEXT L
PLAY ON
PRINT "Music? [Y/N]";
WHILE A$ <> "Y" AND A$ <> "N": A$ = UCASE$(INKEY$): WEND
IF A$ = "Y" THEN MBuff$ = "T150L2O3CGP16L16FEDL2>C<GP16L16FEDL2>C<GP16L16FEFL2D"
PLAY "MB X" + VARPTR$(MBuff$)
SCREEN 13
FOR I% = 1 TO StarNum%
StarX(I%) = (RND * 319) + 1
StarY(I%) = (RND * 199) + 1
NEXT I%
DO
CL = 1
FOR I% = 1 TO StarNum%
PSET (StarX(I%), StarY(I%)), 0
IF I% > (StarNum% / LNum%) * CL THEN CL = CL + 1
StarX(I%) = StarX(I%) + Layer(CL)
IF StarX(I%) < 1 THEN StarX(I%) = 319: StarY(I%) = RND * 199
IF StarX(I%) > 319 THEN StarX(I%) = 1: StarY(I%) = RND * 199
C% = (CL * 4) + 19
PSET (StarX(I%), StarY(I%)), C%
NEXT I%
LOOP UNTIL LEN(INKEY$)
SCREEN 0
WIDTH 80, 25
END
MuchMusic:
Count% = Count% + 1
SELECT CASE Count%
CASE 0: MBuff$ = "T150L2O3CGP16L16FEDL2>C<GP16L16FEDL2>C<GP16L16FEFL2D"
CASE 1: MBuff$ = "P16L16<GGGL2>CGP32L16FEDL2>C<GP16L16FEDL2>C<GP16L16A+"
CASE 2: MBuff$ = "AA+L1GL2G.L8<G.L16GL4A.L8A>FEDCL16CDEDP16L8"
CASE 3: MBuff$ = "<AL4BL8G.L16G"
CASE 4: MBuff$ = "L4A.L8A>FEDCGP8L4D.P8L8<G.L16GL4A.L8A>FEDCL16"
CASE 5: MBuff$ = "CDEDP16L8<A"
CASE 6: MBuff$ = "L4BP16L8>G.L16GL8>C.L16<A+L8G+.L16GL8F.L16D+L8D.L16CL1G"
CASE 7: MBuff$ = "L2G.P16L16GGGL8>CP8L16<CCCL2C.": Count% = -1
END SELECT
PLAY "MB X" + VARPTR$(MBuff$)
RETURN
'--- Maximus/2 3.01
' * Origin: * The Funny Farm BBS (604)287-3933 (1:3412/1121) (1:3412/1121)
 

