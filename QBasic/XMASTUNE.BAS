'============================================================================
'     To: ALL                           Number: 191      Refer: 0     
'   From: AARON TUCKER              Conference: QUIK_BAS -Qui (298)
'   Date: 12-11-97 23:27              BBS Name: YE OLDE PAWN SHOPPE ]I[
'Subject: Tune.Bas [1/5]                BBS ID: YOPSIII
'----------------------------------------------------------------------------

'>>> Page 1 of TUNE.BAS begins here. TYPE:BAS
'** 3-AM SOFTWARE PRESENTS
'** The Christmas Challenge
'** QB Juke
'** As anyone who's seen a graphical program from me you know
'** I'm no good in any screen mode past 2 :)
'** But I'm not bad at all when it comes to text mode.
'** (see help menu for more information)
DECLARE SUB Top.Menu (sel, sel$(), fgc!, bgc!, msg$)
DECLARE SUB WWYAMC ()
DECLARE SUB HFTH ()
DECLARE SUB OCAYF ()
DECLARE SUB OLTOB ()
DECLARE SUB SN ()
DECLARE SUB HHAS ()
DECLARE SUB JTTW ()
DECLARE SUB OHN ()
DECLARE SUB WWAMC ()
DECLARE SUB NS ()
DIM sel$(13, 10)
sel$(0, 0) = " Exit "
sel$(1, 0) = " Side 1 "
sel$(2, 0) = " Side 2 "
sel$(3, 0) = " Help "
sel$(0, 10) = "Press ENTER or ESC to Exit QB Xmas-Juke"
sel$(1, 10) = "Play some tunes"
sel$(2, 10) = "Play some tunes"
sel$(3, 10) = "Help"
'Exit Menu (0,0)
sel$(0, 1) = ""
'Tunes (1,0)
sel$(1, 1) = " Home For The Holidays "
sel$(1, 2) = " O Come All Ye Faithful "
sel$(1, 3) = " O Little Town of Bethlehem "
sel$(1, 4) = " Silent Night "
sel$(1, 5) = " Hark! The Herald Angels Sing "
sel$(1, 6) = ""
sel$(2, 1) = " Joy to The World "
sel$(2, 2) = " O Holy Night"
sel$(2, 3) = " We Wish You a Merry Christmas "
sel$(2, 4) = " Excerpts from The Nutcracker "
sel$(2, 5) = ""
sel$(3, 1) = " About QB-JUKE "
sel$(3, 2) = " How To Navigate "
sel$(3, 3) = " Credits "
sel$(3, 4) = ""
msg$ = " [=] 3-AM SOFTWARE Christmas 97 Production [=] "
Begin: fgc = 7
bgc = 1
CALL Top.Menu(sel, sel$(), fgc!, bgc!, msg$)
'if its 'Esc' then end
IF sel = -1 THEN END
SELECT CASE sel
CASE IS = 1
COLOR 7, 0: CLS : PRINT "Merry Christmas!"
SYSTEM
CASE IS = 11: CALL HFTH:  GOTO Begin
CASE IS = 12: CALL OCAYF: GOTO Begin
CASE IS = 13: CALL OLTOB: GOTO Begin
CASE IS = 14: CALL SN: GOTO Begin
CASE IS = 15: CALL HHAS: GOTO Begin
CASE IS = 21: CALL JTTW: GOTO Begin
CASE IS = 22: CALL OHN: GOTO Begin
CASE IS = 23: CALL WWYAMC: GOTO Begin
CASE IS = 24: CALL NS: GOTO Begin
CASE IS = 31
COLOR 15, 5
LOCATE 10, 15: PRINT " +-------------------------------------------+ "
LOCATE 11, 15: PRINT " |   A 3-AM SOFWARE PRODUCT - Aaron Tucker   | "
LOCATE 12, 15: PRINT " +-------------------------------------------+ "
LOCATE 13, 15: PRINT " | This product is PUBLIC DOMAIN meaning you | "
LOCATE 14, 15: PRINT " | may change, copy, rewrite, or whatever    | "
LOCATE 15, 15: PRINT " | you feel like doing to it.  Please give   | "
LOCATE 16, 15: PRINT " | a little credit though if you find it the | "
LOCATE 17, 15: PRINT " | slightest bit useful!  Thanks!            | "
LOCATE 18, 15: PRINT " |                                           | "
LOCATE 19, 15: PRINT " |             <PRESS ANY KEY>               | "
LOCATE 20, 15: PRINT " +-------------------------------------------+ "
DO: LOOP UNTIL INKEY$ <> ""
GOTO Begin
CASE IS = 32
COLOR 15, 4
LOCATE 10, 15: PRINT " +-------------------------------------------+ "
LOCATE 11, 15: PRINT " |   If You Are Reading This You Got It!     | "
LOCATE 12, 15: PRINT " +-------------------------------------------+ "
LOCATE 13, 15: PRINT " | But just in case, you can scroll through  | "
LOCATE 14, 15: PRINT " | the bar menu with the left and right arr- | "
LOCATE 15, 15: PRINT " | ow keys.  When a menu pops down, use the  | "
LOCATE 16, 15: PRINT " | arrows and ENTER to select an option or   | "
LOCATE 17, 15: PRINT " | press the number corresponding to the     | "
LOCATE 18, 15: PRINT " | option you want to select.                | "
LOCATE 19, 15: PRINT " |             <PRESS ANY KEY>               | "
LOCATE 20, 15: PRINT " +-------------------------------------------+ "
DO: LOOP UNTIL INKEY$ <> ""
GOTO Begin
CASE IS = 33
COLOR 15, 3
LOCATE 10, 15: PRINT " +-------------------------------------------+ "
LOCATE 11, 15: PRINT " |   This program is possible thanks to:     | "
LOCATE 12, 15: PRINT " +-------------------------------------------+ "
LOCATE 13, 15: PRINT " | Chris Gunn - Mr. Select Case              | "
LOCATE 14, 15: PRINT " | Bill White - a file on arrow keys in QB   | "
LOCATE 15, 15: PRINT " | QB Echo    - 3 years of fun!              | "
LOCATE 16, 15: PRINT " | Pat Pridgen - I always used to enter his  | "
LOCATE 17, 15: PRINT " | name as Pat Prigden on SBA and I still    | "
LOCATE 18, 15: PRINT " | say it that way! Weird huh?               | "
LOCATE 19, 15: PRINT " |             <PRESS ANY KEY>               | "
LOCATE 20, 15: PRINT " +-------------------------------------------+ "
DO: LOOP UNTIL INKEY$ <> ""
GOTO Begin
CASE ELSE
GOTO Begin
END SELECT

SUB HFTH
PLAY "t144l4o2ef l2gl4ec l2o3cl4o2ba gfl2e"
PLAY " l4cdl2el4d#e agfe l2d"
PLAY "l4ef l2gl4ec o3dco2ba o3co2el2g"
PLAY "l4go3dc o2l4bgaf gefdl1c"
PLAY "t144l4o2ef l2gl4ec l2o3cl4o2ba gfl2e"
PLAY " l4cdl2el4d#e agfe l2d"
PLAY "l4ef l2gl4ec o3dco2ba o3co2el2g"
PLAY "l4go3dc o2l4bgaf l2o2gefo3dl1cc"
END SUB

SUB HHAS
HA$ = "o3>DP64DP64DC<B-AB-2P32"
HA1$ = ">CP64CP64C<FB-AP64AGP32"
HA2$ = "GA8B-8>C.<F8P64FGF2"
PLAY "<<T170MLCFP64F.E8FAP64AGP32>C"
PLAY "P64CP64C.<B-8AGA2P8CFP64F.E8F"
PLAY "AP64AGP32>C<GP64G.E8P64EDC2P8"
PLAY "X" + VARPTR$(HA1$)
PLAY "X" + VARPTR$(HA1$)
PLAY "P64X" + VARPTR$(HA$)
PLAY "GA8B-8>C.<F8P64FGA2P16>T160D."
PLAY "P64D8P64DC<B-AB-2P32"
PLAY "X" + VARPTR$(HA2$)
PLAY "P16T150GA8B-8>T130C.<F8P64"
PLAY "T120F.T110G.T100F1P4"
END SUB

SUB JTTW
PLAY "T110MLo3Co2B8.A16G.P16F8EDC.P8"
PLAY "G8A.P16A8B.P16B8o3C2P8C8P64C8o2"
PLAY "B8A8G8P64G8.F16E8P32o3C8P64C8o2"
PLAY "B8A8G8P64G8.F16E8P32E8P64E8P64"
PLAY "MNE8P64MLE8P64E16F16G.P16F16E16"
PLAY "D8P64D8P64MND8P64MLD16E16F.P16"
PLAY "E16D16C8o3CP32o2A8G8.F16E8P32T100"
PLAY "F8P64EP64DP64C2P4"
END SUB

SUB NS
PLAY "o5t114"
NCG$ = "MSG8P64MNG16F#16MSG8G8E8D8MNC8MSE8D8P64MND16C#16MSD8D8<B8A8"
NCA$ = "G8MNB16>D16<MSA8MNA16>D16<MSG8MNG16>D16<MSF#8F#16>D16<A8M"
NCA$ = NCA$ + "NA16>D16<MSB8MN"
NCB$ = "B16>D16"
NC1$ = "MS<A8MNA16>D16<MSG8E8G8MN"
NC2$ = "<MSG8MNG16>D16MS<A8G8A8MN>D16E16F#16"
PLAY "<<T160X" + VARPTR$(NCG$)
PLAY "X" + VARPTR$(NCA$)
PLAY "X" + VARPTR$(NCB$)
PLAY "X" + VARPTR$(NC2$) + "T170"
PLAY "X" + VARPTR$(NCG$)
PLAY "X" + VARPTR$(NCA$)
PLAY "X" + VARPTR$(NCB$) + "T180"
PLAY "X" + VARPTR$(NC1$) + ">>D16E16"
PLAY "F#16X" + VARPTR$(NCG$)
PLAY "X" + VARPTR$(NCA$)
PLAY "X" + VARPTR$(NCB$)
PLAY "X" + VARPTR$(NC2$) + "T190"
PLAY "X" + VARPTR$(NCG$)
PLAY "X" + VARPTR$(NCA$)
PLAY "X" + VARPTR$(NCB$) + "T200<A8MNA16>D16<MSG8MLG16>D16MS<B8ML"
PLAY "b16>D16MS<A8MLA16>D16MS<G8MLG16>D16<MSF#8MLF#16>D16<MSA8"
PLAY "MLA16>D16MS<B8MLB16>D16<MSA8MLA16>D16MS<G8MLG16>D16MS<B8MLB16>"
PLAY "D16MS<A8MLA16>D16MSC8MLC16D16MS<B8MLB16>D16MS<G8ML"
PLAY "G16>D16MS<A8MLA16>D16MSC8MLC16D16<MSB8MLB16>D16MS<G8MLG16>D16"
PLAY "<MSA8MLA16>D16<MSG8MLG16>D16<MSA8MLA16>D16<MSG8ML"
PLAY "G16>D16<T190MSA8MLA16>T180D16MS<G8MLG16>T170D16MS<A8ML>D8<G16"
PLAY "B16>D16G8P4P4"
DR$ = "F#64A64>MSD16C#16D16P64C#16D8C#8MN<"
DR$ = DR$ + "G64A64>MSE8MND32F#32A32>D32F#<"
DRA$ = "B32>D32MSG16F#16G16F#16E16D16<A16F#16MND>D16C#D16<<B16B16"
DRB$ = "B16>C#32<MSB8A8"
DR1$ = "MSB16A16B16A16B16A16B16A16B16A16B16A16"
PLAY "<<T100MNX" + VARPTR$(DR$)
PLAY "X" + VARPTR$(DRA$)
PLAY "X" + VARPTR$(DRB$) + ">MNB16<B16MSB16B16MN>C#32<MSB8A8>MNB16"
PLAY "D16MSD16D16D16"
PLAY "C#16E16D16<MNE64G64>D16C#16MS<B16A16>G8MNE32C#32<G32E32"
PLAY "X" + VARPTR$(DR$)
PLAY "X" + VARPTR$(DRA$)
PLAY "X" + VARPTR$(DRB$) + "MN>B16D16T110D16D16E32MSD8C#8MNB16F#16"
PLAY "MSF#16F#16F#16"
PLAY "E16F#16E16A16G#16A16G#16"
PLAY "X" + VARPTR$(DR1$)
PLAY "X" + VARPTR$(DR1$)
PLAY "T100B16A16<B16A16B16A16B16A16B16A16B16A16B16A16MND64G64"
PLAY "MSB16MNE64G64MS>C#16<A16B16>C16C#16<A16B16>C16"
PLAY "C#16<MNE32D32C#32<B32"
PLAY "A32G32F#32E32E32D32C#32<B32A32G32F#32E32>>"
PLAY "X" + VARPTR$(DR$)
PLAY "X" + VARPTR$(DRA$)
PLAY "X" + VARPTR$(DRB$) + ">MNB16<B16MSB16B16MN>C#32<MS"
PLAY "B8A8>MNB16D16MSD16D16D16"
PLAY "C#16E16D16<MNE64G64>D16C#16MS<B16A16>G8MNE32C#32<G32E32D16F#16"
PLAY "A16<D32>D32F#32A32>DP4P4"
MT$ = "MND8P8D12D12D12E8P8E8P8F#8P8D8P8"
MT1$ = "MLE16>C8P16D16C8P16<B16A8P16G16F#8P16"
MT2$ = "MSB8P8MNB12B12B12G8P8E8P8MSC8P8"
MT3$ = "MLD16B8P16>C16<B8P16A16G8P16F#16E8P16G16F#8P16"
MT3$ = MT3$ + "E16D#8P16A16G8P16F#16E8P16B16>C8P16<B16A8"
MT3$ = MT3$ + "P16G16MS>D8P8MNA64B64>C64C#64MS"
PLAY "<T150X" + VARPTR$(MT$) + "E2"
PLAY "X" + VARPTR$(MT$) + "E.P16<"
PLAY "X" + VARPTR$(MT1$) + "T160"
PLAY "X" + VARPTR$(MT3$)
PLAY "D8P8<T150X" + VARPTR$(MT$) + "E2"
PLAY "X" + VARPTR$(MT$) + "E.P16<"
PLAY "X" + VARPTR$(MT1$) + "T160A16>D8P16E16D8P16C16<"
PLAY "B8P16A16G8P16B16>T165E8P16D16C8P16E16F#8P16E16D8P16F#16"
PLAY "MSG8P16MLF#16E8P16F#16MSD#8P8ML"
PLAY "F#64G64A64A#64MNB8P8<X" + VARPTR$(MT2$) + "<A8P8F#8<B8>C#8D#8E8"
PLAY "F#8G8G#8B8>C8<B8A8G8F#8E8D#8C#8<B8P8>>"
PLAY "X" + VARPTR$(MT2$) + "<MNA8P8<D#32B32>F#8>>B8A#8A8G8F#8"
PLAY "E8D8C8<B8A8G8F#8E8D8P8>>MNC#16MSD8P8<<D8P8>D8P8T150MND12D12D12"
PLAY "E8P8E8P8F#8P8D8P8E2T160X" + VARPTR$(MT$) + "E.P16<"
PLAY "X" + VARPTR$(MT1$) + "T170"
PLAY "X" + VARPTR$(MT3$)
PLAY "D8P8<GP4<<<G16G16G16G8P4"
END SUB

SUB OCAYF
PLAY "o4<<T170MLGP64G2DGA2D2P32BAB>C<"
PLAY "B2AP32GP64G2F#EF#GAP32BF#2MNE."
PLAY "P32MSD8MND1MLP16>D2C<B>C2<B2"
PLAY "P32ABGAF#.E8DP16GP64GF#GAG2D"
PLAY "P32BP64BAB>C<B2AP32B>C<BAGF#2"
PLAY "G8P64G#16A64A#64B64>CP16<T160"
PLAY "B2T150A.P32.MSG8MNG1P4"
END SUB

SUB OHN
OH$ = "o3EP64E8.P64E16G.P16G16A8.P64"
OHA$ = "A16F8.A16>C2<G8.P64G16"
OH1$ = "E8.D16CE8.F16GF8.D16C2.P4"
OH2$ = "F#8.E16BG8.A16B>C8.<B16E2."
OH3$ = "GADP32GA8.G16>C8.<E16"
PLAY "<<T100ML"
PLAY "X" + VARPTR$(OH$)
PLAY "X" + VARPTR$(OHA$)
PLAY "X" + VARPTR$(OH1$)
PLAY "X" + VARPTR$(OH$)
PLAY "X" + VARPTR$(OHA$)
PLAY "X" + VARPTR$(OH2$)
PLAY "P8G8P64"
PLAY "X" + VARPTR$(OH3$)
PLAY "AG8.P64G16P64"
PLAY "X" + VARPTR$(OH3$)
PLAY "G2.P4>C2.<B8.A16B2.P64B>D2.<"
PLAY "A16P64A8.P64A16>C2P64C.P16CE2."
PLAY "D.P16<G>C2.P8<B8.A16G2.P64G16"
PLAY "A8.G16P64G2.P16>CD2.P16<G>E2."
PLAY "P16DC2P16T90<B8.P64B16>C8.D16C1P4"
END SUB

SUB OLTOB
PLAY "o2T120MLAP64AP64AG#Ao3Co2B-DP16GF"
PLAY "E8F8GCA2.P16AP64AP64Ao3DCP64Co2B-DP32G"
PLAY "FE8F8AGF2.P8AP64AP64AGFE2P64EP32EDEFGA2.P16AP64AP64AG#A"
PLAY "o3Co2B-DP16o3DCo2FA.G8F1P4o3"
END SUB

SUB SN
PLAY "o4<<T120MLG.A8GE2.P16G.A8GE2.P8>"
PLAY "D2P64D<B2.P16>C2P64C<G2.P16A2"
PLAY "P64A>C.<B8AG.A8GE2.P16A2P64A"
PLAY ">C.<B8AG.A8GE2.P16>D2P64DF."
PLAY "D8<B>C2.E2.P16T110C.<G8EG.F8"
PLAY "DC1.P4"
END SUB

SUB Top.Menu (sel, sel$(), fgc, bgc, msg$)
DIM a(20)
s$ = ""
a = 0
'================== clr screen
COLOR fgc, bgc
FOR i = 1 TO 25
LOCATE i, 1
PRINT STRING$(80, "X");
NEXT
'================== init line 25
LOCATE 25, 1
COLOR bgc, fgc
PRINT SPACE$(80);
COLOR fgc, bgc
i = -1
DO
i = i + 1
a(i) = LEN(sel$(i, 0))
z$ = LTRIM$(sel$(i, 0))
s$ = s$ + UCASE$(LEFT$(z$, 1))
LOOP WHILE sel$(i, 0) <> ""
sel = i - 1
LOCATE 1, 1
COLOR fgc, bgc
t = INT((78 - LEN(msg$)) / 2)
IF t * 2 + LEN(msg$) < 78 THEN f$ = STRING$((78 - (t * 2 + LEN(msg$))), "-") ELSE f$ = ""
PRINT "+" + STRING$(t, "-") + msg$ + f$ + STRING$(t, "-") + "+";
PRINT "|" + SPACE$(78) + "|";
LOCATE 2, 2
FOR i = 0 TO sel
PRINT sel$(i, 0);
NEXT
LOCATE 3, 1
PRINT "+" + STRING$(78, "-") + "+";
subsel = 1
subnum = 1
zold = 2
s = 0
x = 2
GOSUB dis.sub
lp:
oldx = x
x = 2
FOR i = 0 TO s
x = x + LEN(sel$(i, 0))
NEXT
x = x - LEN(sel$(i - 1, 0))
COLOR fgc, bgc
LOCATE 2, oldx
PRINT sel$(olds, 0);
COLOR 15, fgc
LOCATE 2, x
PRINT sel$(s, 0);
t = INT((80 - LEN(sel$(s, 10))) / 2)
IF t * 2 + LEN(sel$(s, 10)) < 78 THEN
f$ = STRING$((78 - (t * 2 + LEN(sel$(s, 10)))), "-")
ELSE f$ = ""
END IF
LOCATE 25, 1
COLOR bgc, fgc
PRINT SPACE$(t) + sel$(s, 10) + f$ + SPACE$(t);
COLOR fgc, bgc
get.key:
DO
a$ = INKEY$
LOOP WHILE a$ = ""
IF LEN(a$) > 1 THEN GOTO get.curkey
a$ = UCASE$(a$)
IF a$ = CHR$(27) THEN sel = -1: EXIT SUB
ret:   IF a$ <> CHR$(13) GOTO test.num
sel = (s * 10) + subnum: EXIT SUB
test.num:
q = VAL(a$)
IF q >= 1 AND q <= cv AND q <= 9 AND q > 0 THEN
subsel = q
GOSUB update.sub
a$ = CHR$(13): GOTO ret
END IF
test.ltr:
IF c <> 0 THEN
c = c + 1
c = INSTR(c, s$, a$)
IF c <> 0 GOTO tr
END IF
c = INSTR(s$, a$)
IF c = 0 GOTO get.key
tr:
olds = s
s = c - 1
subsel = 1
subnum = 1
GOSUB dis.sub
GOTO lp
get.curkey:
a = ASC(RIGHT$(a$, 1))
IF a <> 77 AND a <> 75 AND a <> 72 AND a <> 80 GOTO get.key
olds = s
IF a <> 77 AND a <> 75 GOTO get.updnkey
IF a = 77 THEN s = s + 1
IF a = 75 THEN s = s - 1
IF s > sel THEN s = 0
IF s < 0 THEN s = sel
c = s
subsel = 1
subnum = 1
GOSUB dis.sub
GOTO lp
get.updnkey:
IF a = 80 THEN subsel = subsel + 1
IF a = 72 THEN subsel = subsel - 1
GOSUB update.sub
GOTO lp
dis.sub:
i = 0
a = 0
xtemp = x
COLOR fgc, bgc
FOR i = 1 TO cv + 2
LOCATE 3 + i, zold - 1
PRINT STRING$(aold + 6, "X")
NEXT
i = 0
DO
i = i + 1
IF LEN(sel$(s, i)) > a THEN a = LEN(sel$(s, i))
LOOP WHILE sel$(s, i) <> "" AND i < 10
cv = 0
IF i = 1 THEN RETURN
aold = a
cvold = cv
cv = i - 1
cvold = cv
x = 2
FOR i = 0 TO s
x = x + LEN(sel$(i, 0))
NEXT
x = x - LEN(sel$(i - 1, 0))
IF x + a > 78 THEN z = 76 - a ELSE z = x - 5
IF z < 2 THEN z = 2
zold = z
LOCATE 4, z - 1: PRINT "+" + STRING$(x - z, "-");
LOCATE 4, x: PRINT "+" + SPACE$(LEN(sel$(s, 0)) - 2) + "+";
b = x + LEN(sel$(s, 0)) - 1
n = z + a + 3
PRINT STRING$((n) - b, "-") + "+";
FOR i = 1 TO cv
LOCATE i + 4, z - 1: PRINT "|";
PRINT LTRIM$(STR$(i)) + ". " + sel$(s, i) + SPACE$(a - (LEN(sel$(s, i)) - 1)) + "|";
NEXT
LOCATE i + 4, z - 1: PRINT "+" + STRING$(a + 4, "-") + "+";
x = xtemp
update.sub:
IF cv = 0 THEN RETURN
IF subsel > cv THEN subsel = 1
IF subsel < 1 THEN subsel = cv
LOCATE subnum + 4, z
PRINT LTRIM$(STR$(subnum)) + ". " + sel$(s, subnum);
LOCATE subsel + 4, z
COLOR 15, fgc
PRINT LTRIM$(STR$(subsel)) + ". " + sel$(s, subsel);
subnum = subsel
RETURN
END SUB

SUB WWYAMC
PLAY "o2T159MNDMSGMNP64G8A8G8F#8EMSE"
PLAY "MNP32EMSAMNP64A8B8A8G8F#MSDMN"
PLAY "P32DMSBMNP64B8o3C8o2B8A8GMSEMN"
PLAY "P32D8D8EAF#G2.P4"
PLAY "DGGGMLF#2MNP32F#GF#ED2P32ABAG8"
PLAY "G8o3MSDP64o2DMNP64D8D8EAF#G2.P4o3"
END SUB

