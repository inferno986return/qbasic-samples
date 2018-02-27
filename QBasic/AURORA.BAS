'_|_|_|   AURORA.BAS
'_|_|_|   Interesting visual effect using palette manipulation.
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released to   PUBLIC DOMAIN   by Kurt Kuzba.  (8/23/96)
DECLARE SUB DefPal (C%, R%, G%, B%)
DECLARE SUB Aurora (C%)
COLOR 1, 0: CLS
COLOR , 7: B$ = CHR$(219)
OUT &H3C7, 7: Rdef% = INP(&H3C9): Gdef% = INP(&H3C9): Bdef% = INP(&H3C9)
LOCATE 9, 19: PRINT STRING$(42, B$);
LOCATE 10, 19: PRINT B$; SPACE$(40); B$;
LOCATE 11, 19: PRINT B$; " This is only an example of what you    "; B$;
LOCATE 12, 19: PRINT B$; " can do when you are able to manipulate "; B$;
LOCATE 13, 19: PRINT B$; " your palette directly. Notice that I   "; B$;
LOCATE 14, 19: PRINT B$; " saved the original colors so I can     "; B$;
LOCATE 15, 19: PRINT B$; " restore the default before moving on.  "; B$;
LOCATE 16, 19: PRINT B$; SPACE$(40); B$;
LOCATE 17, 19: PRINT STRING$(42, B$);
DO: Aurora 7: LOOP WHILE INKEY$ = ""
DefPal 7, Rdef%, Gdef%, Bdef%
SUB Aurora (C%)
   STATIC RD%, GD%, BD%
   OUT &H3C7, 7: R% = INP(&H3C9): G% = INP(&H3C9): B% = INP(&H3C9)
   WHILE RD% = 0: RD% = RND * 3 - 1: WEND
   WHILE GD% = 0: GD% = RND * 3 - 1: WEND
   WHILE BD% = 0: BD% = RND * 3 - 1: WEND
   IF R% < RND * 5 + 48 THEN RD% = 1
   IF G% < RND * 5 + 48 THEN GD% = 1
   IF B% < RND * 5 + 48 THEN BD% = 1
   IF R% > RND * 58 + 5 THEN RD% = -1
   IF G% > RND * 58 + 5 THEN GD% = -1
   IF B% > RND * 58 + 5 THEN BD% = -1
   R% = R% + RD%: G% = G% + GD%: B% = B% + BD%
   WHILE (INP(&H3DA) AND 8) = 0: WEND
   WHILE (INP(&H3DA) AND 8) <> 0: WEND
   OUT &H3C8, C%: OUT &H3C9, R%: OUT &H3C9, G%: OUT &H3C9, B%
   WHILE (INP(&H3DA) AND 8) = 0: WEND
   WHILE (INP(&H3DA) AND 8) <> 0: WEND
END SUB
SUB DefPal (C%, R%, G%, B%)
   OUT &H3C8, C%: OUT &H3C9, R%: OUT &H3C9, G%: OUT &H3C9, B%
END SUB
'_|_|_|   end   AURORA.BAS
