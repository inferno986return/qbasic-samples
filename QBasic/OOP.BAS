'_|_|_|   OOP.BAS
'_|_|_|   Example of data encapsulation in QB
'_|_|_|   No warrantee or guarantee is given or implied.
'_|_|_|   Released   PUBLIC DOMAIN   by Kurt Kuzba.  (4/3/97)
DECLARE SUB Users (C$, D$, U%)
TYPE UserData
   UName AS STRING * 40
   Age AS INTEGER
   Address AS STRING * 40
   City AS STRING * 25
   State AS STRING * 2
   Zip AS STRING * 9
   Phone AS STRING * 10
END TYPE
CLS
Users "initarray", "", 100
Users "setname", "Your Name Here", 1
Users "setage", STR$(275), 1
Users "setaddress", "1234 Upda Street", 1
Users "setcity", "Anytown_USA", 1
Users "setstate", "ZZ", 1
Users "setzip", "12345-6789", 1
Users "setphone", "(555) 123-4567", 1
Users "getname", Cmd$, 1: PRINT Cmd$; "  (Age:";
Users "getage", Cmd$, 1: PRINT Cmd$; ")"
Users "getaddress", Cmd$, 1: PRINT Cmd$
Users "getcity", Cmd$, 1: PRINT Cmd$; "  ";
Users "getstate", Cmd$, 1: PRINT Cmd$; "  ";
Users "getzip", Cmd$, 1: PRINT Cmd$
Users "getphone", Cmd$, 1: PRINT "Phone: "; Cmd$
Users "cleararray", "", 0
SYSTEM
SUB Users (C$, D$, U%)
   STATIC USR() AS UserData
   SELECT CASE UCASE$(C$)
      CASE "GETNAME": D$ = RTRIM$(USR(U%).UName)
      CASE "GETAGE": D$ = STR$(USR(U%).Age)
      CASE "GETADDRESS": D$ = RTRIM$(USR(U%).Address)
      CASE "GETCITY": D$ = RTRIM$(USR(U%).City)
      CASE "GETSTATE": D$ = RTRIM$(USR(U%).State)
      CASE "GETZIP": a$ = USR(U%).Zip
         D$ = LEFT$(a$, 5) + "-" + MID$(a$, 6)
      CASE "GETPHONE": a$ = USR(U%).Phone
         t$ = "(" + LEFT$(a$, 3) + ") " + MID$(a$, 4, 3)
         D$ = t$ + "-" + MID$(a$, 7)
      CASE "SETNAME": USR(U%).UName = D$
      CASE "SETAGE": USR(U%).Age = VAL(D$)
      CASE "SETADDRESS": USR(U%).Address = D$
      CASE "SETCITY": USR(U%).City = D$
      CASE "SETSTATE": USR(U%).State = D$
      CASE "SETZIP": USR(U%).Zip = LEFT$(D$, 5) + MID$(D$, 7)
      CASE "SETPHONE": USR(U%).Phone = MID$(D$, 2, 3)
         MID$(USR(U%).Phone, 4) = MID$(D$, 7, 3)
         MID$(USR(U%).Phone, 7) = MID$(D$, 11)
      CASE "INITARRAY": DIM USR(1 TO U%) AS UserData
      CASE "CLEARARRAY": ERASE USR
   END SELECT
END SUB
'_|_|_|   end   OOP.BAS
 
