'$DYNAMIC
CLS : CLEAR
TYPE kilobyte
   K AS STRING * 1024
END TYPE
PRINT "Bytes free  begin ="; FRE(-1)
DIM kb(1 TO 8) AS kilobyte, index(10000) AS INTEGER
PRINT "Bytes free DIMmed ="; FRE(-1)
ERASE kb, index
PRINT "Bytes free  ERASE ="; FRE(-1)

