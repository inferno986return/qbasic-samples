DECLARE SUB testarrays (a%(), b%())
DECLARE FUNCTION CompareArray% (a%(), b%())
CLS
DIM one%(1 TO 10), two%(3 TO 12)
FOR x% = 1 TO 10: one%(x%) = x%: two%(x% + 2) = x%: NEXT
testarrays one%(), two%()
FOR x% = 1 TO 10: one%(x%) = x%: two%(x% + 2) = x% + 1: NEXT
testarrays one%(), two%()

 

FUNCTION CompareArray% (a%(), b%())
  lowA% = LBOUND(a%): highA% = UBOUND(a%)
  lowB% = LBOUND(b%): highB% = UBOUND(b%)
  IF (highA% - lowA%) <> (highB% - lowB%) THEN EXIT FUNCTION
  offset% = lowA% - lowB%
  FOR x% = lowA% TO highA%
    IF a%(x%) <> b%(x% - offset%) THEN EXIT FUNCTION
  NEXT
  CompareArray% = -1
END FUNCTION

SUB testarrays (a%(), b%())
  IF CompareArray%(a%(), b%()) THEN
    PRINT "These arrays are equal"
  ELSE
    PRINT "These arrays are not equal"
  END IF
END SUB

