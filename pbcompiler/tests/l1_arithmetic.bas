' L1: Variables, arithmetic, IF/ELSE
FUNCTION PBMAIN() AS LONG
  LOCAL x AS LONG
  LOCAL y AS LONG
  LOCAL result AS LONG

  x = 10
  y = 3

  ' Basic arithmetic
  result = x + y       ' 13
  result = x - y       ' 7
  result = x * y       ' 30
  result = x \ y       ' 3 (integer division)
  result = x MOD y     ' 1

  ' IF/ELSE
  IF x > y THEN
    result = 1
  ELSE
    result = 0
  END IF

  ' Nested IF
  IF x = 10 THEN
    IF y = 3 THEN
      result = 42
    END IF
  END IF

  FUNCTION = result
END FUNCTION
