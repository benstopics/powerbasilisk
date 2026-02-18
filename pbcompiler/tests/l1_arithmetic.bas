' L1: Variables, arithmetic, IF/ELSE
FUNCTION PBMAIN() AS LONG
  LOCAL x AS LONG
  LOCAL y AS LONG
  LOCAL result AS LONG

  x = 10
  y = 3

  ' Basic arithmetic
  result = x + y
  IF result <> 13 THEN
    FUNCTION = 1
    EXIT FUNCTION
  END IF

  result = x - y
  IF result <> 7 THEN
    FUNCTION = 2
    EXIT FUNCTION
  END IF

  result = x * y
  IF result <> 30 THEN
    FUNCTION = 3
    EXIT FUNCTION
  END IF

  result = x \ y
  IF result <> 3 THEN
    FUNCTION = 4
    EXIT FUNCTION
  END IF

  result = x MOD y
  IF result <> 1 THEN
    FUNCTION = 5
    EXIT FUNCTION
  END IF

  ' IF/ELSE
  IF x > y THEN
    result = 1
  ELSE
    result = 0
  END IF

  IF result <> 1 THEN
    FUNCTION = 6
    EXIT FUNCTION
  END IF

  ' Nested IF
  IF x = 10 THEN
    IF y = 3 THEN
      result = 42
    END IF
  END IF

  IF result <> 42 THEN
    FUNCTION = 7
    EXIT FUNCTION
  END IF

  FUNCTION = 0  ' All tests passed
END FUNCTION
