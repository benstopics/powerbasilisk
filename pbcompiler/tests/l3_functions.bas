' L3: SUB/FUNCTION calls with BYVAL/BYREF
FUNCTION AddTwo(BYVAL a AS LONG, BYVAL b AS LONG) AS LONG
  FUNCTION = a + b
END FUNCTION

FUNCTION Factorial(BYVAL n AS LONG) AS LONG
  IF n <= 1 THEN
    FUNCTION = 1
  ELSE
    FUNCTION = n * Factorial(n - 1)
  END IF
END FUNCTION

SUB DoubleIt(x AS LONG)
  ' x is BYREF by default
  x = x * 2
END SUB

FUNCTION PBMAIN() AS LONG
  LOCAL result AS LONG

  ' Test BYVAL function call
  result = AddTwo(3, 4)
  IF result <> 7 THEN
    FUNCTION = 1
    EXIT FUNCTION
  END IF

  ' Test recursion
  result = Factorial(5)
  IF result <> 120 THEN
    FUNCTION = 2
    EXIT FUNCTION
  END IF

  ' Test BYREF sub call
  LOCAL val AS LONG
  val = 21
  CALL DoubleIt(val)
  IF val <> 42 THEN
    FUNCTION = 3
    EXIT FUNCTION
  END IF

  FUNCTION = 0  ' All tests passed
END FUNCTION
