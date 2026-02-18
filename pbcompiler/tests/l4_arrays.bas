' L4: Array support (1D, 2D, local, global)

GLOBAL GArr() AS LONG

FUNCTION SumArray(BYVAL n AS LONG) AS LONG
  DIM A(1 TO 5) AS LONG
  LOCAL i AS LONG
  LOCAL total AS LONG
  total = 0
  FOR i = 1 TO n
    A(i) = i * 10
  NEXT i
  FOR i = 1 TO n
    total = total + A(i)
  NEXT i
  FUNCTION = total
END FUNCTION

FUNCTION PBMAIN() AS LONG
  LOCAL i AS LONG
  LOCAL result AS LONG

  ' Test 1: Local 1D array, 0-based
  DIM A(10) AS LONG
  A(0) = 100
  A(5) = 200
  A(10) = 300
  IF A(0) <> 100 THEN
    FUNCTION = 1
    EXIT FUNCTION
  END IF
  IF A(5) <> 200 THEN
    FUNCTION = 2
    EXIT FUNCTION
  END IF
  IF A(10) <> 300 THEN
    FUNCTION = 3
    EXIT FUNCTION
  END IF

  ' Test 2: Local 1D array, 1-based with FOR loop
  DIM B(1 TO 5) AS LONG
  FOR i = 1 TO 5
    B(i) = i * 3
  NEXT i
  IF B(3) <> 9 THEN
    FUNCTION = 4
    EXIT FUNCTION
  END IF

  ' Test 3: Local 2D array
  DIM C(3, 4) AS LONG
  C(0, 0) = 10
  C(1, 2) = 20
  C(3, 4) = 30
  IF C(0, 0) <> 10 THEN
    FUNCTION = 5
    EXIT FUNCTION
  END IF
  IF C(1, 2) <> 20 THEN
    FUNCTION = 6
    EXIT FUNCTION
  END IF
  IF C(3, 4) <> 30 THEN
    FUNCTION = 7
    EXIT FUNCTION
  END IF

  ' Test 4: Global array (GLOBAL + DIM pattern)
  DIM GArr(1 TO 10) AS LONG
  GArr(1) = 111
  GArr(10) = 999
  IF GArr(1) <> 111 THEN
    FUNCTION = 8
    EXIT FUNCTION
  END IF
  IF GArr(10) <> 999 THEN
    FUNCTION = 9
    EXIT FUNCTION
  END IF

  ' Test 5: Array in called function
  result = SumArray(5)
  IF result <> 150 THEN
    FUNCTION = 10
    EXIT FUNCTION
  END IF

  ' Test 6: DOUBLE array
  DIM D(1 TO 3) AS DOUBLE
  D(1) = 1.5
  D(2) = 2.5
  D(3) = 3.0
  LOCAL dsum AS DOUBLE
  dsum = D(1) + D(2) + D(3)
  IF dsum < 6.9 THEN
    FUNCTION = 11
    EXIT FUNCTION
  END IF
  IF dsum > 7.1 THEN
    FUNCTION = 12
    EXIT FUNCTION
  END IF

  FUNCTION = 0
END FUNCTION
