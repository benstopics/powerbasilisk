' L7: User-defined TYPEs (structs)

TYPE Point
  x AS LONG
  y AS LONG
END TYPE

TYPE MixedRec
  count AS LONG
  value AS DOUBLE
  flag AS LONG
END TYPE

SUB SetPoint(p AS Point, BYVAL px AS LONG, BYVAL py AS LONG)
  p.x = px
  p.y = py
END SUB

FUNCTION SumFields(r AS MixedRec) AS DOUBLE
  FUNCTION = r.count + r.value + r.flag
END FUNCTION

FUNCTION PBMAIN() AS LONG
  LOCAL p AS Point
  LOCAL q AS Point
  LOCAL r AS MixedRec

  ' Test 1: Assign and read TYPE members (LONG)
  p.x = 10
  p.y = 20
  IF p.x <> 10 THEN
    FUNCTION = 1
    EXIT FUNCTION
  END IF
  IF p.y <> 20 THEN
    FUNCTION = 2
    EXIT FUNCTION
  END IF

  ' Test 2: Arithmetic on TYPE members
  IF p.x + p.y <> 30 THEN
    FUNCTION = 3
    EXIT FUNCTION
  END IF

  ' Test 3: Multiple instances are independent
  q.x = 100
  q.y = 200
  IF p.x <> 10 THEN
    FUNCTION = 4
    EXIT FUNCTION
  END IF
  IF q.x <> 100 THEN
    FUNCTION = 5
    EXIT FUNCTION
  END IF

  ' Test 4: Mixed types (LONG + DOUBLE)
  r.count = 5
  r.value = 3.14
  r.flag = 1
  IF r.count <> 5 THEN
    FUNCTION = 6
    EXIT FUNCTION
  END IF
  IF r.flag <> 1 THEN
    FUNCTION = 7
    EXIT FUNCTION
  END IF

  ' Test 5: Pass TYPE to SUB (BYREF) — modify members inside sub
  CALL SetPoint(p, 42, 99)
  IF p.x <> 42 THEN
    FUNCTION = 8
    EXIT FUNCTION
  END IF
  IF p.y <> 99 THEN
    FUNCTION = 9
    EXIT FUNCTION
  END IF

  ' Test 6: FUNCTION reading TYPE member
  r.count = 10
  r.value = 2.5
  r.flag = 3
  LOCAL total AS DOUBLE
  total = SumFields(r)
  IF total < 15.4 THEN
    FUNCTION = 10
    EXIT FUNCTION
  END IF
  IF total > 15.6 THEN
    FUNCTION = 11
    EXIT FUNCTION
  END IF

  ' Test 7: Array of TYPE
  DIM arr(3) AS Point
  arr(0).x = 1
  arr(0).y = 2
  arr(1).x = 10
  arr(1).y = 20
  arr(2).x = 100
  arr(2).y = 200

  IF arr(0).x <> 1 THEN
    FUNCTION = 12
    EXIT FUNCTION
  END IF
  IF arr(1).y <> 20 THEN
    FUNCTION = 13
    EXIT FUNCTION
  END IF
  IF arr(2).x <> 100 THEN
    FUNCTION = 14
    EXIT FUNCTION
  END IF

  ' Test 8: Array of TYPE — sum check
  LOCAL sum AS LONG
  sum = arr(0).x + arr(1).x + arr(2).x
  IF sum <> 111 THEN
    FUNCTION = 15
    EXIT FUNCTION
  END IF

  ' Test 9: Assign member from expression
  p.x = arr(1).x + arr(2).y
  IF p.x <> 210 THEN
    FUNCTION = 16
    EXIT FUNCTION
  END IF

  FUNCTION = 0
END FUNCTION
