' L2: Loops and SELECT CASE
FUNCTION PBMAIN() AS LONG
  LOCAL i AS LONG
  LOCAL sum AS LONG

  ' FOR loop: sum 1..10
  sum = 0
  FOR i = 1 TO 10
    sum = sum + i
  NEXT i

  ' sum should be 55
  IF sum <> 55 THEN
    FUNCTION = 1
    EXIT FUNCTION
  END IF

  ' DO WHILE loop
  sum = 0
  i = 1
  DO WHILE i <= 5
    sum = sum + i
    i = i + 1
  LOOP

  IF sum <> 15 THEN
    FUNCTION = 2
    EXIT FUNCTION
  END IF

  ' SELECT CASE
  LOCAL grade AS LONG
  grade = 85
  LOCAL letter AS LONG

  SELECT CASE grade
    CASE >= 90
      letter = 65  ' A
    CASE >= 80
      letter = 66  ' B
    CASE >= 70
      letter = 67  ' C
    CASE ELSE
      letter = 70  ' F
  END SELECT

  IF letter <> 66 THEN
    FUNCTION = 3
    EXIT FUNCTION
  END IF

  FUNCTION = 0  ' All tests passed
END FUNCTION
