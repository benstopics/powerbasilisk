' L11: VARPTR — get address of variables and array elements
FUNCTION PBMAIN() AS LONG
  LOCAL x AS LONG
  LOCAL y AS LONG
  LOCAL arr(10) AS LONG

  x = 42
  y = 99

  ' VARPTR should return non-zero addresses
  LOCAL addr_x AS LONG
  LOCAL addr_y AS LONG
  addr_x = VARPTR(x)
  addr_y = VARPTR(y)

  IF addr_x = 0 THEN FUNCTION = 1: EXIT FUNCTION
  IF addr_y = 0 THEN FUNCTION = 2: EXIT FUNCTION

  ' Different variables should have different addresses
  IF addr_x = addr_y THEN FUNCTION = 3: EXIT FUNCTION

  ' VARPTR on array element
  arr(0) = 100
  arr(5) = 200
  LOCAL addr_arr0 AS LONG
  LOCAL addr_arr5 AS LONG
  addr_arr0 = VARPTR(arr(0))
  addr_arr5 = VARPTR(arr(5))

  IF addr_arr0 = 0 THEN FUNCTION = 4: EXIT FUNCTION
  IF addr_arr5 = 0 THEN FUNCTION = 5: EXIT FUNCTION
  IF addr_arr0 = addr_arr5 THEN FUNCTION = 6: EXIT FUNCTION

  ' Array elements should be 20 bytes apart (5 * 4 bytes for LONG)
  LOCAL diff AS LONG
  diff = addr_arr5 - addr_arr0
  IF diff <> 20 THEN FUNCTION = 7: EXIT FUNCTION

  FUNCTION = 0  ' All tests passed
END FUNCTION
