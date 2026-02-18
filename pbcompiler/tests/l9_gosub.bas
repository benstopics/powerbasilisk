' L9: GOSUB/RETURN, GOTO, Labels
' Tests GOSUB/RETURN dispatching with multiple targets,
' numeric labels, and GOTO forward jumps.
' Exit 0 = all tests pass.

FUNCTION PBMAIN() AS LONG

    LOCAL result AS LONG
    LOCAL total AS LONG

    ' ==== Test 1: Simple GOSUB/RETURN ====
    result = 0
    GOSUB AddTen
    IF result <> 10 THEN FUNCTION = 1 : EXIT FUNCTION

    ' ==== Test 2: Multiple GOSUBs ====
    result = 0
    GOSUB AddTen
    GOSUB AddTen
    GOSUB AddTen
    IF result <> 30 THEN FUNCTION = 2 : EXIT FUNCTION

    ' ==== Test 3: Two different GOSUB targets ====
    total = 0
    result = 0
    GOSUB AddFive
    GOSUB AddTen
    IF total <> 5 THEN FUNCTION = 3 : EXIT FUNCTION
    IF result <> 10 THEN FUNCTION = 30 : EXIT FUNCTION

    ' ==== Test 4: GOTO forward jump ====
    result = 100
    GOTO SkipAssign
    result = 999
SkipAssign:
    IF result <> 100 THEN FUNCTION = 4 : EXIT FUNCTION

    ' ==== Test 5: Numeric label with GOSUB ====
    result = 0
    GOSUB 30300
    IF result <> 42 THEN FUNCTION = 5 : EXIT FUNCTION

    ' ==== Test 6: GOSUB with local var modification ====
    LOCAL x AS LONG
    x = 5
    GOSUB DoubleX
    IF x <> 10 THEN FUNCTION = 6 : EXIT FUNCTION
    GOSUB DoubleX
    IF x <> 20 THEN FUNCTION = 7 : EXIT FUNCTION

    FUNCTION = 0
    EXIT FUNCTION

    ' ---- Subroutine targets ----
AddTen:
    result = result + 10
    RETURN

AddFive:
    total = total + 5
    RETURN

30300:
    result = 42
    RETURN

DoubleX:
    x = x * 2
    RETURN

END FUNCTION
