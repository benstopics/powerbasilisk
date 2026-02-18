' L10: Session struct mode (--session-struct)
' Tests that all globals are wrapped into a single %PBSESSIONDATA struct.
' Covers: scalar globals, array globals (top-level DIM and DIM-in-function),
'         string globals, cross-function access, GetSession() export.
' Exit 0 = all tests pass.

GLOBAL Counter AS LONG
GLOBAL Total AS DOUBLE
GLOBAL Items() AS LONG
GLOBAL Prices() AS DOUBLE
GLOBAL Label AS LONG
GLOBAL Tag AS STRING
GLOBAL Names() AS STRING

' Top-level DIM (standard pattern)
DIM Items(1 TO 10)
DIM Prices(1 TO 5)

' DIM inside function body (common SetupDims pattern)
SUB SetupDims()
    DIM Names(1 TO 3)
END SUB

FUNCTION PBMAIN() AS LONG

    CALL SetupDims()

    ' ==== Test 1: scalar LONG global write/read ====
    Counter = 42
    IF Counter <> 42 THEN FUNCTION = 1 : EXIT FUNCTION

    ' ==== Test 2: scalar DOUBLE global ====
    Total = 3.14
    IF Total < 3.13 THEN FUNCTION = 2 : EXIT FUNCTION
    IF Total > 3.15 THEN FUNCTION = 2 : EXIT FUNCTION

    ' ==== Test 3: LONG array write/read ====
    Items(1) = 100
    Items(5) = 500
    Items(10) = 999
    IF Items(1) <> 100 THEN FUNCTION = 3 : EXIT FUNCTION
    IF Items(5) <> 500 THEN FUNCTION = 3 : EXIT FUNCTION
    IF Items(10) <> 999 THEN FUNCTION = 3 : EXIT FUNCTION

    ' ==== Test 4: DOUBLE array ====
    Prices(1) = 10.5
    Prices(3) = 99.99
    Prices(5) = 0.01
    IF Prices(3) < 99.98 THEN FUNCTION = 4 : EXIT FUNCTION
    IF Prices(3) > 100.0 THEN FUNCTION = 4 : EXIT FUNCTION

    ' ==== Test 5: globals modified by SUB ====
    Counter = 0
    CALL IncrementCounter()
    CALL IncrementCounter()
    CALL IncrementCounter()
    IF Counter <> 3 THEN FUNCTION = 5 : EXIT FUNCTION

    ' ==== Test 6: arrays modified by SUB ====
    CALL FillItems()
    IF Items(1) <> 10 THEN FUNCTION = 6 : EXIT FUNCTION
    IF Items(10) <> 100 THEN FUNCTION = 6 : EXIT FUNCTION

    ' ==== Test 7: FUNCTION reading globals ====
    Counter = 7
    Total = 2.5
    IF SumCounterAndTotal() <> 9 THEN FUNCTION = 7 : EXIT FUNCTION

    ' ==== Test 8: multiple globals interact ====
    Label = 0
    CALL ComputeTotal()
    IF Label <> 55 THEN FUNCTION = 8 : EXIT FUNCTION
    IF Total < 5.49 THEN FUNCTION = 8 : EXIT FUNCTION
    IF Total > 5.51 THEN FUNCTION = 8 : EXIT FUNCTION

    ' ==== Test 9: string global ====
    Tag = "hello"
    IF Tag <> "hello" THEN FUNCTION = 9 : EXIT FUNCTION
    Tag = Tag + " world"
    IF Tag <> "hello world" THEN FUNCTION = 9 : EXIT FUNCTION

    ' ==== Test 10: string array (DIM from function body) ====
    Names(1) = "Alice"
    Names(2) = "Bob"
    Names(3) = "Charlie"
    IF Names(1) <> "Alice" THEN FUNCTION = 10 : EXIT FUNCTION
    IF Names(3) <> "Charlie" THEN FUNCTION = 10 : EXIT FUNCTION

    FUNCTION = 0

END FUNCTION

SUB IncrementCounter()
    Counter = Counter + 1
END SUB

SUB FillItems()
    LOCAL i AS LONG
    FOR i = 1 TO 10
        Items(i) = i * 10
    NEXT
END SUB

FUNCTION SumCounterAndTotal() AS LONG
    FUNCTION = Counter + CLNG(Total)
END FUNCTION

SUB ComputeTotal()
    LOCAL i AS LONG
    Label = 0
    Total = 0
    FOR i = 1 TO 10
        Items(i) = i
        Label = Label + i
    NEXT
    Total = Label / 10.0
END SUB
