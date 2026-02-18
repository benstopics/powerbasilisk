FUNCTION PBMAIN() AS LONG
    ' Test 1: Basic string variable (starts uninitialized = should be empty)
    DIM s AS STRING
    DIM n AS LONG
    n = LEN(s)
    IF n <> 0 THEN FUNCTION = 1: EXIT FUNCTION

    ' Test 2: String concatenation with uninitialized
    DIM t AS STRING
    t = "Hello"
    DIM u AS STRING
    u = t & " World"
    IF LEN(u) <> 11 THEN FUNCTION = 2: EXIT FUNCTION

    ' Test 3: Concat with null/empty string
    DIM v AS STRING
    v = s & t
    IF LEN(v) <> 5 THEN FUNCTION = 3: EXIT FUNCTION

    FUNCTION = 0
END FUNCTION
