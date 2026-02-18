' L9: FORMAT$, PARSE$, PARSECOUNT, REMOVE$
' Tests PB runtime library functions.
' Exit 0 = all tests pass.

FUNCTION PBMAIN() AS LONG

    LOCAL s$, result$
    LOCAL n AS LONG

    ' ==== Test 1: FORMAT$ with decimal places ====
    result$ = FORMAT$(3.14159, "0.00")
    IF result$ <> "3.14" THEN FUNCTION = 1 : EXIT FUNCTION

    ' ==== Test 2: FORMAT$ integer ====
    result$ = FORMAT$(42, "0")
    IF result$ <> "42" THEN FUNCTION = 2 : EXIT FUNCTION

    ' ==== Test 3: FORMAT$ with thousands separator ====
    result$ = FORMAT$(1234567, "#,###")
    IF result$ <> "1,234,567" THEN FUNCTION = 3 : EXIT FUNCTION

    ' ==== Test 4: PARSE$ extract first field ====
    s$ = "apple|banana|cherry"
    result$ = PARSE$(s$, "|", 1)
    IF result$ <> "apple" THEN FUNCTION = 4 : EXIT FUNCTION

    ' ==== Test 5: PARSE$ extract second field ====
    result$ = PARSE$(s$, "|", 2)
    IF result$ <> "banana" THEN FUNCTION = 5 : EXIT FUNCTION

    ' ==== Test 6: PARSE$ extract third field ====
    result$ = PARSE$(s$, "|", 3)
    IF result$ <> "cherry" THEN FUNCTION = 6 : EXIT FUNCTION

    ' ==== Test 7: PARSECOUNT ====
    n = PARSECOUNT(s$, "|")
    IF n <> 3 THEN FUNCTION = 7 : EXIT FUNCTION

    ' ==== Test 8: PARSE$ with single item ====
    result$ = PARSE$("hello", "|", 1)
    IF result$ <> "hello" THEN FUNCTION = 8 : EXIT FUNCTION

    ' ==== Test 9: PARSECOUNT single item ====
    n = PARSECOUNT("hello", "|")
    IF n <> 1 THEN FUNCTION = 9 : EXIT FUNCTION

    ' ==== Test 10: REMOVE$ ====
    result$ = REMOVE$("Hello World!", " !")
    IF result$ <> "HelloWorld" THEN FUNCTION = 10 : EXIT FUNCTION

    FUNCTION = 0

END FUNCTION
