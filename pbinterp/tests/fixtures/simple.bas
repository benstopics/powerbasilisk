FUNCTION PBMAIN() AS LONG
    LOCAL x AS LONG
    x = 42
    PRINT "x = " + FORMAT$(x)
    IF x = 42 THEN
        PRINT "PASS: x equals 42"
    ELSE
        PRINT "FAIL: x does not equal 42"
    END IF
    FUNCTION = 0
END FUNCTION
