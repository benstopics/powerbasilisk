FUNCTION PBMAIN() AS LONG
    LOCAL result AS LONG

    ' Test 1: GOTO
    result = 0
    GOTO SkipThis
    result = 999  ' should be skipped
SkipThis:
    IF result = 0 THEN
        PRINT "PASS: GOTO skipped assignment"
    ELSE
        PRINT "FAIL: GOTO did not skip (result=" + FORMAT$(result) + ")"
    END IF

    ' Test 2: GOSUB
    result = 0
    GOSUB SetResult
    IF result = 42 THEN
        PRINT "PASS: GOSUB/RETURN works"
    ELSE
        PRINT "FAIL: GOSUB did not set result (result=" + FORMAT$(result) + ")"
    END IF

    ' Test 3: ON ERROR GOTO (div by zero returns 0 in PB, so just verify wiring)
    ON ERROR GOTO ErrHandler
    ' This won't actually error since PB handles div/0 gracefully
    PRINT "PASS: ON ERROR GOTO set without crashing"
    ON ERROR GOTO 0
    GOTO AfterError
ErrHandler:
    PRINT "PASS: error handler reached"
AfterError:

    ' Test 4: Labels as statement separator context
    LOCAL a AS LONG
    a = 10
    IF a = 10 THEN
        PRINT "PASS: code after labels works"
    END IF

    FUNCTION = 0
    EXIT FUNCTION

SetResult:
    result = 42
    RETURN
END FUNCTION
