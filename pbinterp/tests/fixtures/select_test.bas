FUNCTION PBMAIN() AS LONG
    LOCAL x AS LONG
    x = 2

    SELECT CASE x
      CASE 1
        PRINT "one"
      CASE 2
        PRINT "two"
      CASE 3
        PRINT "three"
    END SELECT

    PRINT "after select"
    FUNCTION = 0
END FUNCTION
