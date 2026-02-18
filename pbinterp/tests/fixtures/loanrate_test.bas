GLOBAL Prime AS SINGLE
GLOBAL GNPRate AS SINGLE
GLOBAL LBondRate AS SINGLE
GLOBAL QTR AS LONG
GLOBAL SUBPRIME AS INTEGER

FUNCTION GetLoanRate(X&) AS DOUBLE
   LOCAL R&, Z#

   R&=X&

   SELECT CASE R&
     CASE 1
        Z#=10 + (.3 * Prime)
        IF Z# < Prime + 3 THEN Z#=Prime + 2 + (RND * 3)
        IF GNPRate < -2 THEN INCR Z#
        IF GNPRate < -7 THEN INCR Z#
     CASE 2
        Z#=(LBondRate + 1.2) - (QTR *.03)
     CASE 3
        Z#=LBondRate + 3 + (.15 * Prime)
        IF Z# < Prime + 2 THEN Z#=Prime + RND(1,3)
        IF SUBPRIME% > 0 THEN
           Z#=Z# + 4.3
        ELSEIF SUBPRIME%=-2 THEN
           Z#=Z# + 2.8
        ELSEIF SUBPRIME%=-1 THEN
           Z#=Z#-1
        END IF
        IF Z# < 7 THEN Z#=7. + (.1 * QTR)
   END SELECT
   FUNCTION=Z#

END FUNCTION

FUNCTION PBMAIN() AS LONG
    Prime = 5.25
    GNPRate = 2.5
    LBondRate = 4.5
    QTR = 2
    SUBPRIME = 0

    LOCAL rate AS DOUBLE
    rate = GetLoanRate(2)
    PRINT "Mortgage rate: " + FORMAT$(rate, "#.####")

    rate = GetLoanRate(1)
    PRINT "Consumer rate: " + FORMAT$(rate, "#.####")

    FUNCTION = 0
END FUNCTION
