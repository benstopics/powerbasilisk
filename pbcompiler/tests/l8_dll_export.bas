' L8: DLL export, external imports, and linking
' Tests: DECLARE parsing, EXPORT/ALIAS on FUNCTION/SUB, dllimport/dllexport codegen
'
' This test verifies that DECLARE statements parse correctly and that
' EXPORT/ALIAS annotations produce the right LLVM IR attributes.
' It does NOT actually call external DLL functions (no DLL roundtrip),
' but it exercises the full parse → codegen → compile pipeline.

' Forward declaration (no LIB — same compilation unit, should be ignored)
DECLARE FUNCTION Helper(BYVAL x AS LONG) AS LONG

' External DLL import (will appear as dllimport in .ll)
DECLARE FUNCTION GetTickCount LIB "KERNEL32.DLL" ALIAS "GetTickCount" () AS LONG

' External DLL import with params
DECLARE FUNCTION MessageBeep LIB "USER32.DLL" ALIAS "MessageBeep" (BYVAL uType AS LONG) AS LONG

' External SUB import
DECLARE SUB ExitProcess LIB "KERNEL32.DLL" ALIAS "ExitProcess" (BYVAL uExitCode AS LONG)

' DECLARE with calling convention (should be skipped gracefully)
DECLARE FUNCTION GetLastError CDECL LIB "KERNEL32.DLL" ALIAS "GetLastError" () AS LONG

' Exported function with ALIAS
FUNCTION Add2 ALIAS "Add2" (BYVAL a AS LONG, BYVAL b AS LONG) EXPORT AS LONG
    FUNCTION = a + b
END FUNCTION

' Exported SUB with ALIAS
SUB DoNothing ALIAS "DoNothing" () EXPORT
    ' empty
END SUB

' Helper function (forward-declared above, no export)
FUNCTION Helper(BYVAL x AS LONG) AS LONG
    FUNCTION = x * 2
END FUNCTION

' Main entry point — test that local functions work correctly
FUNCTION PBMAIN() AS LONG
    LOCAL result AS LONG

    ' Test Add2 (exported function)
    result = Add2(10, 32)
    IF result <> 42 THEN FUNCTION = 1: EXIT FUNCTION

    ' Test Helper (forward-declared function)
    result = Helper(21)
    IF result <> 42 THEN FUNCTION = 2: EXIT FUNCTION

    ' Test Add2 with negative numbers
    result = Add2(-5, 47)
    IF result <> 42 THEN FUNCTION = 3: EXIT FUNCTION

    FUNCTION = 0  ' all tests passed
END FUNCTION
