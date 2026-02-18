' L6: String support (literals, concat, comparison, builtins)

FUNCTION GetHello() AS STRING
  FUNCTION = "Hello"
END FUNCTION

SUB AppendWorld(BYVAL s AS STRING, result AS STRING)
  result = s & " World"
END SUB

FUNCTION PBMAIN() AS LONG
  LOCAL s AS STRING
  LOCAL t AS STRING
  LOCAL n AS LONG

  ' Test 1: String literal assignment
  s = "Hello"
  IF s <> "Hello" THEN
    FUNCTION = 1
    EXIT FUNCTION
  END IF

  ' Test 2: String equality
  t = "Hello"
  IF s <> t THEN
    FUNCTION = 2
    EXIT FUNCTION
  END IF

  ' Test 3: String inequality
  t = "World"
  IF s = t THEN
    FUNCTION = 3
    EXIT FUNCTION
  END IF

  ' Test 4: String concatenation with &
  s = "Hello" & " " & "World"
  IF s <> "Hello World" THEN
    FUNCTION = 4
    EXIT FUNCTION
  END IF

  ' Test 5: LEN
  IF LEN("Hello") <> 5 THEN
    FUNCTION = 5
    EXIT FUNCTION
  END IF

  ' Test 6: LEN of empty string
  IF LEN("") <> 0 THEN
    FUNCTION = 6
    EXIT FUNCTION
  END IF

  ' Test 7: LEFT$
  s = LEFT$("Hello World", 5)
  IF s <> "Hello" THEN
    FUNCTION = 7
    EXIT FUNCTION
  END IF

  ' Test 8: RIGHT$
  s = RIGHT$("Hello World", 5)
  IF s <> "World" THEN
    FUNCTION = 8
    EXIT FUNCTION
  END IF

  ' Test 9: MID$ with 3 args
  s = MID$("Hello World", 7, 5)
  IF s <> "World" THEN
    FUNCTION = 9
    EXIT FUNCTION
  END IF

  ' Test 10: MID$ with 2 args (rest of string)
  s = MID$("Hello World", 7)
  IF s <> "World" THEN
    FUNCTION = 10
    EXIT FUNCTION
  END IF

  ' Test 11: INSTR (found)
  n = INSTR("Hello World", "World")
  IF n <> 7 THEN
    FUNCTION = 11
    EXIT FUNCTION
  END IF

  ' Test 12: INSTR (not found)
  n = INSTR("Hello World", "xyz")
  IF n <> 0 THEN
    FUNCTION = 12
    EXIT FUNCTION
  END IF

  ' Test 13: CHR$ and ASC
  s = CHR$(65)
  IF s <> "A" THEN
    FUNCTION = 13
    EXIT FUNCTION
  END IF

  ' Test 14: ASC
  n = ASC("A")
  IF n <> 65 THEN
    FUNCTION = 14
    EXIT FUNCTION
  END IF

  ' Test 15: STR$
  s = STR$(42)
  IF VAL(s) <> 42 THEN
    FUNCTION = 15
    EXIT FUNCTION
  END IF

  ' Test 16: VAL
  LOCAL d AS DOUBLE
  d = VAL("3.14")
  IF d < 3.13 THEN
    FUNCTION = 16
    EXIT FUNCTION
  END IF
  IF d > 3.15 THEN
    FUNCTION = 17
    EXIT FUNCTION
  END IF

  ' Test 18: UCASE$
  s = UCASE$("hello")
  IF s <> "HELLO" THEN
    FUNCTION = 18
    EXIT FUNCTION
  END IF

  ' Test 19: LCASE$
  s = LCASE$("HELLO")
  IF s <> "hello" THEN
    FUNCTION = 19
    EXIT FUNCTION
  END IF

  ' Test 20: LTRIM$
  s = LTRIM$("  Hello")
  IF s <> "Hello" THEN
    FUNCTION = 20
    EXIT FUNCTION
  END IF

  ' Test 21: RTRIM$
  s = RTRIM$("Hello  ")
  IF s <> "Hello" THEN
    FUNCTION = 21
    EXIT FUNCTION
  END IF

  ' Test 22: TRIM$
  s = TRIM$("  Hello  ")
  IF s <> "Hello" THEN
    FUNCTION = 22
    EXIT FUNCTION
  END IF

  ' Test 23: String comparison < (lexicographic)
  IF "Apple" > "Banana" THEN
    FUNCTION = 23
    EXIT FUNCTION
  END IF

  ' Test 24: String comparison >
  IF "Banana" < "Apple" THEN
    FUNCTION = 24
    EXIT FUNCTION
  END IF

  ' Test 25: SPACE$
  s = SPACE$(3)
  IF LEN(s) <> 3 THEN
    FUNCTION = 25
    EXIT FUNCTION
  END IF
  IF s <> "   " THEN
    FUNCTION = 26
    EXIT FUNCTION
  END IF

  ' Test 27: STRING$
  s = STRING$(4, 65)
  IF s <> "AAAA" THEN
    FUNCTION = 27
    EXIT FUNCTION
  END IF

  ' Test 28: Empty string handling
  s = ""
  IF LEN(s) <> 0 THEN
    FUNCTION = 28
    EXIT FUNCTION
  END IF

  ' Test 29: Concat with empty
  s = "Hello" & ""
  IF s <> "Hello" THEN
    FUNCTION = 29
    EXIT FUNCTION
  END IF

  ' Test 30: String function return
  s = GetHello()
  IF s <> "Hello" THEN
    FUNCTION = 30
    EXIT FUNCTION
  END IF

  ' Test 31: String parameter passing (BYREF)
  LOCAL r AS STRING
  CALL AppendWorld("Hi", r)
  IF r <> "Hi World" THEN
    FUNCTION = 31
    EXIT FUNCTION
  END IF

  ' Test 32: MID$ at start
  s = MID$("ABCDEF", 1, 3)
  IF s <> "ABC" THEN
    FUNCTION = 32
    EXIT FUNCTION
  END IF

  ' Test 33: Chained concat
  s = "A" & "B" & "C" & "D"
  IF s <> "ABCD" THEN
    FUNCTION = 33
    EXIT FUNCTION
  END IF

  ' Test 34: INSTR with start position
  n = INSTR(5, "Hello Hello", "Hello")
  IF n <> 7 THEN
    FUNCTION = 34
    EXIT FUNCTION
  END IF

  FUNCTION = 0
END FUNCTION
