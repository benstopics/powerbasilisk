' L12: File I/O — OPEN, CLOSE, PRINT#, LINE INPUT#, EOF, FREEFILE, KILL
FUNCTION PBMAIN() AS LONG
  LOCAL f AS LONG
  LOCAL s AS STRING

  ' Get a free file number
  f = FREEFILE
  IF f < 1 THEN FUNCTION = 1: EXIT FUNCTION

  ' Write a test file
  OPEN "l12_test.tmp" FOR OUTPUT AS #f
  PRINT #f, "Hello World"
  PRINT #f, "Second Line"
  PRINT #f, "Third Line"
  CLOSE #f

  ' Read it back with LINE INPUT
  f = FREEFILE
  OPEN "l12_test.tmp" FOR INPUT AS #f

  LINE INPUT #f, s
  IF s <> "Hello World" THEN FUNCTION = 2: EXIT FUNCTION

  LINE INPUT #f, s
  IF s <> "Second Line" THEN FUNCTION = 3: EXIT FUNCTION

  LINE INPUT #f, s
  IF s <> "Third Line" THEN FUNCTION = 4: EXIT FUNCTION

  ' Should be at EOF
  IF EOF(f) = 0 THEN FUNCTION = 5: EXIT FUNCTION

  CLOSE #f

  ' Clean up
  KILL "l12_test.tmp"

  FUNCTION = 0  ' All tests passed
END FUNCTION
