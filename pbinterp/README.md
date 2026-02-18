# pbinterp

A Rust-based PowerBASIC interpreter that executes PB code directly from the AST without compiling to native code. Primarily used for running unit tests against PowerBASIC game logic.

## Usage

```bash
# Run a PB program directly
pbinterp run hello.bas

# Run with timeout
pbinterp run tests.bas --timeout 120

# Write test results as JSON
pbinterp run tests.bas --json results.json

# Only run matching test suites
pbinterp run tests.bas --filter "transact"

# Dump preprocessed source (after #INCLUDE resolution)
pbinterp dump hello.bas
```

## How It Works

`pbinterp` shares the same frontend as `pbcompiler` (lexer, parser, AST from the `pb` crate), but instead of generating LLVM IR, it walks the AST and interprets each statement directly in Rust.

This means:
- No LLVM/Clang dependency needed
- Instant startup (no compile step)
- Ideal for running hundreds of unit tests in seconds

## Supported Features

The interpreter supports the full PowerBASIC 9.x subset needed for unit testing:

- Variables (LONG, INTEGER, DOUBLE, SINGLE, QUAD, BYTE, WORD, STRING)
- Arithmetic and comparison operators
- Control flow (IF/ELSEIF/ELSE, FOR/NEXT, DO/LOOP, WHILE/WEND, SELECT CASE)
- SUB and FUNCTION (BYVAL, BYREF)
- Arrays (1D, 2D, GLOBAL, LOCAL)
- User-defined TYPEs (structs, nested field access)
- String builtins (LEN, MID$, LEFT$, RIGHT$, INSTR, UCASE$, LCASE$, TRIM$, etc.)
- Math builtins (ABS, SGN, INT, FIX, SQR, MIN, MAX, ROUND, RND, etc.)
- GOSUB/RETURN, GOTO
- #INCLUDE, %CONSTANTS, preprocessor directives
- PRINT (to stdout)
- VARPTR (limited)

## Test Runner Integration

`pbinterp` is designed to work with the PowerBASIC test framework. Test files use `SUB` blocks with assertion macros:

```basic
SUB TestSomething()
  LOCAL result AS LONG
  result = MyFunction(42)
  AssertEqual result, 100, "MyFunction should return 100 for input 42"
END SUB
```

The interpreter detects test assertion failures and reports them with file/line context. Use `--json` to get machine-readable results for CI integration.
