<p align="center">
  <img src="assets/powerbasilisk-hero.png" width="400" alt="PowerBasilisk">
</p>

<h1 align="center">PowerBasilisk</h1>

<p align="center">
  <b>An open-source 64-bit PowerBASIC compiler written in Rust, targeting LLVM IR</b>
</p>

<p align="center">
  <a href="https://github.com/benstopics/powerbasilisk/actions/workflows/ci.yml"><img src="https://github.com/benstopics/powerbasilisk/actions/workflows/ci.yml/badge.svg" alt="CI"></a>
  <a href="https://github.com/benstopics/powerbasilisk/blob/main/LICENSE"><img src="https://img.shields.io/github/license/benstopics/powerbasilisk" alt="License"></a>
  <a href="https://www.rust-lang.org/"><img src="https://img.shields.io/badge/rust-1.75%2B-orange?logo=rust" alt="Rust"></a>
  <a href="https://llvm.org/"><img src="https://img.shields.io/badge/LLVM-17%2B-blue?logo=llvm" alt="LLVM"></a>
  <a href="https://github.com/benstopics/powerbasilisk"><img src="https://img.shields.io/github/stars/benstopics/powerbasilisk?style=social" alt="Stars"></a>
</p>

---

PowerBasilisk compiles [PowerBASIC](https://en.wikipedia.org/wiki/PowerBASIC) 9.x source code to **LLVM IR**, then uses `clang` to produce native executables, DLLs, or object files. It is written entirely in Rust with **zero external crate dependencies** for the core frontend.

The compiler generates LLVM IR as **plain text** (no `inkwell` or `llvm-sys` bindings required), making it easy to build and inspect the output at every stage. It also includes `pbinterp`, a Rust-based PowerBASIC interpreter for running unit tests without compiling.

## Features

| Feature | Status |
|---------|--------|
| Full PowerBASIC 9.x syntax (SUB, FUNCTION, IF, FOR, DO, SELECT CASE) | Done |
| Variables & arithmetic (LONG, INTEGER, DOUBLE, SINGLE, QUAD, BYTE, WORD) | Done |
| Arrays (1D, 2D, constant bounds, GLOBAL, LOCAL) | Done |
| Strings (dynamic STRING, STRING\*N fixed, 16+ builtins) | Done |
| User-defined TYPEs (structs, nested field access, arrays of TYPE) | Done |
| DLL export/import (DECLARE LIB, ALIAS, EXPORT, --dll mode) | Done |
| GOSUB/RETURN, GOTO, labels (numeric and named) | Done |
| Session struct mode (`--session-struct` wraps all globals) | Done |
| VARPTR, pointer operations | Done |
| File I/O (OPEN, CLOSE, PRINT#, LINE INPUT#, INPUT#, EOF) | Done |
| Preprocessor (#INCLUDE, #IF, %CONSTANTS) | Done |
| Runtime library (FORMAT$, PARSE$, REMOVE$, REPLACE, USING$, DATE$, TIME$) | Done |
| Debug mode (`--debug` for function tracing and crash reporting) | Done |
| 32-bit PE output (x86 stdcall for Win32 API interop) | Done |
| Interpreter (`pbinterp`) for running PB code without compiling | Done |

## Quick Start

### Prerequisites

- **Rust** 1.75+ (for building the compiler and interpreter)
- **LLVM/Clang** 17+ (for compiling generated IR to native code)
  - Install via `winget install LLVM.LLVM` on Windows

### Build

```bash
git clone https://github.com/benstopics/powerbasilisk.git
cd powerbasilisk
cargo build --release
```

This builds three binaries:
- `target/release/pbcompiler` — the compiler (PB source → LLVM IR → native)
- `target/release/pbinterp` — the interpreter (PB source → direct execution)

### Compile a PowerBASIC program

```bash
# Compile to object file
./target/release/pbcompiler build hello.bas -o hello

# Compile to executable
./target/release/pbcompiler build hello.bas -o hello --exe \
  --runtime-lib pbcompiler/runtime/pb_runtime.obj

# Compile to DLL
./target/release/pbcompiler build mylib.bas -o mylib --dll

# Emit LLVM IR only (for inspection)
./target/release/pbcompiler build hello.bas -o hello --emit-llvm
```

### Interpret a PowerBASIC program

```bash
# Run a PB program directly (no compilation needed)
./target/release/pbinterp run hello.bas

# Run with timeout
./target/release/pbinterp run tests.bas --timeout 120

# Dump preprocessed output
./target/release/pbinterp dump hello.bas
```

### Run the compiler test suite

```bash
# Run all compiler level tests (L0-L12)
cargo build --release
for f in pbcompiler/tests/l*.bas; do
  echo "=== $f ==="
  ./target/release/pbcompiler build "$f" -o "${f%.bas}" --exe \
    --runtime-lib pbcompiler/runtime/pb_runtime.obj
  ./"${f%.bas}.exe"
  echo "Exit: $?"
done
```

## Architecture

```
                    PowerBasilisk Compiler Pipeline
 ┌──────────┐    ┌───────┐    ┌───────┐    ┌──────────┐    ┌──────────┐
 │ PB Source │───>│ Prepr │───>│  Lex  │───>│  Parse   │───>│ Codegen  │
 │  (.bas)   │    │ ocess │    │       │    │  (AST)   │    │(LLVM IR) │
 └──────────┘    └───────┘    └───────┘    └──────────┘    └────┬─────┘
                      │                          │              │
                      │      ┌──────────┐        │              v
                      └─────>│ pbinterp │<───────┘       ┌──────────┐
                             │  (AST    │                │ .ll file │
                             │ interp.) │                └────┬─────┘
                             └──────────┘                     │ clang
                                                              v
                                                    ┌─────────────────┐
                                                    │ .obj / .exe /   │
                                                    │      .dll       │
                                                    └─────────────────┘
```

### Crate Structure

| Crate | Purpose |
|-------|---------|
| **`pb`** | Shared frontend: lexer, parser, AST, preprocessor. Zero dependencies. |
| **`pbcompiler`** | LLVM IR code generation, linking, CLI driver. Depends on `pb`. |
| **`pbinterp`** | AST interpreter for running PB code directly. Depends on `pb`. |

### Key Types

| Type | Crate | File | Purpose |
|------|-------|------|---------|
| `IrType` | pbcompiler | `llvm_ir.rs` | LLVM IR types (I32, Double, Array, Ptr, etc.) |
| `Val` | pbcompiler | `llvm_ir.rs` | Named SSA value with type |
| `ModuleBuilder` | pbcompiler | `llvm_ir.rs` | Builds LLVM IR module (globals + functions) |
| `FunctionBuilder` | pbcompiler | `llvm_ir.rs` | Builds a single LLVM IR function body |
| `SymbolTable` | pbcompiler | `symbols.rs` | Tracks variables and arrays during codegen |
| `Compiler` | pbcompiler | `codegen.rs` | Main compilation driver |
| `Token` | pb | `token.rs` | Lexer token enum |
| `TopLevel` / `Statement` / `Expr` | pb | `ast.rs` | AST node types |

### Compilation Pipeline

| Phase | Input | Output | Time* |
|-------|-------|--------|-------|
| Preprocess | .bas + #INCLUDEs | Flattened source lines | ~0.0s |
| Lex | Source text | Token stream | ~0.1s |
| Parse | Tokens | AST (top-level items) | ~0.1s |
| Codegen | AST | LLVM IR (.ll) | ~0.3s |
| Compile | .ll | Object file (.obj) | ~30s |
| Link | .obj + libs | Executable/DLL | ~1s |

*\*Measured on an 89K-line PowerBASIC codebase producing 876K lines of LLVM IR, 3247 top-level items, 100% compile rate.*

## CLI Reference

### pbcompiler

```
pbcompiler build <file.bas> [options]

Options:
  -o <path>              Output path (default: input with .obj extension)
  --exe                  Link to standalone executable
  --dll                  Link to shared library (DLL)
  --session-struct       Wrap all globals into a single exported struct
  --emit-llvm            Emit .ll file only (skip clang compilation)
  --parse-only           Parse and exit (no codegen)
  --debug                Enable debug mode (function tracing, crash handler)
  --runtime-lib <path>   Path to pb_runtime.obj for linking
  --lib-dir <path>       Directory containing import libraries (.lib)
  --split-threshold <N>  Split functions exceeding N IR lines (default: off)
```

### pbinterp

```
pbinterp run <file.bas> [options]

Options:
  --timeout <seconds>    Execution timeout (default: none)
  --json <path>          Write test results as JSON
  --filter <pattern>     Only run test suites matching pattern

pbinterp dump <file.bas>
  Dump preprocessed source (after #INCLUDE resolution)
```

## Type Mapping

| PowerBASIC | LLVM IR | Notes |
|------------|---------|-------|
| `LONG`, `DWORD` | `i32` | |
| `INTEGER`, `WORD` | `i16` | |
| `BYTE` | `i8` | |
| `QUAD` | `i64` | |
| `DOUBLE`, `EXT`, `CUR` | `double` | |
| `SINGLE` | `float` | |
| `STRING` | `ptr` | Null-terminated C string (BSTR on Windows) |
| `STRING * N` | `[N x i8]` | Fixed-size buffer in struct fields |
| `ASCIIZ * N` | `[N x i8]` | Same as fixed string |
| `DIM arr(N)` | `[N+1 x T]` | 1D array flattened |
| `DIM arr(M, N)` | `[M*N x T]` | Multi-dim flattened with compile-time strides |
| `TYPE MyType` | `%MYTYPE = type { ... }` | Named LLVM struct |

## Implementation Details

### String Handling

- Strings are `ptr` (opaque pointer to null-terminated `char*`)
- String variables initialize to `@.str.empty` (global empty string constant)
- Concatenation: `malloc(len_a + len_b + 1)` + `strcpy` + `strcat`
- Comparison: `strcmp()` + `icmp` (supports `=`, `<>`, `<`, `>`, `<=`, `>=`)
- On Windows, strings are allocated as BSTRs via `SysAllocStringByteLen` for COM interop

**String Builtins (16+):**

| Builtin | Implementation |
|---------|---------------|
| `LEN` | `strlen` |
| `CHR$` | `malloc(2)`, store byte + null |
| `ASC` | `load i8`, `zext to i32` |
| `STR$` | `sprintf(buf, "%g", val)` |
| `VAL` | `atof(s)` |
| `LEFT$`, `RIGHT$`, `MID$` | `malloc` + `memcpy` with offset |
| `INSTR` | `strstr` → pointer diff + 1 |
| `UCASE$`, `LCASE$` | Loop calling `toupper`/`tolower` |
| `TRIM$`, `LTRIM$`, `RTRIM$` | Byte-level space skipping |
| `SPACE$`, `STRING$` | Loop filling chars |

### Math Builtins

| Builtin | Implementation |
|---------|---------------|
| `ABS` | `llvm.fabs.f64` (float), `select` (int) |
| `SGN` | Compare chain → `select` |
| `MIN`, `MAX` | Compare → `select` (type-aware) |
| `INT` | `llvm.floor.f64` → `fptosi` |
| `FIX` | `fptosi` (truncation toward zero) |
| `SQR` | `llvm.sqrt.f64` |
| `LOG`, `EXP` | `llvm.log.f64`, `llvm.exp.f64` |
| `SIN`, `COS` | `llvm.sin.f64`, `llvm.cos.f64` |
| `TAN`, `ATN` | C library `tan()`, `atan()` |
| `ROUND` | `llvm.round.f64` + `llvm.pow.f64` for decimal places |
| `RND` | C library `rand()` with modulo |
| `CINT`, `CLNG`, `CDBL`, `CSNG` | Type conversion instructions |

### Array Implementation

- Arrays are flattened to 1D in LLVM IR: `DIM C(3,4)` → `[20 x i32]`
- Multi-dimensional indexing uses compile-time strides: `flat = (row - lower0) * count1 + (col - lower1)`
- Global arrays: `@NAME = global [N x T] zeroinitializer`
- Local arrays: `%r = alloca [N x T]` + `store zeroinitializer`
- Element access: `getelementptr inbounds [N x T], ptr %base, i64 0, i64 %flat_idx`
- Only constant bounds supported (no REDIM)

### TYPE (Struct) Implementation

- TYPEs compile to named LLVM struct types: `%MYTYPE = type { i32, double, i32 }`
- Field access via GEP: `getelementptr inbounds %MYTYPE, ptr %base, i32 0, i32 <field_idx>`
- Local TYPE variable: `alloca %MYTYPE` + `store zeroinitializer`
- Global TYPE variable: `@NAME = global %MYTYPE zeroinitializer`
- Array of TYPE: `[N x %MYTYPE]` — element GEP then field GEP
- BYREF passing: pointer to struct passed directly (PB default for UDTs)
- FixedString (`STRING*N`) TYPE members: compiled as `[N x i8]` in struct, read returns buffer address directly (no load)

### DLL Export/Import

- `DECLARE FUNCTION Foo LIB "BAR.DLL" ALIAS "Foo" (params) AS type` → `declare dllimport i32 @Foo(...)`
- `FUNCTION Add2 ALIAS "Add2" (...) EXPORT` → `define dllexport i32 @Add2(...)`
- Calling conventions: CDECL/STDCALL/BDECL identifiers parsed but mapped to platform ABI (x64 single convention, x86 stdcall for Win32 APIs)
- `--dll` flag: `clang -shared obj -o output.dll`

### GOSUB/RETURN Implementation

- Each function with GOSUBs gets a `gosub_ret_addr` local variable (`alloca i32`)
- `GOSUB label` → store unique return ID, branch to label's block
- `RETURN` → branch to `gosub.dispatch` block
- `gosub.dispatch` → `switch i32 %ret_addr` dispatching to each GOSUB's return-point block
- `GOTO label` → unconditional branch
- Numeric labels supported (e.g., `30300`)

### Runtime Library (pb_runtime.c)

Compiled with `clang -c` and linked with the program:

| Runtime Function | PB Equivalent | Description |
|-----------------|---------------|-------------|
| `pb_format` | `FORMAT$` | Picture formatting (#, 0, comma, dollar, percent) |
| `pb_parse` | `PARSE$` | 1-based field extraction; index=0 returns count |
| `pb_parsecount` | `PARSECOUNT` | Count delimited fields |
| `pb_replace` | `REPLACE` | In-place string replacement |
| `pb_remove` | `REMOVE$` | Remove characters from set |
| `pb_using` | `USING$` | PRINT USING-style formatting |
| `pb_environ` | `ENVIRON$` | Read environment variable |
| `pb_exe_path` | `EXE.PATH$` | Directory of current executable |
| `pb_exe_name` | `EXE.NAME$` | Filename of current executable |
| `pb_date` | `DATE$` | Current date (MM-DD-YYYY) |
| `pb_time` | `TIME$` | Current time (HH:MM:SS) |
| `pb_freefile` | `FREEFILE` | Next available file handle |
| `pb_open` / `pb_close` | `OPEN` / `CLOSE` | File I/O |
| `pb_bstr_alloc` / `pb_bstr_free` | (internal) | BSTR memory management |

## Debug Mode

Compile with `--debug` to enable runtime diagnostics:

```bash
pbcompiler build program.bas -o program --exe --debug \
  --runtime-lib pbcompiler/runtime/pb_runtime.obj

# Enable verbose function-level logging
PB_DEBUG_VERBOSE=1 ./program.exe
```

### Debug Features

| Feature | Always On | `--debug` Only |
|---------|-----------|----------------|
| Function entry tracing (`pb_debug_enter`) | Yes | Yes |
| Source line tracking (`pb_debug_line`) | Yes | Yes |
| Crash handler (vectored exception) | Yes | Yes |
| Modal text logging (`pb_debug_modal`) | No | Yes |
| Verbose logging to `pb_debug.log` | Via `PB_DEBUG_VERBOSE=1` | Via `PB_DEBUG_VERBOSE=1` |

### Log Files

| Log File | Contents |
|----------|----------|
| `pb_debug.log` | Function entry trace, crash info, modal text |
| `pb_crash.log` | Last crash details (function, line, exception code) |

## Session Struct Mode

The `--session-struct` flag wraps all global variables into a single LLVM struct, exported via `GetSession()`. This enables FFI consumers (C, C++, etc.) to access all program state through a single typed pointer:

```llvm
%PBSESSIONDATA = type { i32, double, [10 x i32], ... }
@pb.session = dllexport global %PBSESSIONDATA zeroinitializer

define dllexport ptr @GetSession() {
  ret ptr @pb.session
}
```

Global access uses constant GEP expressions:
```llvm
; Read a scalar global (field index 0):
%val = load i32, ptr getelementptr inbounds (%PBSESSIONDATA, ptr @pb.session, i32 0, i32 0)

; Access an array element (field index 3):
%elem = getelementptr inbounds [10 x i32],
  ptr getelementptr inbounds (%PBSESSIONDATA, ptr @pb.session, i32 0, i32 3),
  i64 0, i64 %idx
```

Session mode handles a common PB pattern where `GLOBAL arr()` is at top level but `DIM arr(N)` is inside a function body (e.g., `SetupDims()`). The compiler scans all function bodies to resolve pending global array dimensions.

## Compiler Test Levels

| Level | Feature | Test File |
|-------|---------|-----------|
| L0 | PBMAIN, exit code, printf | `l0_minimal.bas` |
| L1 | Variables, arithmetic, IF/ELSE | `l1_arithmetic.bas` |
| L2 | FOR, DO/LOOP, WHILE/WEND, SELECT CASE | `l2_loops.bas` |
| L3 | SUB, FUNCTION, BYVAL, BYREF, recursion | `l3_functions.bas` |
| L4 | Arrays (1D, 2D, local, global, constant bounds) | `l4_arrays.bas` |
| L5 | Builtins (ABS, SGN, MIN, MAX, math, RND, ROUND) | `l5_builtins.bas` |
| L6 | Strings (STRING type, concat, comparison, 16 builtins) | `l6_strings.bas` |
| L7 | User-defined TYPEs (structs) | `l7_types.bas` |
| L8 | DLL export/import, DECLARE LIB, ALIAS, EXPORT | `l8_dll_export.bas` |
| L9 | GOSUB/RETURN, GOTO, FORMAT$, PARSE$, REMOVE$ | `l9_gosub.bas` |
| L9b | Additional builtins | `l9_builtins2.bas` |
| L10 | Session struct (`--session-struct`) | `l10_session.bas` |
| L11 | VARPTR, pointer operations | `l11_varptr.bas` |
| L12 | File I/O (OPEN, CLOSE, PRINT#, LINE INPUT#, INPUT#) | `l12_fileio.bas` |

Each test returns 0 on success, non-zero on failure (unique error code per test case).

## Contributing

Contributions are welcome! Please open an issue or pull request.

### Development

```bash
# Build in debug mode (faster compilation)
cargo build

# Run clippy
cargo clippy --all-targets

# Format code
cargo fmt --all
```

## License

[MIT](LICENSE)
