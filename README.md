<p align="center">
  <img src="assets/powerbasilisk-hero.png" width="400" alt="PowerBasilisk">
</p>

<h1 align="center">PowerBasilisk</h1>

<p align="center">
  <b>An open-source 64-bit PowerBASIC compiler written in Rust, targeting LLVM IR</b>
</p>

<p align="center">
  <a href="https://github.com/benstopics/powerbasilisk/actions/workflows/ci.yml"><img src="https://github.com/benstopics/powerbasilisk/actions/workflows/ci.yml/badge.svg" alt="CI"></a>
  <a href="https://github.com/benstopics/powerbasilisk/blob/main/LICENSE"><img src="https://img.shields.io/badge/license-Apache--2.0-blue" alt="License"></a>
  <a href="https://www.rust-lang.org/"><img src="https://img.shields.io/badge/rust-1.75%2B-orange?logo=rust" alt="Rust"></a>
  <a href="https://llvm.org/"><img src="https://img.shields.io/badge/LLVM-17%2B-blue?logo=llvm" alt="LLVM"></a>
  <a href="https://github.com/benstopics/powerbasilisk"><img src="https://img.shields.io/github/stars/benstopics/powerbasilisk?style=social" alt="Stars"></a>
</p>

---

PowerBasilisk compiles [PowerBASIC](https://en.wikipedia.org/wiki/PowerBASIC) 9.x source code to **LLVM IR**, then uses `clang` to produce native executables, DLLs, or object files. It is written entirely in Rust with **zero external crate dependencies** for the core frontend.

The compiler generates LLVM IR as **plain text** (no `inkwell` or `llvm-sys` bindings required), making it easy to build and inspect the output at every stage. It also includes `pbinterp`, a Rust-based PowerBASIC interpreter for running unit tests without compiling.

## Architecture

```
                    PowerBasilisk Compiler Pipeline
 ┌───────────┐    ┌───────┐    ┌───────┐    ┌──────────┐    ┌──────────┐
 │ PB Source │───>│ Prepr │───>│  Lex  │───>│  Parse   │───>│ Codegen  │
 │  (.bas)   │    │ ocess │    │       │    │  (AST)   │    │(LLVM IR) │
 └───────────┘    └───────┘    └───────┘    └──────────┘    └───┬──────┘
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

| Crate | Purpose | Details |
|-------|---------|---------|
| [**`pb`**](pb/) | Shared frontend: lexer, parser, AST, preprocessor. Zero dependencies. | [README](pb/README.md) |
| [**`pbcompiler`**](pbcompiler/) | LLVM IR code generation, linking, CLI driver. Depends on `pb`. | [README](pbcompiler/README.md) |
| [**`pbinterp`**](pbinterp/) | AST interpreter for running PB code directly. Depends on `pb`. | [README](pbinterp/README.md) |

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

This builds two binaries:
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

[Apache-2.0](LICENSE)
