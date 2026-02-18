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

## Why PowerBasilisk Exists

### PowerBASIC

In the mid-1980s, Bob Zale created a fast native-code BASIC compiler called BASIC/Z, later published by Borland as Turbo Basic. When Borland dropped it, Zale bought it back and founded PowerBASIC Inc. in 1989. It became a solid niche compiler for DOS and Windows with an active community and forums. There were real efforts toward a 64-bit compiler and cross-platform support.

Zale died in 2012. His widow Vivian ran things until 2017, when **Drake Software** bought the source code and assets, saying they'd continue development. Since then: nothing. No releases, no 64-bit support, website gone, and the forums shut down without notice. Decades of accumulated knowledge from thousands of developers, just gone overnight. The compiler has been stuck at 32-bit Windows-only ever since.

People built real things on PowerBASIC. Chris Boss ([EZGUIProguy](https://twitter.com/ezguiproguy)) spent years building EZGUI, a full Win32 GUI framework in pure PowerBASIC, basically his own WinForms from scratch. It still runs on modern Windows. But the community around it is gone.

### Wall Street Raider

Michael Jenkins started sketching out a corporate finance board game while at Harvard Law in 1967. He wanted something like Monopoly but with real mechanics: stock issuance, mergers, leveraged buyouts, tax accounting. It needed a computer to work, and personal computers didn't exist yet.

In 1983 he got a Kaypro, taught himself BASIC from a booklet Bill Gates wrote, and started building. He retired from law and CPA practice at 42 to work on it full-time. Over the next 40 years, working alone, he wrote [Wall Street Raider](https://www.wallstreetraider.com): 180,000 lines of PowerBASIC covering 1,600 companies, stocks, bonds, options, futures, swaps, derivatives, ETFs, antitrust, consolidated tax accounting based on actual IRS rules, and a 271-page manual. He'd code through the night because he was afraid if he went to bed he'd lose the thread of whatever financial logic he was trying to get right.

Multiple teams tried to port or rewrite it over the years. A Denver legal software company. A game studio that did work for Disney, with a team in Armenia and a math PhD, spending over a year and hundreds of thousands of dollars. Commodore Computers looked at it for three months and mailed the source code back. They all hit the same wall which was rewriting the code requires understanding corporate finance, tax law, and securities regulation at the level of someone who actually practiced those things for decades.

In 2024, [Ben Ward](https://github.com/benstopics) spent a year reading the PowerBASIC manual, forum archives, and Jenkins' source code, and figured out the approach that actually works: don't rewrite the engine, wrap it. He built a modern Electron/Preact UI that talks to Jenkins' untouched PowerBASIC engine through a C++ REST bridge. The [remaster is on Steam](https://store.steampowered.com/app/3525620/Wall_Street_Raider/).

### Why a new compiler

Wall Street Raider still compiles with the original PowerBASIC 9.80 compiler. It is 32-bit, Windows-only, and no longer maintained or sold, thanks to Drake Software/Cinven. The game couldn't go cross-platform or 64-bit while it depended on a dead compiler, and rewriting 180,000 lines of financial logic in another language is the approach that already failed multiple times.

So PowerBasilisk is a clean-room PowerBASIC-compatible compiler in Rust targeting LLVM IR. It reads the same `.bas` and `.inc` files, parses the same syntax, and produces native 32-bit or 64-bit executables and DLLs through LLVM/Clang. It doesn't derive from the original PowerBASIC source. It also includes `pbinterp`, an interpreter that runs PB code directly from the AST for test-driven development against the original source.

This isn't only about Wall Street Raider. There are PowerBASIC developers out there with codebases that work fine but are stuck on a compiler that will never be updated. PowerBasilisk gives them 64-bit, cross-platform, and open-source without having to rewrite anything.

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

### Option 1: Download prebuilt binaries (recommended)

Grab the latest release from [GitHub Releases](https://github.com/benstopics/powerbasilisk/releases). Prebuilt binaries are available for:

- **Windows** (x86_64) — `pbcompiler.exe`, `pbinterp.exe`
- **macOS** (x86_64, Apple Silicon) — `pbcompiler`, `pbinterp`
- **Linux** (x86_64, aarch64) — `pbcompiler`, `pbinterp`

Extract the archive and add the directory to your `PATH`, or run the binaries directly.

### Option 2: Build from source

Requires **Rust** 1.75+:

```bash
git clone https://github.com/benstopics/powerbasilisk.git
cd powerbasilisk
cargo build --release
```

This builds two binaries in `target/release/`:
- `pbcompiler` — the compiler (PB source → LLVM IR → native)
- `pbinterp` — the interpreter (PB source → direct execution)

### Prerequisites for compilation

**LLVM/Clang** 17+ is required for `pbcompiler` to compile generated IR to native code:
- Windows: `winget install LLVM.LLVM`
- macOS: `brew install llvm`
- Linux: `apt install clang llvm` or equivalent

`pbinterp` does **not** require LLVM — it interprets PB code directly from the AST.

### Compile a PowerBASIC program

```bash
# Compile to object file (32-bit, the default)
pbcompiler build hello.bas -o hello

# Compile to executable
pbcompiler build hello.bas -o hello --exe \
  --runtime-lib pbcompiler/runtime/pb_runtime.obj

# Compile to DLL
pbcompiler build mylib.bas -o mylib --dll

# Emit LLVM IR only (for inspection)
pbcompiler build hello.bas -o hello --emit-llvm
```

### Targeting 32-bit vs 64-bit

The compiler defaults to 32-bit (`i686-pc-windows-msvc`) for compatibility with legacy PowerBASIC code. Use `--target` to select a different architecture:

```bash
# 32-bit (default) — compatible with original PowerBASIC binaries
pbcompiler build hello.bas -o hello --exe \
  --runtime-lib pbcompiler/runtime/pb_runtime.obj

# 64-bit
pbcompiler build hello.bas -o hello --exe \
  --target x86_64-pc-windows-msvc \
  --runtime-lib pbcompiler/runtime/pb_runtime_x64.obj
```

The runtime library must be compiled for the same target:

```bash
# Build 32-bit runtime (default)
clang -c --target=i686-pc-windows-msvc pbcompiler/runtime/pb_runtime.c \
  -o pbcompiler/runtime/pb_runtime.obj

# Build 64-bit runtime
clang -c --target=x86_64-pc-windows-msvc pbcompiler/runtime/pb_runtime.c \
  -o pbcompiler/runtime/pb_runtime_x64.obj
```

Both the compiler output and the runtime must use the same target triple. Mixing 32-bit and 64-bit objects produces linker error 1112 (machine type mismatch).

### Interpret a PowerBASIC program

```bash
# Run a PB program directly (no compilation needed)
pbinterp run hello.bas

# Run with timeout
pbinterp run tests.bas --timeout 120

# Dump preprocessed output
pbinterp dump hello.bas
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
  --target <triple>      LLVM target triple (default: i686-pc-windows-msvc)
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

Contributions are welcome! There are two main ways to help:

### Submit code changes

Open an issue or pull request on GitHub. For development setup:

```bash
# Build in debug mode (faster compilation)
cargo build

# Run clippy
cargo clippy --all-targets

# Format code
cargo fmt --all
```

### Share your PowerBASIC source code

If you have a PowerBASIC codebase you'd like to compile with PowerBasilisk, send your source to **Ben Ward** at [benstopics@gmail.com](mailto:benstopics@gmail.com). He will:

- Use your code as a **litmus test** for compiler compatibility
- Write tests based on your program's expected behavior
- Troubleshoot any compilation or runtime issues for you
- Report back what works and what still needs compiler support

This is one of the most valuable ways to contribute — real-world PowerBASIC code exposes edge cases and missing features that synthetic tests don't catch. Your code helps make PowerBasilisk work for everyone.

## License

[Apache-2.0](LICENSE)
