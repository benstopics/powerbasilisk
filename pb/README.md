# pb

Shared frontend crate for PowerBasilisk. Contains the lexer, parser, AST definitions, and preprocessor used by both `pbcompiler` and `pbinterp`.

This crate has **zero external dependencies** — it is pure Rust with no third-party crates.

## Key Types

| Type | File | Purpose |
|------|------|---------|
| `Token` | `token.rs` | Lexer token enum (keywords, operators, literals, identifiers) |
| `TopLevel` | `ast.rs` | Top-level AST nodes (SUB, FUNCTION, TYPE, GLOBAL, DIM, etc.) |
| `Statement` | `ast.rs` | Statement-level AST nodes (IF, FOR, DO, SELECT, assignments, calls) |
| `Expr` | `ast.rs` | Expression AST nodes (binary ops, function calls, array indexing, field access) |

## Pipeline

```
Source text (.bas)
       │
       ▼
┌──────────────┐
│ Preprocessor │  Resolves #INCLUDE, %CONSTANTS, #IF/#ELSE
└──────┬───────┘
       │  Flattened source lines
       ▼
┌──────────────┐
│    Lexer     │  Tokenizes source into Token stream
└──────┬───────┘
       │  Vec<Token>
       ▼
┌──────────────┐
│    Parser    │  Builds typed AST from token stream
└──────┬───────┘
       │  Vec<TopLevel>
       ▼
  pbcompiler        pbinterp
  (codegen)       (interpret)
```

## Usage

This crate is a library — it is not used directly. Import it from `pbcompiler` or `pbinterp`:

```rust
use pb::lexer::Lexer;
use pb::parser::Parser;
use pb::preprocessor::Preprocessor;
use pb::ast::{TopLevel, Statement, Expr};
```
