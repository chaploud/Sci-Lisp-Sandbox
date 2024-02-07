# `src` directory explanation

```toml
- src: toplevel
  - main.rs               # entry point
  - lib/                  # standard/preload libraries
    - rust/               # Rust InterOp
    - std/                # Sci-Lisp Standard library
  - cli/
    - repl.rs             # REPL
    - execute.rs          # Run a file
  - structures/           # Data structures
  - parser/               # Parse Source code to AST
  - analyzer/             # Analyze AST
  - compiler/             # Compile AST to Wasm IR
  - executor/             # Execute Wasm using wasmtime
```
