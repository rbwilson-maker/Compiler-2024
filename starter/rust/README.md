This code was tested against Stable Rust (1.75.0). It has some dependencies for lexing, parsing,
and some quality of life features. You are free to extend it with other crates as you see fit.

You may want to use rust ecosystem tools like
[Clippy](https://github.com/rust-lang/rust-clippy) and
[rustfmt](https://github.com/rust-lang/rustfmt) to automatically keep your code
clean. This helps immensely when collaborating with a partner on a large
project. You can run `make check` to run these tools.

### Compiler Structure

#### Driver

- **Main** (`main.rs`): Spawns a new child thread and runs the compile cycle,
  returning appropriate error codes.
- **Args** (`args.rs`): Parses command line arguments.

#### Passes

- **Lexer** (`lex.rs`): Lexer generator relying on [logos](https://docs.rs/logos/0.12.1/logos/index.html)
  Uses regular expressions to generate a fast lexer
  `token()` is called. Returns `Err(Error::EOF)` when done.
- **Parser** (`parse.rs`): Adds utilites to interact with the lexer and parser. The parser grammar
  is defined in `c0.lalrpop` and [lalrpop](https://github.com/lalrpop/lalrpop) to generate an LR(1) parser
- **AST** (`ast.rs`): Type definitions for an L1 Abstract Syntax Tree
  representation.
- **Type Checking** (`tc.rs`): Verify that the AST is a valid program that
  conforms to L1 static semantics, particularly regarding variable declaration
  and definition.
- **ASM** (`asm.rs`): Type definitions for bytecode-like abstract assembly
  syntax that uses 3-address operations and an infinite number of temporary
  virtual registers. This is a useful IR for optimization passes and SSA
  conversion.
- **Codegen** (`codegen.rs`): Translates the AST into flattened abstract
  assembly.
- **Emit** (`emit.rs`): Writes the abstract assembly to a target file. This may
  be useful as a basis for your x86 assembly emitter.

#### Helpers

- **Errors** (`error.rs`): Defines a basic error & result type so that we can
  make use of the `?` symbol for clean result handling (as opposed to dealing
  with options everywhere).
- **Checkpoint** (`checkpoint.rs`): Deserializes and serializes the checkpoint
  format and has a place to actually implement the checkpoint, which is run
  from main when the compiler is run in checkpoint mode.

### Other Information

We have included a script, `bin/c0c` that will run your compiler and then
actually create an executable. This can be helpful when debugging. You can
also compile and run a file using the `-x` flag.

If you use OS X, you will need to output slightly different assembly (error
codes and labels will be different). `bin/c0c` sets UNAME in the environment
to "Darwin" in these cases.
