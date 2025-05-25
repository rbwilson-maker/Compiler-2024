This is a C0 compiler built by Nicole Fang and Rachel Wilson in 15-411: Compiler Design. Spring 2024 at Carnegie Mellon University.

C0 is a simplified version of C with some safety gurantees.

# Table of Contents

- [Lab6 Compiler Extension Project](#lab6-compiler-extension)
- [Compiler Overview](#compiler-overview)
    - [Optimizations](#optimizations)
- [Compiler Architecture](#compiler-architecture)
    - [Representations](#representations)
    - [Phases](#phases)
    - [Utils](#utils)
- [Development Best Practices](#development-best-practices)
    - [GitHub](#github)
    - [Useful Commands](#useful-commands)


# Lab 6 Compiler Extension Project

We implemented higher order functions in C0! We parse lambda functions and compile
them into normal functions. All functions, named or lambda, can be passed around as
values with a function type. They may be arbitrarily nested, but they may not use scoped variables (no closures yet). A lambda function application may look like:

`tests/lab6-tests/ret_direct_app.l4`
```
int main() {
  return fn (int x)->int {return x + 1;} (0);
}
```

### Implementation

We extended our lab5 code with constructors to represent function values. 
Elaboration (`lib/phases/p3-elaborate/`) takes care of potentially nested lambdas
by converting all lambdas into named functions, replacing them at their introduction site with effectively a function pointer, and adding them to a queue of functions to be elaborated.

Now that we have function values, they can be used in two ways: either a function value is directly the name of an existing function, or it's an expression that 
computes a function. This was tricky but important for us to distinguish early on, 
so we had the typechecker detect when a function was a named function in scope, 
and it would consider the function used.

At the lower level, there are two new constructs, which correspond to creating a function value, and using one.

1. The first is relative addressing of function labels. Once all lambdas have labels, this is pretty straightforward and is equivalent to `fn_label(%rip)` in assembly.
2. The second is `*%reg` in assembly, which allows you to treat the contents of a register as an address we can call or jump to. This required some reworking in phases 4 and 5 (translate and codegen).

### Testing

Testing uses the existing `Compiler-dist/gradecompiler` script to test a new test suite of sixteen tests in `Compiler/tests/lab6-tests/`. The tests in the test suite use the file extension `.l4` instead of `.l6` because our code builds on the lab4 C0 language, and `.l6` seems to be reserved for the C1 lab6 project.

In `Compiler/Makefile` we added a rule with allows for running our test suite.
```
test:
	../gradecompiler tests/lab6-tests -O1

# run on the command line `make test`
```
Some notes:
 - *Since our code employs the gradecompiler to link and run our generated `.s` files, we did not use the `--exe` flag. Although it can be passed, it does nothing.*
 - *All implemented `-O1` optimizations work for our extension! In the future,
   tail call optimizations could be updated to account for tail calls to lambdas.*
 - **We have not implemented closures.** Closures allow for non-trivial currying of functions, which we were unable to implement within the given time. That said, 
*what we do have allows for a wide variety of generalized higher order functions* which take functions as input, such as an uncurried fold function over a list type, or a list tabulate function. We implemented some such tests in `tests/lab6-tests/ret_basic_hofs.l4` and `tests/lab6-tests/ret_dropwhile.l4`

# Compiler Overview

This compiler converts C0 code to x86-64 assembly code.
The compiler translates between intermediate languages across phases, eventually implementing assembly instructions.

As of 18 April 2024, there is:
- context-sensitive parsing with the lexer hack
- pure expressions separated from statements in the ir tree
- 3-address abstract assembly
- calling conventions handled when elaborating assembly
- caller saves encoded via liveness 
- not-optimal register allocation
- assembly instructions parameterized by register size
- calling conventions in convert
- memory allocation with safety checks
- `-O1`optimizations!
- primitive lambda functions and function values

## Optimizations

This compiler implements some optimizations in order to increase the efficiency of the outputted assembly code at the cost of increased compile time.

The optimization level of the compiler can be given via the `-O` flag:
- `-O0` means that optimizations are not performed
- `-O1` indicates that all optimizations will be performed

*Note:* some optimizations involved restructuring the compiler code, specifically the IR tree, those are always run on `-O0`.

The following optimizations have been implemented:

| Optimization | Level | [Phase](#phases) | Description |
| ------------ |:-----:|:----------------:| ----------- |
| Control flow graph | O1 | 7 | The control flow graph provides the compiler with a framework for reasoning about basic blocks in the code, which makes other optimizations faster such as liveness analysis. |
| Liveness analysis | O1 | 7 | Liveness analysis involves analysing which temps are "in use" at any given line in the code. This uses the control flow graph and aids in register allocation. |
| Register allocation | O1 | 7 | Instead of spilling all temps onto the stack, temps are stored in registers when possible in order to perform fewer memory accesses at the lower levels of the memory hierarchy. We implement maximum cardinality search with a binary heap as our priority queue. This uses liveness analysis and the control flow graph. |
| Unsafe mode | n/a | n/a | Unsafe mode can be invoked via the `--unsafe` flag when compiling. This mode removes the dynamic safety checks built into the C0 language, such as checking for in-bound array accesses and valid bit shift arguments. Removal of these checks speeds up the runtime of the compiled code. |
| Mini constant folding | O1 | 4.5 | Pure expressions in the IR tree containing only constants can be evaluated statically by the compiler rather than at runtime, and some conditional branches can be reduced to jumps. |
| Reduce temps | O0 | 4 | In translation to the IR tree, reduce the number of temps generated by directly moving an effectful expression into a temp rather than separating out the effectful expression into an extra temp. |
| Peephole optimizations | O1 | 7, 9 | Remove self moves and address calculations in the abstract assembly, after other optimizations, and in the generated x86 afterwards. |
| Recursive Tail Call optimizations | O1 | 7 | Uses the Control Flow Graph to detect tail calls and replace them with jumps so that they don't grow the stack. |


# Compiler Architecture

```
--1--> ast 
       --2--> (typechecked)
       --3--> elab 
              --4--> ir 
              -4.5-> ir (optimized)
                     --5--> assem
                     --6--> assem (elaborated)
                     --7--> assem (optimized)
                            --8--> x86-64
                            --9--> x86-64 (peephole optimized)
```

`lib/top.ml` encodes these phases. Building the compiler creates `bin/c0c` which is an executable that may be run on C0 files. Use `bin/c0c --help` to see accepted command line arguments.


## Representations

**AST:** `lib/languages/ast.mli`

- The Abstract Syntax Tree currently supports unary and binary operations on ints.

**Elab** `lib/languages/elab.mli`

- The Elaborated Abstract Syntax Tree removes unary ops and for loops.

**IR:** `lib/languages/tree.mli`

- Flattens the ast and includes branches and jumps.
- Separate pure expressions from statements.

**Assem:** `lib/languages/assem.mli`

 - The Abstract Assembly uses 3-address moves and binary operations. Calls are a form of move
 - The elaborated abstract assembly handles elaborates register-specific instructions and function argument moves.
 This helps encode liveness for register allocation and decreases the number of instructions needed when converting to x86.
 - After register allocation, spilled temps are left as temps

**x86-64:** `lib/languages/x86.mli`
 - Represents 64-bit address assembly instructions

## Phases

**Phase 1:** `lib/phases/p1-parse/`

- Parse to the AST.

**Phase 2:** `lib/phases/p2-typecheck/`

- Typecheck the AST.
- Record type information at the expression level by replacing file markings with type markings.
- Calculate struct sizes and offsets.
- Replace typedef'd types with their lowest level definitions for convenient size calculations.

**Phase 3:** `lib/phases/p3-elaborate/`

- Elaborate the AST. Removes unops and all the nested statement types leftover from parsing.
- Ignores all typedefs and function declarations since those are guaranteed to be fine by the typechecker.
- Change names of user-defined funcitons to include `_c0_`.
- Remove file markings.

**Phase 4:** `lib/phases/p4-makeIRtree/`

- Translate the well-formed program to the IR Tree.
- Distinguish between effectful versus pure statements and expressions.
- Encode addresses and distinguish between safe and unsafe addressing.
- Encode when safety checks should be performed.

**Phase 4.5:** `lib/phases/p4.5-optimizeTree`

- Perform a limited version of constant folding on pure expressions containing only constants within the IR Tree.

**Phase 5:** `lib/phases/p5-selectinstr/`

- Perform instruction selection. Generates new temps only when expanding non-atomic expressions.
- When branching, it preserves comparison operators in the conditional, since those are needed to determine which assembly jump instruction is used.

**Phase 6:** `lib/phases/p6-elabassem/`

- Elaborate the 3-address abstract assembly to encode some of the quirks of assembly.
- Div and mod are elaborated to use `%rax` and `%rdx` appropriately
- Function calls are elaborated to move their first six arguments into registers and move its result from `%rax`

**Phase 7:** `lib/phases/p7-optimizeAssem/`

Optimize is a meta-phase which manages and runs the following optimizations on the abstract assembly:

1. Control flow graph: `lib/utils/data_structures/cfg.ml`
    
    - From 3-address assembly, generates a control flow graph for each function, which encodes information about:
      - Basic blocks and their successors
      - Lines and their uses, definitions, and successors
    - Includes information about the original 3-address assembly input which is relevant to future optimizations, such as liveness analysis.

2. Liveness analysis: `lib/phases/p7-optimizeAssem/optimizations/liveness.ml`

    - Uses the information from the control graph to generate a representation of the 3-address abstract assembly program which encodes the LiveIn and LiveOut sets of each line.
    - If at any point a line has more than 200 liveOut variables, liveness analysis exits early and indicates not to perform register allocation.

3. Register allocation: `lib/phases/p7-optimizeAssem/optimizations/regalloc.ml`

    - Uses the liveness information and the control flow graph to construct an interference graph which encodes whether or not any two temps are live at the same time.
    - Regalloc does not assign registers to spilled arguments nor to function calls.
    - Implements the maximum cardinality search algorithm using a binary heap priority queue to determine an ordering of vertices with which to greedily color the graph. Each temp's color directly corresponds to a register or indicates the temp will be spilled.

Other optimizations are summarized in [Optimizations](#optimizations).

**Phase 8:** `lib/phases/p8-convert/`

- Convert the abstract assembly to x86-64 assembly.
- `%r11` is used as an intermediary register to execute assembly instructions which have requirements on the operands which couldn't be respected otherwise. For example, a memory address to memory address `mov` requires the use of an intermediary register, namely `%r11`.
- `%r12` is occasionally used as a secondary intermediary register, particularly in the case of memory to memory `mov`s involving address calculations.
- `%rbp` is used to calculate stack offsets for temps that are stored on the stack.

## Utils

In addition to the starter code we created new util files and modified some others.

The utils folder includes:

**General utilities:** `lib/utils/`
  - `error_msg.mli`: Useful error messages that use filing marking data.

  - `print.mli`: Additional useful print functions such as list, option, and grid printing.

**Data structures:** `lib/utils/data_structures/`

  - `binheap.mli`: Max-binheap for key-value pairs which includes the ability to delete a particular value an the $n$-sized binheap in $O(\log n)$ time. Used to implement the priority queue interface in `pq.ml`.

  - `cfg.mli`: Graph interface which encodes the control flow of a 3-address abstract assembly program.

  - `graph.mli`: Graph interface which takes a node module to make the graph. It allows for graph coloring.

  - `pq.mli`: A priority queue. Used for maximum cardinality search. Uses the `binheap.mli` interface.

**Language utilities:** `lib/utils/lang_utils`

  - `mark.mli`: Module for marking positions within a file. Used for emitting typechecking error messages. 

  - `reg.mli`: Interface for registers which formats them and stores special use registers such as caller/callee saves and argument registers.

  - `size.ml`: Provides an interface for type sizes, which persist through many of the languages in `lib/languages` to describe temps, operands, instructions, and more.

  - `symbol.mli`: Creates symbols identified by their name but much faster to compare. We added a `unique_symbol` function which allows for creation using an existing name.

  - `temp.mli`: Creates temporary variables which are not identified by their names.

  - `type.ml`: Provides an interface for `C0` types, as well as the `Tag` module which allows the typecheck phase to mark types at the expression level.


# Development Best Practices

### GitHub

- Maintain feature branches.
  - EX: `l1-reg-allocator` would be where
all programmers work on the register allocator during lab1.
  - Prefix all branches with the lab (EX `l1-...`).
  - Branch out of feature branches: Make smaller, incremental PRs which merge into their base feature branch.
  - Delete branches once merged.
  
- Optionally Before Committing
  1. Run `dune build @fmt --auto-promote` to auto-format code.
  2. Run `dune build` to make sure the code compiles.

### Useful Commands

From the Compiler folder, test an assembly file with
```
arch -x86_64 gcc -m64 tests/file.s ../runtime/run411.c
```

From the Compiler folder, run the autograder locally with
```
../gradecompiler ../tests/<test suite>/ -m labX [--typecheck-only]
```

Within the lab folder, run tests with
```
dune runtest
```
