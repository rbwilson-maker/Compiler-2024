#### Authorship
 * Author: Frank Pfenning <fp@cs.cmu.edu>
 * Modified: Anand Subramanian <asubrama@andrew.cmu.edu>
 * Modified: Rob Simmons <rjsimmon@cs.cmu.edu>
 * Modified: Thea Brick <tbrick@andrew.cmu.edu>

---
## Welcome to 15-411!

This is some starter code for the L1 compiler you have to build for the Lab1.
It contains a lexer, parser, translator, and even a code generator, except that
the code generator creates pseudo assembly language with fictitious instructions
and an unlimited number of registers. We took some care to use good style
(according to the TAs); you may consider this a model for your own coding. Feel
free to modify any and all of this code as you see fit.

---
## SML Notes

There are many different compilers for SML, perhaps the most
popular ones are

  - [SML/NJ](http://www.smlnj.org/)
  - [MLton](http://www.mlton.org/)
  - [Poly/ML](http://www.polyml.org/)

Both Andrew Linux and the autograder use SML/NJ v110.99. The starter
code compiles under both MLton 20210117 and later versions.

In general, SML/NJ and MLton are quite likely to be compatible. Unfortunately,
both use different build systems (SML/NJ uses CM files, MLton uses MLB). You
can generally work around this by maintaining both. If you don't want to
maintain both, [Molasses](https://github.com/T-Brick/molasses) allows for
compiling SML using MLB into CM files (or directly launching the REPL).

The starter code uses the
[Standard Basis Libraries](http://sml-family.org/Basis/). Further resources,
such as documentation for ML-Lex and ML-Yacc, and documentation for the SML/NJ
specific libraries which are used in the starter code, can be found on the
course website.

If you've only used SML for 15-150/15-210, there is an
[SML Help](https://smlhelp.github.io/book/docs/concepts/beyond/) section
which covers things that may be overlooked in those courses but you might find
helpful.

For developing SML, we highly recommend using
[Millet](https://azdavis.net/posts/millet/). This provides hover-types, inline
errors, and more. It makes writing correct SML much easier. To help, we already
supply a `millet.toml` configuration in this directory.

---
## Source Files
The following are the source files for the L1 compiler

    README               -- this file

    Makefile             -- makefile for the compiler
                            For a quick test

        % make              (generates file bin/c0c.heap.<os-tag>)
        % bin/c0c --verbose ../tests0/return01.l1
                      (generate ../tests/return01.s in pseudo assembly)

        % make clean        (removes generated files)
    millet.toml          -- File that Millet uses for some configuration
    compile-c0c.sml      -- SML/NJ commands that will create bin/c0c.heap.<os-tag>
    bin/c0c.sh           -- the script that will run the exported SML heap

    sources.mlb          -- lists all source files, including libraries,
                            and lexer and grammar specifications
    sources.cm           -- same, but for SML/NJ.
                            For a quick test

        % sml
        - CM.make "sources.cm";
        - Top.test "--verbose ../tests0/return01.l1";

                            should generate ../tests/return01.s in pseudo assembly

    parse/ast.sml        -- definition and printer for abstract syntax trees (AST's)
    parse/c0.lex         -- L1 lexer
    parse/c0.grm         -- L1 grammar
    parse/parse.sml      -- L1 parser
    parse/parsestate.sml -- L1 parser support for error messages

    type/typechecker.sml -- (trivial) type-checker for AST

    trans/temp.sml       -- functions to generate and track temp's
    trans/tree.sml       -- definition and pretty printer for IR trees
    trans/trans.sml      -- translation from AST to IR trees

    codegen/assem.sml    -- pseudo assembly format for this starter code
    codegen/codegen.sml  -- pseudo code generator

    util/errormsg.sml    -- error message utilities
    util/flag.sml        -- library for defining flags
    util/mark.sml        -- library for tracking source file positions
    util/safe-io.sml     -- I/O utilities
    util/symbol.sml      -- symbol table library
    util/word32.sml      -- machine word utilities for two's complement interpretation

    top/top.sml          -- top level function for export to binary and testing
    top/go.sml           -- starts compiler for MLton

---
Debugging Hints
---
You can use

  - Top.test "--verbose --dump-ast --dump-ir --dump-assem file.l1";

to print information from all the phases of the current compiler.

If you want to see the internal representations, you can call directly
on SML/NJ's top level:

  - val ast = Parse.parse "file.l1";
  - val ir = Trans.translate ast;
  - val assem = Codegen.codgen ir;

This will use SML/NJ's internal printing function to print the data
structures.  However, not everything will show.

"-" means that the type is opaque.  Sometimes you can replace an opaque
    signature ascription ":>" with a transparent one ":" to see the info.
    For reasons of general hygiene, however, you should change it back
    before handing in.

"#" means that the printing depth is exceeded.  Use

      - Control.Print.printDepth := 100;

    to increase the depth if you need to see more.

"..." means that the printing length is exceeded.  Use

      - Control.Print.printLength := 1000;

    to increase the length if you need to see more.

------------------------------------------------------------------------
Library Hints
------------------------------------------------------------------------
See util/symbol.sml for some uses of libraries provided with SML/NJ
(and some other SML implementations).

RedBlackMapFn and
RedBlackSetFn are likely of general use.

You may also find the following utilities in the smlnj libraries useful:
- HashTable
- PP (Pretty Printer)

To see their interface, you can check
http://www.smlnj.org/doc/smlnj-lib/Manual/toc.html.

HEADS UP: the aforementioned page has not been maintained for a while
now. You may find the documentation out-dated compared to the
inline comments in the source distribution of these libraries.
Please download the sources from the smlnj webpage and lookup the
inline comments for more in-depth documentation.

Alternative libraries include [MPLlib](https://github.com/MPLLang/mpllib),
which includes Sequences, Dictionaries, and other various utilities you may
want. Importantly, this does not currently work with SML/NJ directly. There
currently exists a fork which adds support
[here](https://github.com/T-Brick/mpllib), but there are some bugs still (it
mostly works though).

Hints for MLton use of the SMLNJ library:
http://mlton.org/SMLNJLibrary
