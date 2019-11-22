Internals
=========

Stages
------

- Tokenizing (source text -> token sequence)
- Parsing (tokens -> AST)
- Simplification (AST -> AST)
- Typechecking (AST -> type errors)
- Optimization (AST -> AST)
- Code generation (AST -> assembly code)
- Assembly (assembly code -> executable image)
- Disassembly


The AST
-------

The AST is represented as a tree of nested dynamically typed list-expressions.
