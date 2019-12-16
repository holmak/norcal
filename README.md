NORCAL
======

NORCAL is a C compiler for the NES. This file describes how to use the program and provides an overview of its implementation.


User Manual
===========

Differences from C (by design)
------------------------------

- Single translation unit; all input files are concatenated in order; "static" and "extern" are ignored; forward declarations work normally.

- No preprocessor directives.

- Extension: "define" declarations, which are typed, replace "#define" declarations -- and only work with numeric constants; `define uint8_t MAX_SPRITES = 6;`.

- Labels and "ordinary" identifiers share the same namespace. Labels still have function scope, not lexical scope.


Extension: Variable location specifiers
---------------------------------------

~~~
__zeropage uint8_t vital_counter;
__location(0x2000) uint8_t PPU_CTRL;
~~~


Extension: Inline assembly
--------------------------

~~~
uint8_t my_func(uint8_t x, uint8_t y)
{
    uint8_t temp = x;
    __asm
    {
        LDA x;
        ROL A;
        CLC;
        ADC y;
        ADC temp;
    }
    return x;
}
~~~


Extension: Binary literals
--------------------------

...


Issues / Bugs / Missing features
--------------------------------

...


Implementation Notes
====================

Stages
------

- Tokenizer (source text -> token sequence)
- Parser (tokens -> AST)
- Flattener (AST -> VSM)
- Code generator and typechecker (VSM -> assembly code)
- Assembler (assembly code -> executable image)
- Disassembler (executable image -> assembly code)


Stage: Tokenizer and Parser
---------------------------

The tokenizer and parser are unremarkable. The parser is implemented in the recursive descent style, and produces a standard Abstract Syntax Tree. The AST is represented as a tree of nested dynamically typed list-expressions.


Stage: Flattener
----------------

The flattening stage turns the AST into "Virtual Stack Machine" code. Unlike an AST, VSM code is non-nested; instead, it uses an operand stack and nested operations are represented with reverse polish notation. The flattener also applies namespace prefixes to all identifiers so that each symbol has a globally unique name. Together, these changes make VSM code much easier for the code generator and typechecker to analyze.
