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
- Parser and flattener (tokens -> VSM)
- Code generator and typechecker (VSM -> assembly code)
- Assembler (assembly code -> executable image)
- Disassembler (executable image -> assembly code)


Stage: Tokenizer
----------------

The tokenizer is unremarkable.


Stage: Parser and flattener
---------------------------

The parser is implemented in the recursive descent style. This stage emits "Virtual Stack Machine" code. Unlike an AST, VSM code is non-nested; instead, it uses an operand stack and nested operations are represented with reverse polish notation.

In the process of emitting VSM code, the parser prefixes every identifier with its namespace, and checks for references to undefined identifiers. Using fully qualified identifiers means that later stages don't need to track lexical scope.


Stage: Code generator and typechecker
-------------------------------------

Responsibilities:
- Convert VSM code into 6502 assembly.
- References variables and labels symbolically, but can perform constant folding on constant and immediate values.
- Detect type errors and most other semantic errors.
- Most optimizations happen here -- not as a special pass, but by intelligently interpreting the VSM code.

...


Stage: Assembler
----------------

Responsibilities:
- Convert assembly instructions into 6502 machine code.
- Allocate memory for global and local variables.
- Convert symbols into real addresses, possibly after allocating space for the symbol.

...


Stage: Disassembler
-------------------

The disassembler has very limited functionality and is only used to debug the assembler stage.
