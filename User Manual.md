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
