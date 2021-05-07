NORCAL
======

NORCAL is a C compiler for the NES.


Usage
-----

norcal first.c second.c ... [-o out.nes] [--debug-output]


Differences from Standard C
---------------------------

- There is a single translation unit: all input files are parsed as if concatenated. Keywords "static" and "extern" are ignored. Forward declarations are not required or allowed.

- There is no preprocessor.

- "define" declarations, which are typed, replace "#define" declarations -- and only work with numeric constants:

 `define uint8_t MAX_SPRITES = 6;`.

- Labels and "ordinary" identifiers share the same namespace. Labels still have function scope, not lexical scope.


Extension: Variable location specifiers
---------------------------------------

~~~
__zeropage uint8_t vital_counter;

__location(0x2000) uint8_t PPU_CTRL;

__prg_rom uint8_t PALETTES[] =
{
    0x0F, 0x00, 0x10, 0x20,
    0x0F, 0x00, 0x10, 0x20,
    0x0F, 0x00, 0x10, 0x20,
    0x0F, 0x00, 0x10, 0x20,
};
~~~


Extension: Inline assembly
--------------------------

~~~
uint8_t my_func(uint8_t x, uint8_t y)
{
    uint8_t temp = x;
    __asm
    {
        LDA x
        ROL A
        CLC
        ADC y
        ADC temp
    }
    return x;
}
~~~
