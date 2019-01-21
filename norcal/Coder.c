#define _CRT_SECURE_NO_WARNINGS
#include <assert.h>
#include <stdio.h>
#include "Common.h"

// Documented at http://wiki.nesdev.com/w/index.php/INES
static uint8_t Header[16] =
{
    0x4E, 0x45, 0x53, 0x1A,     // iNES file signature
    0x02,                       // PRG ROM size
    0x01,                       // CHR ROM size
    0x01, 0x00, 0x00, 0x00,     // various flags
    0x00, 0x00, 0x00, 0x00,     // unused
    0x00, 0x00,
};

static uint8_t Prg[32 * 1024];
static uint8_t Chr[8 * 1024];
static size_t PrgNext;

static void Append(uint8_t b)
{
    if (PrgNext >= sizeof(Prg)) Error("program size limit exceeded");
    Prg[PrgNext++] = b;
}

void Emit(Opcode op)
{
    Append(op);
}

void Emit_U8(Opcode op, uint8_t arg)
{
    Append(op);
    Append(arg);
}

void Emit_U16(Opcode op, uint16_t arg)
{
    Append(op);
    Append((uint8_t)arg);
    Append((uint8_t)(arg >> 8));
}

void WriteImage(char *filename)
{
    // HACK: Hardcoded interrupt vector:
    // Interrupts: nmi, reset, irq
    Prg[0x7FFA] = 0x00;
    Prg[0x7FFB] = 0x80;
    Prg[0x7FFC] = 0x00;
    Prg[0x7FFD] = 0x80;
    Prg[0x7FFE] = 0x00;
    Prg[0x7FFF] = 0x80;

    // TODO: Use a specified CHR ROM input file.

    FILE *f = fopen(filename, "wb");
    assert(f);
    assert(fwrite(Header, sizeof(Header), 1, f) == 1);
    assert(fwrite(Prg, sizeof(Prg), 1, f) == 1);
    assert(fwrite(Chr, sizeof(Chr), 1, f) == 1);
    fclose(f);
}
