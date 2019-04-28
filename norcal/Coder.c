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

#define PrgRomBase 0x8000

static uint8_t Prg[32 * 1024];
static uint8_t Chr[8 * 1024];
static int32_t PrgNext;

static FILE *CommentFile;

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

void EmitComment(char *comment)
{
    if (!CommentFile)
    {
        // TODO: Choose a more appropriate path for the comments file.
        CommentFile = fopen("comments.txt", "w");
        assert(CommentFile);
    }

    char line[256];
    int len = sprintf(line, "%04X    %s\n", (uint16_t)PrgNext, comment);
    assert(fwrite(line, len, 1, CommentFile) == 1);
    fflush(CommentFile);
}

void EmitFix_S8(int32_t address, int32_t target)
{
    int32_t romOffset = address - PrgRomBase;
    if (romOffset < 0 || romOffset >= PrgNext) Panic("fixup address is out of bounds");
    int32_t offset = target - address - 1;
    if (offset < INT8_MIN || offset > INT8_MAX) Panic("branch distance is too far");
    Prg[romOffset] = (uint8_t)offset;
}

void EmitFix_U16(int32_t address, int32_t target)
{
    int32_t romOffset = address - PrgRomBase;
    if (romOffset < 0 || romOffset + 1 >= PrgNext) Panic("fixup address is out of bounds");
    Prg[romOffset] = LowByte(target);
    Prg[romOffset + 1] = HighByte(target);
}

int32_t GetCurrentCodeAddress()
{
    return PrgRomBase + PrgNext;
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
