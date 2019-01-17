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

void WriteImage(char *filename)
{
    // HACK: Hardcoded program contents:
    // reset:
    // LDA #42
    // STA $6000
    // STA $6002
    // nmi, irq:
    // RTI
    Prg[0] = 0xA9;
    Prg[1] = 0x2A;
    Prg[2] = 0x8D;
    Prg[3] = 0x00;
    Prg[4] = 0x60;
    Prg[5] = 0x8D;
    Prg[6] = 0x02;
    Prg[7] = 0x60;
    Prg[8] = 0x40;
    // Vectors: nmi, reset, irq
    Prg[0x7FFA] = 0x08;
    Prg[0x7FFB] = 0x80;
    Prg[0x7FFC] = 0x00;
    Prg[0x7FFD] = 0x80;
    Prg[0x7FFE] = 0x08;
    Prg[0x7FFF] = 0x80;

    // TODO: Use a specified CHR ROM input file.

    FILE *f = fopen(filename, "wb");
    assert(f);
    assert(fwrite(Header, sizeof(Header), 1, f) == 1);
    assert(fwrite(Prg, sizeof(Prg), 1, f) == 1);
    assert(fwrite(Chr, sizeof(Chr), 1, f) == 1);
    fclose(f);
}
