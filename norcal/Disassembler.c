#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include "Common.h"

// "???" indicates an invalid opcode.
static char *Mnemonics[256] = {
    "BRK", "ORA", "???", "???", "???", "ORA", "ASL", "???", "PHP", "ORA", "ASL", "???", "???", "ORA", "ASL", "???",
    "BPL", "ORA", "???", "???", "???", "ORA", "ASL", "???", "CLC", "ORA", "???", "???", "???", "ORA", "ASL", "???",
    "JSR", "AND", "???", "???", "BIT", "AND", "ROL", "???", "PLP", "AND", "ROL", "???", "BIT", "AND", "ROL", "???",
    "BMI", "AND", "???", "???", "???", "AND", "ROL", "???", "SEC", "AND", "???", "???", "???", "AND", "ROL", "???",
    "RTI", "EOR", "???", "???", "???", "EOR", "LSR", "???", "PHA", "EOR", "LSR", "???", "JMP", "EOR", "LSR", "???",
    "BVC", "EOR", "???", "???", "???", "EOR", "LSR", "???", "CLI", "EOR", "???", "???", "???", "EOR", "LSR", "???",
    "RTS", "ADC", "???", "???", "???", "ADC", "ROR", "???", "PLA", "ADC", "ROR", "???", "JMP", "ADC", "ROR", "???",
    "BVS", "ADC", "???", "???", "???", "ADC", "ROR", "???", "SEI", "ADC", "???", "???", "???", "ADC", "ROR", "???",
    "???", "STA", "???", "???", "STY", "STA", "STX", "???", "DEY", "???", "TXA", "???", "STY", "STA", "STX", "???",
    "BCC", "STA", "???", "???", "STY", "STA", "STX", "???", "TYA", "STA", "TXS", "???", "???", "STA", "???", "???",
    "LDY", "LDA", "LDX", "???", "LDY", "LDA", "LDX", "???", "TAY", "LDA", "TAX", "???", "LDY", "LDA", "LDX", "???",
    "BCS", "LDA", "???", "???", "LDY", "LDA", "LDX", "???", "CLV", "LDA", "TSX", "???", "LDY", "LDA", "LDX", "???",
    "CPY", "CMP", "???", "???", "CPY", "CMP", "DEC", "???", "INY", "CMP", "DEX", "???", "CPY", "CMP", "DEC", "???",
    "BNE", "CMP", "???", "???", "???", "CMP", "DEC", "???", "CLD", "CMP", "???", "???", "???", "CMP", "DEC", "???",
    "CPX", "SBC", "???", "???", "CPX", "SBC", "INC", "???", "INX", "SBC", "NOP", "???", "CPX", "SBC", "INC", "???",
    "BEQ", "SBC", "???", "???", "???", "SBC", "INC", "???", "SED", "SBC", "???", "???", "???", "SBC", "INC", "???",
};

typedef enum OperandFormat
{
    INV, // unknown
    IMP, // none/implicit
    IMM, // immediate
    ZPG, // zero page
    ABS, // absolute (two bytes)
    ZPX, // zp,X
    ZPY, // zp,Y
    ABX, // abs,X
    ABY, // abs,Y
    ZXI, // (zp,X)
    ZYI, // (zp),Y
    REL, // relative (branch)
} OperandFormat;

static uint8_t OperandFormats[256] = {
    IMM, ZXI, INV, INV, INV, ZPG, ZPG, INV, IMP, IMM, IMP, INV, INV, ABS, ABS, INV,
    REL, ZYI, INV, INV, INV, ZPX, ZPX, INV, IMP, ABY, INV, INV, INV, ABX, ABX, INV,
    ABS, ZXI, INV, INV, ZPG, ZPG, ZPG, INV, IMP, IMM, IMP, INV, ABS, ABS, ABS, INV,
    REL, ZYI, INV, INV, INV, ZPX, ZPX, INV, IMP, ABY, INV, INV, INV, ABX, ABX, INV,
    IMP, ZXI, INV, INV, INV, ZPG, ZPG, INV, IMP, IMM, IMP, INV, ABS, ABS, ABS, INV,
    REL, ZYI, INV, INV, INV, ZPX, ZPX, INV, IMP, ABY, INV, INV, INV, ABX, ABX, INV,
    IMP, ZXI, INV, INV, INV, ZPG, ZPG, INV, IMP, IMM, IMP, INV, ABS, ABS, ABS, INV,
    REL, ZYI, INV, INV, INV, ZPX, ZPX, INV, IMP, ABY, INV, INV, INV, ABX, ABX, INV,
    REL, ZXI, INV, INV, ZPG, ZPG, ZPG, INV, IMP, INV, IMP, INV, ABS, ABS, ABS, INV,
    REL, ZYI, INV, INV, ZPX, ZPX, ZPX, INV, IMP, ABY, IMP, INV, INV, ABX, INV, INV,
    IMM, ZXI, IMM, INV, ZPG, ZPG, ZPG, INV, IMP, IMM, IMP, INV, ABS, ABS, ABS, INV,
    REL, ZYI, INV, INV, ZPX, ZPX, ZPX, INV, IMP, ABY, IMP, INV, ABX, ABX, ABX, INV,
    IMM, ZXI, INV, INV, ZPG, ZPG, ZPG, INV, IMP, IMM, IMP, INV, ABS, ABS, ABS, INV,
    REL, ZYI, INV, INV, INV, ZPX, ZPX, INV, IMP, ABY, INV, INV, INV, ABX, ABX, INV,
    IMM, ZXI, INV, INV, ZPG, ZPG, ZPG, INV, IMP, IMM, IMP, INV, ABS, ABS, ABS, INV,
    REL, ZYI, INV, INV, INV, ZPX, ZPX, INV, IMP, ABY, INV, INV, INV, ABX, ABX, INV,
};

// Operand size in bytes; between zero and two. Indexed by OperandFormat.
static uint8_t OperandSizes[] = {
    0,
    0,
    1,
    1,
    2,
    1,
    1,
    2,
    2,
    1,
    1,
    1,
};

static char *OperandFormatStrings[] = {
    " ???",
    "",
    " #$%02X",
    " $%02X",
    " $%04X",
    " $%02X,X",
    " $%02X,Y",
    " $%04X,X",
    " $%04X,Y",
    " ($%02X,X)",
    " ($%02X),Y",
    " %+02X",
};

static void ReadNextComment(FILE *comments, int32_t *address, char *text)
{
    int matched = fscanf(comments, "%X %500[^\n]", address, text);
    if (matched != 2) *address = INT32_MAX;
}

void Disassemble(char *outputfile)
{
    FILE *program = fopen(outputfile, "rb");
    FILE *comments = fopen("comments.txt", "r");
    // TODO: Choose a better disassembly filename.
    FILE *dis = fopen("dis.s", "w");

    // Skip the iNES header.
    for (int i = 0; i < 16; i++) fgetc(program);

    int commentAddress;
    char comment[500];
    ReadNextComment(comments, &commentAddress, comment);

    // Read program bytes:
    const int CodeBaseAddress = 0x8000;
    for (int32_t addr = 0; addr < 0x10000; addr++)
    {
        while (addr >= commentAddress)
        {
            fprintf(dis, "; %s\n", comment);
            ReadNextComment(comments, &commentAddress, comment);
        }

        int opcode = fgetc(program);
        if (opcode == EOF)
        {
            fprintf(dis, "\n");
            break;
        }
        opcode &= 0xFF;

        // TODO: This assumes that BRK instructions won't occur in the middle of the program.
        if (opcode == 0x00) break;

        char *mnem = Mnemonics[opcode];
        OperandFormat format = OperandFormats[opcode];
        int operandSize = OperandSizes[format];

        fprintf(dis, "%04X    %02X", (uint16_t)(CodeBaseAddress + addr), opcode);

        int operand = 0;
        for (int i = 0; i < 2; i++)
        {
            if (i < operandSize)
            {
                int imm = fgetc(program);
                if (imm == EOF) Panic("unexpected end of file while disassembling");
                addr++;
                operand |= (i == 0) ? imm : (imm << 8);
                fprintf(dis, " %02X", imm);
            }
            else
            {
                fprintf(dis, "   ");
            }
        }

        fprintf(dis, "    %s", mnem);
        fprintf(dis, OperandFormatStrings[format], operand);
        fprintf(dis, "\n");
    }

    fclose(program);
    fclose(comments);
    fclose(dis);
}
