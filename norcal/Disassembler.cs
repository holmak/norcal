using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

static class Disassembler
{
    public static void Disassemble(string programPath)
    {
        Queue<byte> program = new Queue<byte>(File.ReadAllBytes(programPath));
        StringBuilder dis = new StringBuilder();

        // Skip the iNES header.
        for (int i = 0; i < 16; i++) program.Dequeue();

        // Read program bytes:
        const int CodeBaseAddress = 0x8000;
        for (int addr = 0; addr < 0x10000; addr++)
        {
            byte opcode;
            if (!TryDequeue(program, out opcode))
            {
                dis.AppendLine();
                break;
            }
            opcode &= 0xFF;

            // TODO: This assumes that BRK instructions won't occur in the middle of the program.
            if (opcode == 0x00) break;

            string mnem = AsmInfo.Mnemonics[opcode];
            int format = AsmInfo.OperandFormats[opcode];
            int operandSize = AsmInfo.OperandSizes[format];

            dis.AppendFormat("{0:X4}    {1:X2}", (ushort)(CodeBaseAddress + addr), opcode);

            int operand = 0;
            for (int i = 0; i < 2; i++)
            {
                if (i < operandSize)
                {
                    byte imm;
                    if (!TryDequeue(program, out imm)) Program.Panic("unexpected end of file while disassembling");
                    addr++;
                    operand |= (i == 0) ? imm : (imm << 8);
                    dis.AppendFormat(" {0:X2}", imm);
                }
                else
                {
                    dis.Append("   ");
                }
            }

            dis.AppendFormat("    {0}", mnem);
            dis.AppendFormat(AsmInfo.OperandFormatStrings[format], operand);
            dis.Append("\n");
        }

        Program.WriteDebugFile("dis.s", dis.ToString());
    }

    static bool TryDequeue(Queue<byte> input, out byte item)
    {
        if (input.Count > 0)
        {
            item = input.Dequeue();
            return true;
        }
        else
        {
            item = 0;
            return false;
        }
    }
}

public static class AsmInfo
{
    // "???" indicates an invalid opcode.
    public static string[] Mnemonics = new string[]
    {
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

    // Operand formats:
    public const int INV = 0;  // invalid instruction
    public const int IMP = 1;  // none/implicit
    public const int IMM = 2;  // immediate
    public const int ZPG = 3;  // zero page
    public const int ABS = 4;  // absolute (two bytes)
    public const int ZPX = 5;  // zp,X
    public const int ZPY = 6;  // zp,Y
    public const int ABX = 7;  // abs,X
    public const int ABY = 8;  // abs,Y
    public const int ZXI = 9;  // (zp,X)
    public const int ZYI = 10; // (zp),Y
    public const int REL = 11; // relative (branch)
    public const int ABI = 12;  // absolute indirect (two bytes)

    public static byte[] OperandFormats = {
        IMM, ZXI, INV, INV, INV, ZPG, ZPG, INV, IMP, IMM, IMP, INV, INV, ABS, ABS, INV,
        REL, ZYI, INV, INV, INV, ZPX, ZPX, INV, IMP, ABY, INV, INV, INV, ABX, ABX, INV,
        ABS, ZXI, INV, INV, ZPG, ZPG, ZPG, INV, IMP, IMM, IMP, INV, ABS, ABS, ABS, INV,
        REL, ZYI, INV, INV, INV, ZPX, ZPX, INV, IMP, ABY, INV, INV, INV, ABX, ABX, INV,
        IMP, ZXI, INV, INV, INV, ZPG, ZPG, INV, IMP, IMM, IMP, INV, ABS, ABS, ABS, INV,
        REL, ZYI, INV, INV, INV, ZPX, ZPX, INV, IMP, ABY, INV, INV, INV, ABX, ABX, INV,
        IMP, ZXI, INV, INV, INV, ZPG, ZPG, INV, IMP, IMM, IMP, INV, ABI, ABS, ABS, INV,
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
    public static byte[] OperandSizes = {
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
        2,
    };

    // Indexed by OperandFormat.
    public static string[] OperandFormatStrings = {
        " ???",
        "",
        " #${0:X2}",
        " ${0:X2}",
        " ${0:X4}",
        " ${0:X2},X",
        " ${0:X2},Y",
        " ${0:X4},X",
        " ${0:X4},Y",
        " (${0:X2},X)",
        " (${0:X2}),Y",
        " {0:X2}",
        " (${0:X4})",
    };

    public static readonly string[] ShortJumpInstructions = new string[]
    {
        "BCC",
        "BCS",
        "BEQ",
        "BMI",
        "BNE",
        "BPL",
        "BVC",
        "BVS",
    };

    public static readonly string[] LongJumpInstructions = new string[]
    {
        "JMP",
        "JSR",
    };

    public static bool IsJumpInstruction(string mnemonic)
    {
        return ShortJumpInstructions.Contains(mnemonic) || LongJumpInstructions.Contains(mnemonic);
    }
}
