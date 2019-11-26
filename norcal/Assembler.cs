﻿using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

static class Assembler
{
    // PRG must be exactly 32k, and CHR must be exactly 8k.
    static readonly int ChrRomSize = 0x2000;
    static readonly int PrgRomBase = 0x8000;
    static readonly int PrgRomSize = 0x8000;

    // Documented at http://wiki.nesdev.com/w/index.php/INES
    static readonly byte[] Header = new byte[16]
    {
        0x4E, 0x45, 0x53, 0x1A,     // iNES file signature
        0x02,                       // PRG ROM size
        0x01,                       // CHR ROM size
        0x01, 0x00, 0x00, 0x00,     // various flags
        0x00, 0x00, 0x00, 0x00,     // unused
        0x00, 0x00,
    };

    public static void Assemble(List<Expr> assembly, string outputFilename)
    {
        // TODO: Use a specified CHR ROM input file.
        byte[] chr = new byte[ChrRomSize];

        // First pass: Calculate label values.
        Dictionary<string, int> labels = new Dictionary<string, int>();
        RunPass(
            assembly,
            (label, address) =>
            {
                // TODO: Make sure the address is in the allowed range.
                labels[label] = address;
            },
            (label, baseAddress) => 0);

        // Second pass: Generate machine code.
        List<byte> prg = RunPass(
            assembly,
            (label, address) => { /* ignored */ },
            (label, baseAddress) =>
            {
                int address;
                if (labels.TryGetValue(label, out address)) return address - baseAddress;
                Program.Panic("assembler: label not defined: {0}", label);
                return 0;
            });

        // TODO: Make sure that the PRG ROM size limit isn't exceeded. It must not overwrite the vector table.
        if (prg.Count != PrgRomSize) Program.Error("assembler: program has wrong length");

        using (BinaryWriter w = new BinaryWriter(new FileStream(outputFilename, FileMode.Create)))
        {
            w.Write(Header);
            w.Write(prg.ToArray());
            w.Write(chr);
        }
    }

    enum LabelType
    {
        Relative,
        Absolute,
    }

    static List<byte> RunPass(List<Expr> input, Action<string, int> defineLabel, Func<string, int, int> findLabel)
    {
        List<byte> prg = new List<byte>();

        foreach (Expr e in input)
        {
            string label;
            int skipTarget;

            if (e.MatchTag(Tag.Comment))
            {
                // Ignore.
            }
            else if (e.Match(Tag.Function, out label) || e.Match(Tag.Label, out label))
            {
                defineLabel(label, PrgRomBase + prg.Count);
            }
            else if (e.Match(Tag.SkipTo, out skipTarget))
            {
                skipTarget -= PrgRomBase;

                // Pad out the code until reaching the target address:
                if (skipTarget > ushort.MaxValue) Program.Panic("assembler: skip address is too large");
                if (prg.Count > skipTarget) Program.Panic("assembler: cannot skip backward");
                while (prg.Count < skipTarget)
                {
                    prg.Add(0);
                }
            }
            else if (e.Match(Tag.Word, out label))
            {
                int address = findLabel(label, 0);
                prg.Add(LowByte(address));
                prg.Add(HighByte(address));
            }
            else
            {
                string mnemonic = "???";
                int operand = 0;
                string modifier;
                int operandFormat = AsmInfo.INV;
                if (e.Match(out mnemonic))
                {
                    operandFormat = AsmInfo.IMP;
                }
                else if (e.Match(out mnemonic, out operand, out modifier))
                {
                    if (modifier == Tag.Absolute) operandFormat = AsmInfo.ABS;
                    else if (modifier == Tag.Immediate) operandFormat = AsmInfo.IMM;
                    else if (modifier == Tag.IndirectY) operandFormat = AsmInfo.ZYI;
                    else Program.Panic("unknown assembly modifier: {0}", modifier);
                }
                else if (e.Match(out mnemonic, out label))
                {
                    int baseAddress;
                    if (AsmInfo.ShortJumpInstructions.Contains(mnemonic))
                    {
                        operandFormat = AsmInfo.REL;
                        // Relative jumps are always relative to the address of the *following* instruction:
                        baseAddress = PrgRomBase + prg.Count + 2;
                    }
                    else if (AsmInfo.LongJumpInstructions.Contains(mnemonic))
                    {
                        operandFormat = AsmInfo.ABS;
                        baseAddress = 0;
                    }
                    else
                    {
                        operandFormat = AsmInfo.INV;
                        baseAddress = 0;
                    }
                    operand = findLabel(label, baseAddress);
                }
                else
                {
                    Program.Panic("invalid instruction format: {0}", e.Show());
                }

                int formalSize = AsmInfo.OperandSizes[operandFormat];

                int actualSize;
                if (operandFormat == AsmInfo.IMP) actualSize = 0;
                else if (operand >= sbyte.MinValue && operand <= byte.MaxValue) actualSize = 1;
                else if (operand >= short.MinValue && operand <= ushort.MaxValue) actualSize = 2;
                else actualSize = 99;

                if (actualSize > formalSize)
                {
                    Program.Panic("assembly operand is too large: {0}", e.Show());
                }

                // Search for a matching instruction:
                List<byte> candidates = new List<byte>();
                for (int opcode = 0; opcode < 256; opcode++)
                {
                    if (mnemonic == AsmInfo.Mnemonics[opcode] &&
                        operandFormat == AsmInfo.OperandFormats[opcode])
                    {
                        candidates.Add((byte)opcode);
                    }
                }

                if (candidates.Count == 1)
                {
                    if (AsmInfo.ShortJumpInstructions.Contains(mnemonic) && (operand < sbyte.MinValue || operand > sbyte.MaxValue))
                    {
                        Program.Panic("relative branch offset is too large");
                    }

                    prg.Add(candidates[0]);

                    if (formalSize == 1)
                    {
                        prg.Add(LowByte(operand));
                    }
                    else if (formalSize == 2)
                    {
                        prg.Add(LowByte(operand));
                        prg.Add(HighByte(operand));
                    }
                }
                else
                {
                    Program.Panic("invalid or ambiguous instruction: {0}", e.Show());
                }
            }
        }

        return prg;
    }

    static byte LowByte(int n)
    {
        return (byte)(n & 0xFF);
    }

    static byte HighByte(int n)
    {
        return (byte)((n >> 8) & 0xFF);
    }
}

public class Instruction
{
    public byte Opcode;
    public int Operand;
    public int OperandFormat;
}
