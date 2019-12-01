using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

class Assembler
{
    LabelScope Labels = new LabelScope();
    List<Fixup> Fixups = new List<Fixup>();
    int ZeroPageNext = ZeroPageStart;
    int RamNext = RamStart;

    static readonly int ZeroPageStart = 0x000;
    static readonly int ZeroPageEnd = 0x100;
    static readonly int RamStart = 0x300;
    static readonly int RamEnd = 0x800;

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
        Assembler assembler = new Assembler();
        assembler.Run(assembly, outputFilename);
    }

    void Run(List<Expr> assembly, string outputFilename)
    {
        // TODO: Use a specified CHR ROM input file.
        byte[] chr = new byte[ChrRomSize];

        List<byte> prg = new List<byte>();
        foreach (Expr e in assembly)
        {
            string label;
            int skipTarget;

            if (e.MatchTag(Tag.Comment))
            {
                // Ignore.
            }
            else if (e.Match(Tag.Function, out label) || e.Match(Tag.Label, out label))
            {
                DefineLabel(label, PrgRomBase + prg.Count);
            }
            else if (e.Match(Tag.BeginScope))
            {
                Labels = new LabelScope
                {
                    Outer = Labels,
                };
            }
            else if (e.Match(Tag.EndScope))
            {
                Labels = Labels.Outer;

                // TODO: If we reach the global scope and there are any unresolved fixups, it is an error.
                //Program.Panic("assembler: label not defined: {0}", label);
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
                int address;
                if (!TryFindLabel(label, out address))
                {
                    // TODO: Figure out what to do if an entrypoint is not defined.
                    Program.Warning("assembler: label not defined: {0}", label);
                    address = PrgRomBase;
                }
                prg.Add(LowByte(address));
                prg.Add(HighByte(address));
            }
            else
            {
                MemoryRegion region;
                int size, number, operandFormat = AsmInfo.INV, operandOffset = 0;
                string name, mnemonic = "???", mode;
                Maybe<string> operandBase = Maybe.Nothing;
                bool isInstruction = false;
                Expr compoundOperand;

                if (e.Match(Tag.Constant, out name, out number))
                {
                    DefineLabel(name, number);
                }
                else if (e.Match(Tag.Variable, out region, out size, out name))
                {
                    AllocateGlobal(region, size);
                }
                else if (e.Match(Tag.Asm, out mnemonic))
                {
                    isInstruction = true;
                    operandFormat = AsmInfo.IMP;
                }
                else if (e.Match(Tag.Asm, out mnemonic, out compoundOperand, out mode))
                {
                    isInstruction = true;
                    operandFormat = ParseAddressMode(mode);
                    int baseAddress = 0;

                    if (AsmInfo.ShortJumpInstructions.Contains(mnemonic))
                    {
                        operandFormat = AsmInfo.REL;
                        // Relative jumps are relative to the address of the subsequent instruction:
                        baseAddress = PrgRomBase + prg.Count + 2;
                    }

                    if (compoundOperand.Match(Tag.AsmOperand, out operandOffset))
                    {
                        operandBase = Maybe.Nothing;
                    }
                    else if (compoundOperand.Match(Tag.AsmOperand, out label, out operandOffset))
                    {
                        operandBase = label;
                    }
                    else
                    {
                        Program.Panic("invalid assembly operand format");
                    }
                }
                else
                {
                    Program.Panic("invalid instruction format: {0}", e.Show());
                }

                if (isInstruction)
                {
                    int formalSize = AsmInfo.OperandSizes[operandFormat];

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
                        prg.Add(candidates[0]);
                    }
                    else if (candidates.Count == 0)
                    {
                        Program.Panic("invalid combination of instruction and mode: {0}", e.Show());
                    }
                    else
                    {
                        Program.Panic("ambiguous instruction and mode: {0}", e.Show());
                    }

                    int operand = 0;
                    bool checkOperandRange = true;
                    if (!operandBase.HasValue)
                    {
                        operand = operandOffset;
                    }
                    else if (TryFindLabel(operandBase.Value, out operand))
                    {
                        operand += operandOffset;
                    }
                    else
                    {
                        checkOperandRange = false;
                        Fixups.Add(new Fixup
                        {
                            Name = operandBase.Value,
                            Location = prg.Count,
                            Type = (operandFormat == AsmInfo.REL) ? FixupType.Relative : FixupType.Absolute,
                        });
                    }

                    // Relative jumps are calculated relative to the address of the subsequent instruction.
                    if (operandFormat == AsmInfo.REL)
                    {
                        operand -= (PrgRomBase + prg.Count + 1);
                    }

                    if (checkOperandRange)
                    {
                        if (operandFormat == AsmInfo.REL && (operand < sbyte.MinValue || operand > sbyte.MaxValue))
                        {
                            Program.Panic("relative branch offset is too large: {0}", e.Show());
                        }
                        else
                        {
                            int actualSize;
                            if (operandFormat == AsmInfo.IMP) actualSize = 0;
                            else if (operand >= sbyte.MinValue && operand <= byte.MaxValue) actualSize = 1;
                            else if (operand >= short.MinValue && operand <= ushort.MaxValue) actualSize = 2;
                            else actualSize = 99;

                            if (actualSize > formalSize)
                            {
                                Program.Panic("assembly operand is too large: {0}", e.Show());
                            }
                        }
                    }

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
            }
        }

        // TODO: Make sure that the PRG ROM size limit isn't exceeded. It must not overwrite the vector table.
        if (prg.Count != PrgRomSize) Program.Error("assembler: program has wrong length");

        using (BinaryWriter w = new BinaryWriter(new FileStream(outputFilename, FileMode.Create)))
        {
            w.Write(Header);
            w.Write(prg.ToArray());
            w.Write(chr);
        }
    }

    void DefineLabel(string label, int address)
    {
        // TODO: Check for duplicates.

        Labels.Table.Add(label, address);

        // TODO: Fix fixups.
    }

    bool TryFindLabel(string label, out int value)
    {
        for (LabelScope scope = Labels; scope != null; scope = scope.Outer)
        {
            if (scope.Table.TryGetValue(label, out value))
            {
                return true;
            }
        }

        value = 0;
        return false;
    }

    static int ParseAddressMode(string mode)
    {
        if (mode == Tag.Absolute) return AsmInfo.ABS;
        else if (mode == Tag.Immediate) return AsmInfo.IMM;
        else if (mode == Tag.IndirectY) return AsmInfo.ZYI;
        else if (mode == Tag.Relative) return AsmInfo.REL;
        else
        {
            Program.Panic("unknown assembly modifier: {0}", mode);
            return AsmInfo.INV;
        }
    }

    static byte LowByte(int n)
    {
        return (byte)(n & 0xFF);
    }

    static byte HighByte(int n)
    {
        return (byte)((n >> 8) & 0xFF);
    }

    int AllocateGlobal(MemoryRegion region, int size)
    {
        int address;
        if (region == MemoryRegion.ZeroPage)
        {
            if (ZeroPageNext + size > ZeroPageEnd) Program.Error("Not enough zero page RAM to allocate global.");
            address = ZeroPageNext;
            ZeroPageNext += size;
        }
        else if (region == MemoryRegion.Ram)
        {
            if (RamNext + size > RamEnd) Program.Error("Not enough RAM to allocate global.");
            address = RamNext;
            RamNext += size;
        }
        else
        {
            Program.NYI();
            address = -1;
        }

        return address;
    }
}

class LabelScope
{
    public LabelScope Outer = null;
    public Dictionary<string, int> Table = new Dictionary<string, int>();
}

enum FixupType
{
    Absolute,
    Relative,
}

class Fixup
{
    public string Name;
    public int Location;
    public FixupType Type;
}
