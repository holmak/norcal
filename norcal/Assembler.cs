using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

class Assembler
{
    Dictionary<string, int> Symbols = new Dictionary<string, int>();
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
            string label, name, mnemonic;
            int skipTarget, size;
            MemoryRegion region;
            AsmOperand operand;

            if (e.MatchTag(Tag.Comment))
            {
                // Ignore.
            }
            else if (e.Match(Tag.Function, out label) || e.Match(Tag.Label, out label))
            {
                DefineSymbol(label, PrgRomBase + prg.Count);
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
                if (!Symbols.TryGetValue(label, out address))
                {
                    // TODO: Figure out what to do if an entrypoint is not defined.
                    Program.Warning("assembler: label not defined: {0}", label);
                    address = PrgRomBase;
                }
                prg.Add(LowByte(address));
                prg.Add(HighByte(address));
            }
            else if (e.Match(Tag.Variable, out region, out size, out name))
            {
                DefineSymbol(name, AllocateGlobal(region, size));
            }
            else if (e.Match(Tag.Asm, out mnemonic, out operand))
            {
                bool isRelativeBranchToAbsoluteTarget = AsmInfo.ShortJumpInstructions.Contains(mnemonic) && operand.Mode == AddressMode.Absolute;
                AddressMode actualMode = operand.Mode;

                if (isRelativeBranchToAbsoluteTarget)
                {
                    actualMode = AddressMode.Relative;
                }

                // Search for a matching instruction:
                int formalSize;
                List<byte> candidates = new List<byte>();
                {
                    int operandFormat = ParseAddressMode(actualMode);
                    formalSize = AsmInfo.OperandSizes[operandFormat];

                    for (int opcode = 0; opcode < 256; opcode++)
                    {
                        if (mnemonic == AsmInfo.Mnemonics[opcode] && operandFormat == AsmInfo.OperandFormats[opcode])
                        {
                            candidates.Add((byte)opcode);
                        }
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

                int operandValue;
                if (!TryGetOperandValue(operand, actualMode, prg.Count, isRelativeBranchToAbsoluteTarget, out operandValue))
                {
                    Fixups.Add(new Fixup
                    {
                        Operand = operand,
                        Location = prg.Count,
                        Mode = actualMode,
                        IsRelativeBranchToAbsoluteTarget = isRelativeBranchToAbsoluteTarget,
                    });
                    operandValue = 0;
                }

                if (formalSize == 1)
                {
                    prg.Add(LowByte(operandValue));
                }
                else if (formalSize == 2)
                {
                    prg.Add(LowByte(operandValue));
                    prg.Add(HighByte(operandValue));
                }
            }
            else
            {
                Program.Panic("invalid instruction format: {0}", e.Show());
            }
        }

        foreach (Fixup fixup in Fixups)
        {
            int target = 0;
            if (!TryGetOperandValue(fixup.Operand, fixup.Mode, fixup.Location, fixup.IsRelativeBranchToAbsoluteTarget, out target))
            {
                Program.Panic("symbol not defined: {0}", fixup.Operand.Show());
            }

            int formalSize = GetFormalOperandSize(fixup.Mode);
            if (formalSize == 1)
            {
                prg[fixup.Location] = LowByte(target);
            }
            else if (formalSize == 2)
            {
                prg[fixup.Location] = LowByte(target);
                prg[fixup.Location + 1] = HighByte(target);
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

    void DefineSymbol(string symbol, int address)
    {
        if (Symbols.ContainsKey(symbol)) Program.Panic("duplicate symbol definitions should not reach the assembler");
        Symbols.Add(symbol, address);
    }

    bool TryGetOperandValue(AsmOperand operand, AddressMode mode, int location, bool isRelativeBranchToAbsoluteTarget, out int value)
    {
        value = 0;

        int symbolAddress;
        if (!operand.Base.HasValue)
        {
            value += operand.Offset;
        }
        else if (Symbols.TryGetValue(operand.Base.Value, out symbolAddress))
        {
            value += symbolAddress;
            value += operand.Offset;
        }
        else
        {
            return false;
        }

        if (isRelativeBranchToAbsoluteTarget)
        {
            // Relative jumps are calculated relative to the address of the subsequent instruction.
            value -= (PrgRomBase + location + 1);
        }

        if (operand.Modifier == ImmediateModifier.LowByte)
        {
            value = value & 0xFF;
        }
        else if (operand.Modifier == ImmediateModifier.HighByte)
        {
            value = (value >> 8) & 0xFF;
        }

        if (mode == AddressMode.Relative && (value < sbyte.MinValue || value > sbyte.MaxValue))
        {
            Program.Panic("relative branch offset is too large: {0}", operand.Show());
        }
        else
        {
            int actualSize;
            if (mode == AddressMode.Implicit) actualSize = 0;
            else if (value >= sbyte.MinValue && value <= byte.MaxValue) actualSize = 1;
            else if (value >= short.MinValue && value <= ushort.MaxValue) actualSize = 2;
            else actualSize = 99;

            if (actualSize > GetFormalOperandSize(mode))
            {
                Program.Panic("assembly operand is too large: {0}", operand.Show());
            }
        }

        return true;
    }

    static int ParseAddressMode(AddressMode mode)
    {
        if (mode == AddressMode.Implicit) return AsmInfo.IMP;
        else if (mode == AddressMode.Absolute) return AsmInfo.ABS;
        else if (mode == AddressMode.Immediate) return AsmInfo.IMM;
        else if (mode == AddressMode.IndirectY) return AsmInfo.ZYI;
        else if (mode == AddressMode.Relative) return AsmInfo.REL;
        else
        {
            Program.Panic("unknown assembly modifier: {0}", mode);
            return AsmInfo.INV;
        }
    }

    static int GetFormalOperandSize(AddressMode mode)
    {
        return AsmInfo.OperandSizes[ParseAddressMode(mode)];
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
        if (region.Tag == MemoryRegionTag.ZeroPage)
        {
            if (ZeroPageNext + size > ZeroPageEnd) Program.Error("Not enough zero page RAM to allocate global.");
            address = ZeroPageNext;
            ZeroPageNext += size;
        }
        else if (region.Tag == MemoryRegionTag.Ram)
        {
            if (RamNext + size > RamEnd) Program.Error("Not enough RAM to allocate global.");
            address = RamNext;
            RamNext += size;
        }
        else if (region.Tag == MemoryRegionTag.Fixed)
        {
            if (region.FixedAddress < 0 || region.FixedAddress > ushort.MaxValue) Program.Error("Invalid address.");
            address = region.FixedAddress;
        }
        else
        {
            Program.NYI();
            address = -1;
        }

        return address;
    }
}

class Fixup
{
    public AsmOperand Operand;
    public int Location;
    public AddressMode Mode;
    public bool IsRelativeBranchToAbsoluteTarget;
}

[DebuggerDisplay("{Show(),nq}")]
class AsmOperand
{
    public readonly Maybe<string> Base = Maybe.Nothing;
    public readonly int Offset = 0;
    public readonly AddressMode Mode;
    public readonly ImmediateModifier Modifier;

    public static readonly AsmOperand Implicit = new AsmOperand(0, AddressMode.Implicit);

    public AsmOperand(string actualBase, AddressMode mode) : this(actualBase, 0, mode, ImmediateModifier.None) { }

    public AsmOperand(string actualBase, ImmediateModifier modifier) : this(actualBase, 0, AddressMode.Immediate, modifier) { }

    public AsmOperand(int value, AddressMode mode) : this(Maybe.Nothing, value, mode, ImmediateModifier.None) { }

    public AsmOperand(int value, ImmediateModifier modifier) : this(Maybe.Nothing, value, AddressMode.Immediate, modifier) { }

    public AsmOperand(Maybe<string> optionalBase, int offset, AddressMode mode) : this(optionalBase, offset, mode, ImmediateModifier.None) { }

    private AsmOperand(Maybe<string> optionalBase, int offset, AddressMode mode, ImmediateModifier modifier)
    {
        Base = optionalBase;
        Offset = offset;
        Mode = mode;
        Modifier = modifier;

        if (Mode != AddressMode.Immediate && modifier != ImmediateModifier.None)
        {
            Program.Panic("non-immediate operands cannot be modified");
        }
    }

    public string Show()
    {
        if (!Base.HasValue)
        {
            return Program.FormatAssemblyInteger(Offset);
        }
        else if (Offset == 0)
        {
            return Base.Value;
        }
        else
        {
            return string.Format("{0}+{1}", Base.Value, Program.FormatAssemblyInteger(Offset));
        }
    }
}

enum AddressMode
{
    Implicit,
    Immediate,
    ZeroPage,
    Absolute,
    IndirectY,
    Relative,
}

enum ImmediateModifier
{
    None,
    LowByte,
    HighByte,
}
