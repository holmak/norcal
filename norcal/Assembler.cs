using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

class Assembler
{
    Dictionary<string, AsmSymbol> Symbols = new Dictionary<string, AsmSymbol>();
    List<Fixup> Fixups = new List<Fixup>();
    DebugExporter Debug = new DebugExporter();

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

    public static void Assemble(IReadOnlyList<Expr> assembly, string outputFilename)
    {
        Assembler assembler = new Assembler();
        assembler.Run(assembly, outputFilename);
    }

    void Run(IReadOnlyList<Expr> assembly, string outputFilename)
    {
        // TODO: Use a specified CHR ROM input file.
        byte[] chr = File.ReadAllBytes("chr.bin");
        if (chr.Length != ChrRomSize) Program.Error("CHR ROM has wrong length: ", chr.Length);

        List<byte> prg = new List<byte>();
        List<string> comments = new List<string>();

        foreach (Expr e in assembly)
        {
            string label, name, mnemonic, text;
            int skipTarget, address, size;
            byte[] bytes;
            AsmOperand operand;

            if (e.Match(Tag.Comment, out text))
            {
                comments.Add(text);
            }
            else if (e.Match(Tag.Function, out label))
            {
                address = PrgRomBase + prg.Count;
                DefineSymbol(prg, label, address, isLabel: false);
                Debug.AddFunction(label, address);

                // Delete any (local) label definitions; they are no longer in scope.
                string[] labels = Symbols.Where(x => x.Value.IsLabel).Select(x => x.Key).ToArray();
                foreach (string key in labels)
                {
                    Symbols.Remove(key);
                }
            }
            else if (e.Match(Tag.Label, out label))
            {
                address = PrgRomBase + prg.Count;
                DefineSymbol(prg, label, address, isLabel: true);
                Debug.AddFunction(label, address);
            }
            else if (e.Match(Tag.SkipTo, out skipTarget))
            {
                skipTarget -= PrgRomBase;

                // Pad out the code until reaching the target address:
                if (skipTarget > ushort.MaxValue) Program.Panic("assembler: warning: skip address is too large");
                if (prg.Count > skipTarget) Program.Panic("assembler: warning: cannot skip backward");
                while (prg.Count < skipTarget)
                {
                    prg.Add(0);
                }
            }
            else if (e.Match(Tag.Word, out label))
            {
                AsmSymbol sym;
                if (Symbols.TryGetValue(label, out sym))
                {
                    address = sym.Value;
                }
                else
                {
                    // TODO: Figure out what to do if an entrypoint is not defined.
                    Program.Warning("assembler: warning: label not defined: {0}", label);
                    address = PrgRomBase;
                }
                prg.Add(LowByte(address));
                prg.Add(HighByte(address));
            }
            else if (e.Match(Tag.Variable, out name, out address, out size))
            {
                Debug.AddVariable(name, address, size);
            }
            else if (e.Match(Tag.ReadonlyData, out name, out bytes))
            {
                address = PrgRomBase + prg.Count;
                DefineSymbol(prg, name, address, isLabel: false);
                prg.AddRange(bytes);
                Debug.AddVariable(name, address, bytes.Length);
            }
            else if (e.Match(Tag.Asm, out mnemonic, out operand))
            {
                bool isRelativeBranchToAbsoluteTarget = AsmInfo.ShortJumpInstructions.Contains(mnemonic) && operand.Mode == AddressMode.Absolute;
                AddressMode actualMode = operand.Mode;

                if (isRelativeBranchToAbsoluteTarget)
                {
                    actualMode = AddressMode.Relative;
                }

                // Use a smaller address mode when possible:
                if (operand.Mode == AddressMode.AbsoluteX && operand.Offset < 256)
                {
                    actualMode = AddressMode.ZeroPageX;
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
                    int instructionSize = 1 + formalSize;
                    if (prg.Count + instructionSize > PrgRomSize)
                    {
                        Program.Error("not enough PRG-ROM for code");
                    }

                    Debug.TagInstruction(PrgRomBase + prg.Count, instructionSize, comments);
                    comments.Clear();

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

        // All symbolic references should be fixed by now:
        foreach (Fixup fixup in Fixups)
        {
            Program.Error("symbol not defined: {0}", fixup.Operand.Show());
        }

        // Make sure that the PRG ROM size limit isn't exceeded. It must not overwrite the vector table.
        if (prg.Count != PrgRomSize) Program.Error("assembler: program has wrong length");

        using (BinaryWriter w = new BinaryWriter(new FileStream(outputFilename, FileMode.Create)))
        {
            w.Write(Header);
            w.Write(prg.ToArray());
            w.Write(chr);
        }

        Debug.Save(Path.ChangeExtension(outputFilename, ".dbg"));
    }

    void DefineSymbol(List<byte> prg, string symbol, int address, bool isLabel)
    {
        if (Symbols.ContainsKey(symbol)) Program.Panic("duplicate symbol definitions should not reach the assembler");
        Symbols.Add(symbol, new AsmSymbol
        {
            Value = address,
            IsLabel = isLabel,
        });

        // Fix any references to this symbol:
        DoFixups(prg);
    }

    void DoFixups(List<byte> prg)
    {
        foreach (Fixup fixup in Fixups)
        {
            int target;
            if (TryGetOperandValue(fixup.Operand, fixup.Mode, fixup.Location, fixup.IsRelativeBranchToAbsoluteTarget, out target))
            {
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
                fixup.Fixed = true;
            }
        }

        Fixups.RemoveAll(x => x.Fixed);
    }

    bool TryGetOperandValue(AsmOperand operand, AddressMode mode, int location, bool isRelativeBranchToAbsoluteTarget, out int value)
    {
        value = 0;

        AsmSymbol sym;
        if (!operand.Base.HasValue)
        {
            value += operand.Offset;
        }
        else if (Symbols.TryGetValue(operand.Base.Value, out sym))
        {
            value += sym.Value;
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
            Program.Error("relative branch offset is too large: {0}", operand.Show());
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
                Program.Error("assembly operand is too large: {0}", operand.Show());
            }
        }

        return true;
    }

    static int ParseAddressMode(AddressMode mode)
    {
        if (mode == AddressMode.Implicit) return AsmInfo.IMP;
        else if (mode == AddressMode.ZeroPage) return AsmInfo.ZPG;
        else if (mode == AddressMode.ZeroPageX) return AsmInfo.ZPX;
        else if (mode == AddressMode.Absolute) return AsmInfo.ABS;
        else if (mode == AddressMode.AbsoluteX) return AsmInfo.ABX;
        else if (mode == AddressMode.AbsoluteY) return AsmInfo.ABY;
        else if (mode == AddressMode.Immediate) return AsmInfo.IMM;
        else if (mode == AddressMode.Indirect) return AsmInfo.INV;
        else if (mode == AddressMode.IndirectX) return AsmInfo.ZXI;
        else if (mode == AddressMode.IndirectY) return AsmInfo.ZYI;
        else if (mode == AddressMode.Relative) return AsmInfo.REL;
        else
        {
            Program.UnhandledCase();
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
}

class Fixup
{
    public AsmOperand Operand;
    public int Location;
    public AddressMode Mode;
    public bool IsRelativeBranchToAbsoluteTarget;
    public bool Fixed = false;
}

[DebuggerDisplay("{Show(),nq}")]
class AsmOperand
{
    public readonly Maybe<string> Base = Maybe.Nothing;
    public readonly int Offset = 0;
    public readonly AddressMode Mode;
    public readonly ImmediateModifier Modifier;
    public readonly string Comment;

    public static readonly AsmOperand Implicit = new AsmOperand(0, AddressMode.Implicit);

    public AsmOperand(string actualBase, AddressMode mode) : this(actualBase, 0, mode, ImmediateModifier.None) { }

    public AsmOperand(string actualBase, ImmediateModifier modifier) : this(actualBase, 0, AddressMode.Immediate, modifier) { }

    public AsmOperand(int value, AddressMode mode) : this(Maybe.Nothing, value, mode, ImmediateModifier.None) { }

    public AsmOperand(int value, ImmediateModifier modifier) : this(Maybe.Nothing, value, AddressMode.Immediate, modifier) { }

    public AsmOperand(Maybe<string> optionalBase, int offset, AddressMode mode)
        : this(optionalBase, offset, mode, ImmediateModifier.None)
    {
    }

    public AsmOperand(Maybe<string> optionalBase, int offset, AddressMode mode, ImmediateModifier modifier)
        : this(optionalBase, offset, mode, modifier, null)
    {
    }

    public AsmOperand(Maybe<string> optionalBase, int offset, AddressMode mode, ImmediateModifier modifier, string comment)
    {
        Base = optionalBase;
        Offset = offset;
        Mode = mode;
        Modifier = modifier;
        Comment = comment;

        if (Mode != AddressMode.Immediate && modifier != ImmediateModifier.None)
        {
            Program.Panic("non-immediate operands cannot be modified");
        }
    }

    public AsmOperand WithMode(AddressMode newMode)
    {
        return new AsmOperand(Base, Offset, newMode, Modifier, Comment);
    }

    public AsmOperand WithComment(string newComment)
    {
        return new AsmOperand(Base, Offset, Mode, Modifier, newComment);
    }

    public AsmOperand WithComment(string newCommentFormat, params object[] args)
    {
        return WithComment(string.Format(newCommentFormat, args));
    }

    public AsmOperand ReplaceBase(int baseValue)
    {
        if (!Base.HasValue) throw new Exception("this operand has no base symbol");
        return new AsmOperand(Maybe.Nothing, baseValue + Offset, Mode, Modifier, Comment);
    }

    public string Show()
    {
        string s;
        if (!Base.HasValue)
        {
            s = Program.FormatAssemblyInteger(Offset);
        }
        else if (Offset == 0)
        {
            s = Base.Value;
        }
        else
        {
            s = string.Format("{0}+{1}", Base.Value, Program.FormatAssemblyInteger(Offset));
        }

        // Show the immediate modifier effect:
        if (Modifier == ImmediateModifier.LowByte) s = "<" + s;
        else if (Modifier == ImmediateModifier.HighByte) s = ">" + s;

        // Show the address mode:
        string format = "???";
        if (Mode == AddressMode.Implicit) format = "<implicit>";
        else if (Mode == AddressMode.Immediate) format = "#{0}";
        else if (Mode == AddressMode.ZeroPage) format = "{0}";
        else if (Mode == AddressMode.ZeroPageX) format = "{0},X";
        else if (Mode == AddressMode.Absolute) format = "{0}";
        else if (Mode == AddressMode.AbsoluteX) format = "{0},X";
        else if (Mode == AddressMode.AbsoluteY) format = "{0},Y";
        else if (Mode == AddressMode.Indirect) format = "({0})";
        else if (Mode == AddressMode.IndirectX) format = "({0},X)";
        else if (Mode == AddressMode.IndirectY) format = "({0}),Y";
        else if (Mode == AddressMode.Relative) format = "+{0}";

        s = string.Format(format, s);

        if (Comment != null) s = string.Format("{0}; {1}", s.PadRight(8), Comment);

        return s;
    }
}

enum AddressMode
{
    Implicit,
    Immediate,
    ZeroPage,
    ZeroPageX,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndirectX,
    IndirectY,
    Relative,
}

enum ImmediateModifier
{
    None,
    LowByte,
    HighByte,
}

class AsmSymbol
{
    public int Value;
    public bool IsLabel;
}
