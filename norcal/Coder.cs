using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

partial class Compiler
{
    List<byte> Prg = new List<byte>();
    List<byte> Chr = new List<byte>();
    int PrgNext => Prg.Count;
    List<string> Comments = new List<string>();

    static readonly int PrgRomBase = 0x8000;

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

    void Append(byte b)
    {
        if (PrgNext >= 0x8000) Program.Error("program size limit exceeded");
        Prg.Add(b);
    }

    void Append(Opcode op) => Append((byte)op);

    void Emit(Opcode op)
    {
        Append(op);
    }

    void Emit_U8(Opcode op, int arg)
    {
        // TODO: Verify that "arg" is in range.
        Append(op);
        Append((byte)arg);
    }

    void Emit_U16(Opcode op, int arg)
    {
        // TODO: Verify that "arg" is in range.
        Append(op);
        Append((byte)arg);
        Append((byte)(arg >> 8));
    }

    void EmitComment(string comment)
    {
        Comments.Add(string.Format("{0:X4}    {1}", (ushort)PrgNext, comment));
    }

    void EmitFix_S8(int address, int target)
    {
        int romOffset = address - PrgRomBase;
        if (romOffset < 0 || romOffset >= PrgNext) Program.Panic("fixup address is out of bounds");
        int offset = target - address - 1;
        if (offset < sbyte.MinValue || offset > sbyte.MaxValue) Program.Panic("branch distance is too far");
        Prg[romOffset] = (byte)offset;
    }

    void EmitFix_U16(int address, int target)
    {
        int romOffset = address - PrgRomBase;
        if (romOffset < 0 || romOffset + 1 >= PrgNext) Program.Panic("fixup address is out of bounds");
        Prg[romOffset] = LowByte(target);
        Prg[romOffset + 1] = HighByte(target);
    }

    int GetCurrentCodeAddress()
    {
        return PrgRomBase + PrgNext;
    }

    public void WriteImage(string filename)
    {
        // TODO: Use a specified CHR ROM input file.
        // TODO: Make sure that the PRG ROM size limit isn't exceeded. It must not overwrite the vector table.

        // PRG must be exactly 32k, and CHR must be exactly 8k.
        PadList(Prg, 32 * 1024);
        PadList(Chr, 8 * 1024);

        // Interrupts: nmi, reset, irq
        int nmi = FindVectorFunction("nmi");
        int reset = FindVectorFunction("reset");
        int brk = FindVectorFunction("brk");
        Prg[0x7FFA] = LowByte(nmi);
        Prg[0x7FFB] = HighByte(nmi);
        Prg[0x7FFC] = LowByte(reset);
        Prg[0x7FFD] = HighByte(reset);
        Prg[0x7FFE] = LowByte(brk);
        Prg[0x7FFF] = HighByte(brk);

        using (BinaryWriter w = new BinaryWriter(new FileStream(filename, FileMode.Create)))
        {
            w.Write(Header);
            w.Write(Prg.ToArray());
            w.Write(Chr.ToArray());
        }

        // Write the comment file:
        // TODO: Choose a more appropriate path for the comments file.
        File.WriteAllLines("comments.txt", Comments);
    }

    void PadList(List<byte> list, int size)
    {
        if (list.Count > size) Program.Panic("ROM is too large");
        while (list.Count < size) list.Add(0);
    }

    int FindVectorFunction(string name)
    {
        Symbol sym;
        if (FindSymbol(name, out sym))
        {
            // Vector functions must be functions that take no arguments and return nothing.
            if (sym.Type.Tag != CTypeTag.Function) Program.Error("vectors must be functions");
            if (sym.Type.Parameters.Count != 0) Program.Error("vectors cannot take arguments");
            if (sym.Type.Subtype != CType.Void) Program.Error("vectors cannot return a value");
            return sym.Value;
        }
        else
        {
            // TODO: Decide what to do if a vector-handling function is missing.
            return 0;
        }
    }
}
