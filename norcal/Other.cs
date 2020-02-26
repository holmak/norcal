using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

class CAggregateInfo
{
    public readonly AggregateLayout Layout;
    public readonly int TotalSize;
    public readonly CField[] Fields;

    public CAggregateInfo(AggregateLayout layout, int totalSize, CField[] fields)
    {
        Layout = layout;
        TotalSize = totalSize;
        Fields = fields;
    }
}

enum AggregateLayout
{
    Struct,
    Union,
}

class CField
{
    public CType Type;
    public string Name;
    public int Offset;
}

[DebuggerDisplay("{Show(),nq}")]
class CFunctionInfo
{
    public CParameter[] Parameters;
    public CType ReturnType;

    public override bool Equals(object obj)
    {
        throw new NotSupportedException();
    }

    public override int GetHashCode()
    {
        throw new NotSupportedException();
    }

    public string Show()
    {
        var paramTypes = Parameters.Select(x => string.Format("{0} {1}", x.Type.Show(), x.Name));
        return string.Format("function({0}) {1}", string.Join(", ", paramTypes), ReturnType.Show());
    }
}

class CParameter
{
    public readonly CType Type;
    public readonly string Name;

    public CParameter(CType type, string name)
    {
        Type = type;
        Name = name;
    }
}

class LexicalScope
{
    public readonly string Name;
    public readonly HashSet<string> SubscopeNames = new HashSet<string>();
    public readonly Dictionary<string, string> QualifiedNames = new Dictionary<string, string>();

    public LexicalScope(string name)
    {
        Name = name;
    }
}

class LoopScope
{
    public LoopScope Outer;
    public string ContinueLabel;
    public string BreakLabel;
}

struct MemoryRegion
{
    public readonly MemoryRegionTag Tag;
    public readonly int FixedAddress;

    MemoryRegion(MemoryRegionTag tag, int address)
    {
        Tag = tag;
        FixedAddress = address;
    }

    public static readonly MemoryRegion ZeroPage = new MemoryRegion(MemoryRegionTag.ZeroPage, 0);
    public static readonly MemoryRegion Oam = new MemoryRegion(MemoryRegionTag.Oam, 0);
    public static readonly MemoryRegion Ram = new MemoryRegion(MemoryRegionTag.Ram, 0);
    public static readonly MemoryRegion ProgramRom = new MemoryRegion(MemoryRegionTag.ProgramRom, 0);
    public static MemoryRegion Fixed(int address) => new MemoryRegion(MemoryRegionTag.Fixed, address);

    public override string ToString()
    {
        if (Tag == MemoryRegionTag.Fixed) return string.Format("{0}=${1:X4}", Tag, FixedAddress);
        return Tag.ToString();
    }
}

enum MemoryRegionTag
{
    ZeroPage,
    Oam,
    Ram,
    ProgramRom,
    Fixed,
}
