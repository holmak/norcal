using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

class AggregateInfo
{
    public readonly AggregateLayout Layout;
    public readonly int TotalSize;
    public readonly FieldInfo[] Fields;

    public AggregateInfo(AggregateLayout layout, int totalSize, FieldInfo[] fields)
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

class FieldInfo
{
    public readonly CType Type;
    public readonly string Name;
    public readonly int Offset;

    public FieldInfo(CType type, string name, int offset)
    {
        Type = type;
        Name = name;
        Offset = offset;
    }
}

[DebuggerDisplay("{Show(),nq}")]
class CFunctionInfo
{
    public FieldInfo[] Parameters;
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
