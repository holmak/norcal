using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

class DebugExporter
{
    List<SegmentInfo> Segments = new List<SegmentInfo>();
    List<SymbolInfo> Symbols = new List<SymbolInfo>();

    public DebugExporter()
    {
        // HACK: This is hard-coded to match the values hard-coded in Assembler.
        Segments.Add(new SegmentInfo() { ID = 0, Start = 0x0000, Size = 0x0800, IsWritable = true, IsPrgRom = false });
        Segments.Add(new SegmentInfo() { ID = 1, Start = 0x6000, Size = 0x0006, IsWritable = false, IsPrgRom = false });
        Segments.Add(new SegmentInfo() { ID = 2, Start = 0x8000, Size = 0x8000, IsWritable = false, IsPrgRom = true });
    }

    public void AddVariable(string name, int address, int size)
    {
        Symbols.Add(new SymbolInfo()
        {
            ID = Symbols.Count,
            Name = SanitizeName(name),
            Address = address,
            SegmentID = GetSegment(address).ID,
            Size = size,
        });
    }

    string SanitizeName(string name)
    {
        return name.Replace(':', '_').Replace('$', '@');
    }

    SegmentInfo GetSegment(int address)
    {
        foreach (SegmentInfo segment in Segments)
        {
            if (address >= segment.Start && address < segment.Start + segment.Size)
            {
                return segment;
            }
        }

        throw new Exception("Address not in known segment.");
    }

    public void Save(string path)
    {
        List<string> lines = new List<string>();
        lines.Add(string.Format("version\tmajor=2,minor=0"));
        lines.Add(string.Format(
            "info\tcsym={0},file={1},lib={2},line={3},mod={4},scope={5},seg={6},span={7},sym={8},type={9}", 
            0, 0, 0, 0, 0, 0, Segments.Count, 0, Symbols.Count, 0));

        foreach (SegmentInfo segment in Segments)
        {
            lines.Add(string.Format("seg\tid={0},start=0x{1:X4},size=0x{2:X4},type={3}{4}", segment.ID, segment.Start, segment.Size, segment.IsWritable ? "rw" : "ro", segment.IsPrgRom ? ",ooffs=16" : ""));
        }

        // ^sym\tid=([0-9]+),.*name=\"([0-9a-zA-Z@_-]+)\"(,.*size=([0-9]+)){0,1}(,.*def=([0-9+]+)){0,1}(,.*ref=([0-9+]+)){0,1}(,.*val=0x([0-9a-fA-F]+)){0,1}(,.*seg=([0-9]+)){0,1}(,.*exp=([0-9]+)){0,1}
        foreach (SymbolInfo symbol in Symbols)
        {
            lines.Add(string.Format("sym\tid={0},name=\"{1}\",size={2},val=0x{3:X4},seg={4}", symbol.ID, symbol.Name, symbol.Size, symbol.Address, symbol.SegmentID));
        }

        File.WriteAllLines(path, lines);
    }

    class SegmentInfo
    {
        public int ID;
        public int Start;
        public int Size;
        public int FileOffset;
        public bool IsWritable;
        public bool IsPrgRom;
    }

    class SymbolInfo
    {
        public int ID;
        public string Name;
        public int? Address;
        public int? SegmentID;
        public int? ExportSymbolID;
        public int? Size;
        public List<int> References;
        public List<int> Definitions;
    }
}
