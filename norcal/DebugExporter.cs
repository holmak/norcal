using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

class DebugExporter
{
    List<SegmentInfo> Segments = new List<SegmentInfo>();
    List<SpanInfo> Spans = new List<SpanInfo>();
    List<SymbolInfo> Symbols = new List<SymbolInfo>();
    List<LineInfo> Lines = new List<LineInfo>();
    List<string> Comments = new List<string>();

    public DebugExporter()
    {
        // HACK: This is hard-coded to match the values hard-coded in Assembler.
        AddSegment(0x0000, 0x0800, false);
        AddSegment(0x2000, 0x0008, false, false);
        AddSegment(0x4000, 0x0018, false, false);
        AddSegment(0x6000, 0x0006, false, false);
        AddSegment(0x8000, 0x8000, true);
    }

    void AddSegment(int address, int size, bool isPrgRom, bool export = true)
    {
        Segments.Add(new SegmentInfo()
        {
            ID = Segments.Count,
            Start = address,
            Size = size,
            IsPrgRom = isPrgRom,
            Export = export,
        });
    }

    int AddSpan(int address, int size, bool isData)
    {
        SegmentInfo segment = GetSegment(address);
        int spanID = Spans.Count;

        Spans.Add(new SpanInfo()
        {
            ID = spanID,
            SegmentID = segment.ID,
            Offset = address - segment.Start,
            Size = size,
            IsData = isData,
        });

        return spanID;
    }

    public void AddVariable(string name, int address, int size)
    {
        AddSpan(address, size, true);

        Symbols.Add(new SymbolInfo()
        {
            ID = Symbols.Count,
            Name = SanitizeName(name),
            Address = address,
            SegmentID = GetSegment(address).ID,
            Size = size,
        });
    }

    public void AddFunction(string name, int address)
    {
        Symbols.Add(new SymbolInfo()
        {
            ID = Symbols.Count,
            Name = SanitizeName(name),
            Address = address,
            SegmentID = GetSegment(address).ID,
            Size = 1
        });
    }

    public void TagInstruction(int address, int size, List<string> localComments)
    {
        int spanID = AddSpan(address, size, false);

        if (localComments.Count > 0)
        {
            foreach (string comment in localComments)
            {
                Comments.Add("; " + comment);
            }

            Comments.Add("NOP");

            Lines.Add(new LineInfo()
            {
                ID = Lines.Count,
                FileID = 0,
                LineNumber = Comments.Count - 1,
                SpanID = spanID,
            });
        }
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

        Program.Panic("Address not in known segment: ${0:X4}", address);
        return null;
    }

    public void Save(string path)
    {
        // Purge segments that should not be exported, like NES register segments (which Mesen treats as errors):
        foreach (SegmentInfo segment in Segments.ToArray())
        {
            if (!segment.Export)
            {
                Segments.Remove(segment);
                Spans.RemoveAll(x => x.SegmentID == segment.ID);
                Symbols.RemoveAll(x => x.SegmentID == segment.ID);
            }
        }

        // Write the file contents:
        List<string> lines = new List<string>();
        lines.Add(string.Format("version\tmajor=2,minor=0"));
        lines.Add(string.Format(
            "info\tcsym={0},file={1},lib={2},line={3},mod={4},scope={5},seg={6},span={7},sym={8},type={9}", 
            0, 1, 0, 0, 0, 0, Segments.Count, Spans.Count, Symbols.Count, 0));
        lines.Add(string.Format("file\tid=0,name=\"{0}\"", Path.ChangeExtension(Path.GetFileName(path), ".dbc")));

        foreach (SegmentInfo segment in Segments)
        {
            lines.Add(string.Format("seg\tid={0},start=0x{1:X4},size=0x{2:X4},{3}", segment.ID, segment.Start, segment.Size, segment.IsPrgRom ? "type=ro,ooffs=16" : "type=rw"));
        }

        foreach (SpanInfo span in Spans)
        {
            lines.Add(string.Format("span\tid={0},seg={1},start={2},size={3}{4}", span.ID, span.SegmentID, span.Offset, span.Size, span.IsData ? ",type=0" : ""));
        }

        foreach (SymbolInfo symbol in Symbols)
        {
            lines.Add(string.Format("sym\tid={0},name=\"{1}\",size={2},val=0x{3:X4},seg={4}", symbol.ID, symbol.Name, symbol.Size, symbol.Address, symbol.SegmentID));
        }

        foreach (LineInfo line in Lines)
        {
            lines.Add(string.Format("line\tid={0},file={1},line={2},span={3}", line.ID, line.FileID, line.LineNumber, line.SpanID));
        }

        File.WriteAllLines(path, lines);

        // Write a separate file with comment text that our DBG file can reference:
        File.WriteAllLines(Path.ChangeExtension(path, ".dbc"), Comments);
    }

    class SegmentInfo
    {
        public int ID;
        public int Start;
        public int Size;
        public bool IsPrgRom;
        public bool Export;
    }

    class SpanInfo
    {
        public int ID;
        public int SegmentID;
        public int Offset;
        public int Size;
        public bool IsData;
    }

    class SymbolInfo
    {
        public int ID;
        public string Name;
        public int Address;
        public int SegmentID;
        public int Size;
    }

    class LineInfo
    {
        public int ID;
        public int FileID;
        public int LineNumber;
        public int SpanID;
    }
}
