using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

static class Program
{
    public static bool EnableDebugOutput { get; private set; } = false;
    private static bool AttachDebuggerOnError = false;
    private static int NextPassNumber = 0;

    public static readonly string DebugOutputPath = "debug_output";

    static void Main(string[] argsArray)
    {
        Queue<string> args = new Queue<string>(argsArray);

        List<string> sourceFilenames = new List<string>();
        string outputFilename = "out.nes";
        bool help = (args.Count == 0);

        while (args.Count > 0)
        {
            string arg = args.Dequeue();
            if (arg == "-?" || arg == "-h" || arg == "--help")
            {
                help = true;
            }
            else if (arg == "--debug-output")
            {
                EnableDebugOutput = true;
            }
            else if (arg == "--attach")
            {
                AttachDebuggerOnError = true;
            }
            else if (arg == "-o")
            {
                if (args.Count > 0) outputFilename = args.Dequeue();
                else Error("error: -o option requires a filename");
            }
            else if (arg.StartsWith("-"))
            {
                Error("error: unknown option: " + arg);
            }
            else
            {
                sourceFilenames.Add(arg);
            }
        }

        if (help)
        {
            Error("usage: norcal first.c second.c ... [-o out.nes] [--debug-output]");
        }

        if (sourceFilenames.Count == 0)
        {
            Error("error: no source files provided");
        }

        List<Expr> program = new List<Expr>();
        foreach (string file in sourceFilenames)
        {
            program.AddRange(Parser.ParseFile(file));
        }
        WritePassOutputToFile("parse", program);

        List<Expr> stackCode = Compiler.Compile(program);
        if (EnableDebugOutput) WritePassOutputToFile("stack-code", ShowAssembly(stackCode));
        List<Expr> assembly = StackAssembler.Convert(stackCode);
        if (EnableDebugOutput) WritePassOutputToFile("assembly", ShowAssembly(assembly));
        Assembler.Assemble(assembly, outputFilename);

        if (EnableDebugOutput)
        {
            Disassembler.Disassemble(outputFilename);
        }
    }

    public static void WriteDebugFile(string filename, string text)
    {
        if (EnableDebugOutput)
        {
            Directory.CreateDirectory(DebugOutputPath);
            File.WriteAllText(Path.Combine(DebugOutputPath, filename), text);
        }
    }

    public static void WritePassOutputToFile(string passName, List<Expr> program)
    {
        WritePassOutputToFile(passName, ShowProgram(program));
    }

    public static void WritePassOutputToFile(string passName, string output)
    {
        WriteDebugFile(string.Format("pass{0}-{1}.txt", NextPassNumber, passName), output);
        NextPassNumber += 1;
    }

    static string ShowProgram(List<Expr> program)
    {
        StringBuilder sb = new StringBuilder();
        foreach (Expr decl in program)
        {
            sb.Append(decl.ShowMultiline());
            sb.AppendLine();
            sb.AppendLine();
        }
        return sb.ToString();
    }

    static string ShowAssembly(List<Expr> assembly)
    {
        StringBuilder sb = new StringBuilder();
        foreach (Expr e in assembly)
        {
            string line = "";
            string mnemonic, text, mode;
            AsmOperand operand;

            // Put a blank line before top-level constructs.
            // Indent most other things.
            bool isTopLevel = e.MatchTag(Tag.Function) || e.MatchTag(Tag.Constant);
            if (isTopLevel) sb.AppendLine();
            if (!isTopLevel && !e.MatchTag(Tag.Label)) line = "\t";

            if (e.Match(Tag.Comment, out text)) line += "; " + text;
            else if (e.Match(Tag.Label, out text)) line += text + ":";
            else if (e.Match(Tag.Function, out text)) line += string.Format("function {0}:", text);
            else if (e.MatchTag(Tag.Function)) line += e.Show();
            else if (e.Match(Tag.Asm, out mnemonic, out operand, out mode)) line += FormatAssembly(mnemonic, operand, mode);
            else line += e.Show();

            sb.AppendLine(line);
        }
        return sb.ToString();
    }

    static string FormatAssembly(string mnemonic, AsmOperand operand, string mode)
    {
        string format;
        if (mode == Tag.Implicit) format = "{0}";
        else if (mode == Tag.Absolute) format = "{0} {1}";
        else if (mode == Tag.Relative) format = "{0} +{1}";
        else if (mode == Tag.Immediate) format = "{0} #{1}";
        else if (mode == Tag.IndirectY) format = "{0} ({1}),Y";
        else format = "{0} {1} ???";
        return string.Format(format, mnemonic, operand.Show());
    }

    public static string FormatAssemblyInteger(int n)
    {
        if (n < 256) return n.ToString();
        else return string.Format("${0:X}", n);
    }

    [DebuggerStepThrough]
    public static void Warning(string format, params object[] args)
    {
        string message = string.Format(format, args);
        Console.Error.WriteLine(message);
    }

    [DebuggerStepThrough]
    public static void Error(string format, params object[] args)
    {
        string message = string.Format(format, args);
        Console.Error.WriteLine(message);
        Exit(1);
    }

    [DebuggerStepThrough]
    public static void Panic(string format, params object[] args)
    {
        string message = string.Format(format, args);
        Console.Error.WriteLine(message);
        Exit(2);
    }

    [DebuggerStepThrough]
    static void Exit(int code)
    {
        if (AttachDebuggerOnError)
        {
            Debugger.Launch();
        }

        Debugger.Break();

        Environment.Exit(code);
    }

    [DebuggerStepThrough]
    public static void NYI() => Panic("not yet implemented");

    [DebuggerStepThrough]
    public static void UnhandledCase() => Panic("unhandled case");
}

class FieldInfo
{
    public readonly MemoryRegion Region;
    public readonly CType Type;
    public readonly string Name;

    public FieldInfo(MemoryRegion region, CType type, string name)
    {
        Region = region;
        Type = type;
        Name = name;
    }
}

/// <summary>
/// Names of AST nodes.
/// </summary>
static class Tag
{
    // Top-level declarations:
    public static readonly string Function = "$function";
    public static readonly string Constant = "$constant";
    public static readonly string Variable = "$variable";
    public static readonly string Struct = "$struct";

    // Special nodes:
    public static readonly string Empty = "$empty";
    public static readonly string Int = "$int";
    public static readonly string Name = "$name";
    public static readonly string Scope = "$scope";
    public static readonly string Sequence = "$sequence";
    public static readonly string AddressOf = "$address_of";
    public static readonly string Switch = "$switch";
    public static readonly string For = "$for";
    public static readonly string Return = "$return";
    public static readonly string Cast = "$cast";
    public static readonly string Field = "$field";
    public static readonly string Index = "$index";
    public static readonly string Label = "$label";
    public static readonly string Jump = "$jump";
    public static readonly string JumpIfTrue = "$jump_if_true";
    public static readonly string JumpIfFalse = "$jump_if_false";
    public static readonly string Continue = "$continue";
    public static readonly string Break = "$break";
    public static readonly string Asm = "$asm";

    // Intrinsic functions:
    public static readonly string Add = "$add";
    public static readonly string Subtract = "$sub";
    public static readonly string Multiply = "$mul";
    public static readonly string Divide = "$div";
    public static readonly string Modulus = "$mod";
    public static readonly string Load = "$load";
    public static readonly string Store = "$store";
    public static readonly string Predecrement = "$predec";
    public static readonly string Postdecrement = "$postdec";
    public static readonly string Preincrement = "$preinc";
    public static readonly string Postincrement = "$postinc";
    public static readonly string Equal = "$equal";
    public static readonly string NotEqual = "$not_equal";
    public static readonly string LessThan = "$less_than";
    public static readonly string LessThanOrEqual = "$less_than_or_equal";
    public static readonly string GreaterThan = "$greater_than";
    public static readonly string GreaterThanOrEqual = "$greater_than_or_equal";
    public static readonly string BitwiseAnd = "$bitwise_and";
    public static readonly string BitwiseOr = "$bitwise_or";
    public static readonly string BitwiseXor = "$bitwise_xor";
    public static readonly string BitwiseNot = "$bitwise_not";
    public static readonly string ShiftLeft = "$shift_left";
    public static readonly string ShiftRight = "$shift_right";
    public static readonly string LogicalNot = "$logical_not";

    // Virtual stack machine instructions:
    public static readonly string BeginScope = "$begin_scope";
    public static readonly string EndScope = "$end_scope";
    public static readonly string PushImmediate = "$push";
    public static readonly string PushVariable = "$pushv";
    public static readonly string PushAddressOfVariable = "$pushav";
    public static readonly string PushFieldName = "$pushfn";

    // Assembly directives:
    public static readonly string Comment = "$comment";
    public static readonly string SkipTo = "$skip_to";
    public static readonly string Word = "$word";

    // 6502 address modes:
    public static readonly string Implicit = "$implicit";
    public static readonly string Absolute = "$absolute";
    public static readonly string Immediate = "$immediate";
    public static readonly string IndirectY = "$indirect_y";
    public static readonly string Relative = "$relative";
}
