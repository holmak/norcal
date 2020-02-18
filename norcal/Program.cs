using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

/// <summary>
/// Global compile-time configuration options.
/// </summary>
public static class Config
{
    public static readonly bool ShowSourcePositionInDebugOutput = false;
}

static class Program
{
    public static bool EnableDebugOutput { get; private set; } = false;
    private static bool AttachDebuggerOnError = false;

    public static readonly string DebugOutputPath = "debug_output";
    public static readonly string NamespaceSeparator = ":";

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
            Console.Error.WriteLine("usage: norcal first.c second.c ... [-o out.nes] [--debug-output]");
            Exit(1);
        }

        if (sourceFilenames.Count == 0)
        {
            Error("error: no source files provided");
        }

        try
        {
            IReadOnlyList<Expr> stackCode = Parser.ParseFiles(sourceFilenames);
            if (EnableDebugOutput) WritePassOutputToFile("virtual_stack_code", ShowAssembly(stackCode));
            IReadOnlyList<Expr> assembly = CodeGenerator.Convert(stackCode);
            if (EnableDebugOutput) WritePassOutputToFile("assembly_code", ShowAssembly(assembly));
            Assembler.Assemble(assembly, outputFilename);

            if (EnableDebugOutput)
            {
                Disassembler.Disassemble(outputFilename);
            }
        }
        catch (Exception ex)
        {
            Panic(ex.Message);
        }

        Exit(0);
    }

    public static void WriteDebugFile(string filename, string text)
    {
        if (EnableDebugOutput)
        {
            Directory.CreateDirectory(DebugOutputPath);
            File.WriteAllText(Path.Combine(DebugOutputPath, filename), text);
        }
    }

    public static void WritePassOutputToFile(string passName, string output)
    {
        WriteDebugFile(string.Format("{0}.txt", passName), output);
    }

    static string ShowAssembly(IReadOnlyList<Expr> assembly)
    {
        StringBuilder sb = new StringBuilder();
        foreach (Expr e in assembly)
        {
            string line = "";
            string mnemonic, text;
            AsmOperand operand;

            // Put a blank line before top-level constructs.
            // Indent most other things.
            bool isTopLevel = e.MatchTag(Tag.Function) || e.MatchTag(Tag.Constant);
            if (isTopLevel) sb.AppendLine();
            if (!isTopLevel && !e.MatchTag(Tag.Label)) line = "\t";

            if (Config.ShowSourcePositionInDebugOutput)
            {
                sb.AppendLine(line + "\t<" + e.Source + ">");
            }

            if (e.Match(Tag.Comment, out text)) line += "; " + text;
            else if (e.Match(Tag.Label, out text)) line += text + ":";
            else if (e.Match(Tag.Function, out text)) line += string.Format("function {0}:", text);
            else if (e.MatchTag(Tag.Function)) line += e.Show();
            else if (e.Match(Tag.Asm, out mnemonic, out operand)) line += FormatAssembly(mnemonic, operand);
            else line += e.Show();

            sb.AppendLine(line);
        }
        return sb.ToString();
    }

    static string FormatAssembly(string mnemonic, AsmOperand operand)
    {
        string format = (operand.Mode == AddressMode.Implicit) ? "{0}" : "{0} {1}";
        return string.Format(format, mnemonic, operand.Show());
    }

    public static string FormatAssemblyInteger(int n)
    {
        if (n < 256) return n.ToString();
        else return string.Format("${0:X}", n);
    }

    public static void GeneralError(Severity severity, string format, params object[] args)
    {
        GeneralError(severity, Maybe.Nothing, format, args);
    }

    [DebuggerStepThrough]
    public static void Warning(string format, params object[] args)
    {
        GeneralError(Severity.Warning, Maybe.Nothing, format, args);
    }

    [DebuggerStepThrough]
    public static void Warning(Maybe<FilePosition> position, string format, params object[] args)
    {
        GeneralError(Severity.Warning, position, format, args);
    }

    [DebuggerStepThrough]
    public static void Error(string format, params object[] args)
    {
        GeneralError(Severity.Error, Maybe.Nothing, format, args);
    }

    [DebuggerStepThrough]
    public static void Error(Maybe<FilePosition> position, string format, params object[] args)
    {
        GeneralError(Severity.Error, position, format, args);
    }

    [DebuggerStepThrough]
    public static void Panic(string format, params object[] args)
    {
        GeneralError(Severity.InternalError, Maybe.Nothing, format, args);
    }

    [DebuggerStepThrough]
    public static void Panic(Maybe<FilePosition> position, string format, params object[] args)
    {
        GeneralError(Severity.InternalError, position, format, args);
    }

    [DebuggerStepThrough]
    public static void GeneralError(Severity severity, Maybe<FilePosition> position, string format, params object[] args)
    {
        string prefix;
        if (position.HasValue)
        {
            prefix = string.Format("{0} {1}: ", position.Value.ToString(), SeverityText[severity]);
        }
        else
        {
            prefix = string.Format("{0}: ", SeverityText[severity]);
        }

        string message = string.Format(format, args);
        Console.Error.WriteLine(prefix + message);

        if (severity == Severity.Error)
        {
            Exit(1);
        }
        else if (severity == Severity.InternalError)
        {
            Exit(2);
        }
    }

    [DebuggerStepThrough]
    static void Exit(int code)
    {
        if (code != 0)
        {
            if (AttachDebuggerOnError)
            {
                Debugger.Launch();
            }

            if (Debugger.IsAttached)
            {
                Debugger.Break();
            }
        }

        Environment.Exit(code);
    }

    [DebuggerStepThrough]
    public static void NYI() => GeneralError(Severity.InternalError, Maybe.Nothing, "not yet implemented");

    [DebuggerStepThrough]
    public static void UnhandledCase() => GeneralError(Severity.InternalError, Maybe.Nothing, "unhandled case");

    static readonly Dictionary<Severity, string> SeverityText = new Dictionary<Severity, string>
    {
        { Severity.Warning, "warning" },
        { Severity.Error, "error" },
        { Severity.InternalError, "internal error" },
    };
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
    public static readonly string ReadonlyData = "$readonly_data";
    public static readonly string Struct = "$struct";

    // Special nodes:
    public static readonly string NoOperation = "$nop";
    public static readonly string AddressOf = "$address_of";
    public static readonly string Return = "$return";
    public static readonly string ReturnVoid = "$return_void";
    public static readonly string Cast = "$cast";
    public static readonly string Field = "$field";
    public static readonly string Continue = "$continue";
    public static readonly string Break = "$break";
    public static readonly string Call = "$call";

    // Intrinsic functions:
    public static readonly string Add = "$add";
    public static readonly string Subtract = "$sub";
    public static readonly string Multiply = "$mul";
    public static readonly string Divide = "$div";
    public static readonly string Modulus = "$mod";
    public static readonly string Load = "$load";
    public static readonly string LoadNondestructive = "$load_nondestructive";
    public static readonly string Store = "$store";
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
    public static readonly string PreIncrement = "$pre_increment";
    public static readonly string PostIncrement = "$post_increment";
    public static readonly string PreDecrement = "$pre_decrement";
    public static readonly string PostDecrement = "$post_decrement";

    // Virtual stack machine instructions:
    public static readonly string PushImmediate = "$push";
    public static readonly string PushVariableAddress = "$pushva";
    public static readonly string DropFinal = "$drop_final";
    public static readonly string Drop = "$drop";
    public static readonly string Materialize = "$materialize";

    // Assembly directives:
    public static readonly string Asm = "$asm";
    public static readonly string Label = "$label";
    public static readonly string Jump = "$jump";
    public static readonly string JumpIfTrue = "$jump_if_true";
    public static readonly string JumpIfFalse = "$jump_if_false";
    public static readonly string Comment = "$comment";
    public static readonly string SkipTo = "$skip_to";
    public static readonly string Word = "$word";
}

enum Severity
{
    Warning,
    Error,
    InternalError,
}
