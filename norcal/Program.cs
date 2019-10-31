using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

static class Program
{
    public static bool EnableDebugOutput { get; private set; } = false;
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

        List<Declaration> program = new List<Declaration>();
        foreach (string file in sourceFilenames)
        {
            program.AddRange(Parser.ParseFile(file));
        }
        WritePassOutputToFile("parse", program);

        Compiler compiler = new Compiler();
        compiler.CompileProgram(program);
        compiler.Assemble(outputFilename);

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

    public static void WritePassOutputToFile(string passName, List<Declaration> program)
    {
        WritePassOutputToFile(passName, ShowProgram(program));
    }

    public static void WritePassOutputToFile(string passName, string output)
    {
        WriteDebugFile(string.Format("pass{0}-{1}.txt", NextPassNumber, passName), output);
        NextPassNumber += 1;
    }

    static string ShowProgram(List<Declaration> program)
    {
        StringBuilder sb = new StringBuilder();

        foreach (Declaration decl in program)
        {
            if (decl.Tag == DeclarationTag.Function)
            {
                sb.AppendFormat("{0}()\n", decl.Name);
                sb.AppendFormat(decl.Body.ShowMultiline());
                sb.AppendFormat("\n\n");
            }
            else if (decl.Tag == DeclarationTag.Constant)
            {
                sb.AppendFormat("define {0} = ", decl.Name);
                sb.AppendFormat(decl.Body.ShowMultiline());
                sb.AppendFormat("\n\n");
            }
            else if (decl.Tag == DeclarationTag.Variable)
            {
                sb.AppendFormat("var {0};\n\n", decl.Name);
                sb.AppendFormat("\n\n");
            }
            else if (decl.Tag == DeclarationTag.Struct)
            {
                sb.AppendFormat("struct {0} {{ ... }}\n\n", decl.Name);
                // TODO: Print the fields.
            }
            else
            {
                Panic("unhandled declaration type");
            }
        }

        return sb.ToString();
    }

    public static void Warning(string format, params object[] args)
    {
        string message = string.Format(format, args);
        Console.Error.WriteLine(message);
    }

    public static void Error(string format, params object[] args)
    {
        string message = string.Format(format, args);
        Console.Error.WriteLine(message);
        Environment.Exit(1);
    }

    public static void Panic(string format, params object[] args)
    {
        string message = string.Format(format, args);
        Console.Error.WriteLine(message);
        Environment.Exit(2);
    }

    public static void NYI() => Panic("not yet implemented");

    public static void UnhandledCase() => Panic("unhandled case");
}

class Declaration
{
    public DeclarationTag Tag;
    public CType Type;
    public string Name;
    public Expr Body;
    public List<NamedField> Fields;
}

enum DeclarationTag
{
    Function,
    Constant,
    Variable,
    Struct,
}

class NamedField
{
    public CType Type;
    public string Name;

    public NamedField(CType type, string name)
    {
        Type = type;
        Name = name;
    }
}

/// <summary>
/// Names of AST nodes.
/// </summary>
static class Tag
{
    // Special nodes:
    public static readonly string Empty = "$empty";
    public static readonly string Int = "$int";
    public static readonly string Name = "$name";
    public static readonly string Scope = "$scope";
    public static readonly string Sequence = "$sequence";
    public static readonly string Local = "$local";
    public static readonly string AddressOf = "$address_of";
    public static readonly string Switch = "$switch";
    public static readonly string For = "$for";
    public static readonly string Return = "$return";
    public static readonly string Cast = "$cast";
    public static readonly string Field = "$field";
    public static readonly string Index = "$index";

    // Type-generic pseudo-functions:
    public static readonly string AddGeneric = "$add";
    public static readonly string SubtractGeneric = "$sub";
    public static readonly string LoadGeneric = "$load";
    public static readonly string StoreGeneric = "$store";
    public static readonly string BoolFromGeneric = "$bool_from";
    public static readonly string PredecrementGeneric = "$predecr";
    public static readonly string LessThanGeneric = "$less_than";
    public static readonly string GreaterThanGeneric = "$greater_than";
    public static readonly string BitwiseAndGeneric = "$bitwise_and";
    public static readonly string BitwiseOrGeneric = "$bitwise_or";
    public static readonly string BitwiseXorGeneric = "$bitwise_xor";
    public static readonly string BitwiseNotGeneric = "$bitwise_not";

    // Runtime functions:
    public static readonly string AddU8 = "_rt_add_u8";
    public static readonly string AddU8Ptr = "_rt_add_u8_ptr";
    public static readonly string AddU16 = "_rt_add_u16";
    public static readonly string SubtractU8 = "_rt_sub_u8";
    public static readonly string SubtractU16 = "_rt_sub_u16";
    public static readonly string MultiplyU16 = "_rt_mul_u16";
    public static readonly string LessThanU8 = "_rt_lt_u8";
    public static readonly string LessThanU16 = "_rt_lt_u16";
    public static readonly string GreaterThanU8 = "_rt_gt_u8";
    public static readonly string GreaterThanU16 = "_rt_gt_u16";
    public static readonly string LoadU8 = "_rt_load_u8";
    public static readonly string LoadU16 = "_rt_load_u16";
    public static readonly string StoreU8 = "_rt_store_u8";
    public static readonly string StoreU16 = "_rt_store_u16";
    public static readonly string BoolFromU16 = "_rt_bool_from_u16";
    public static readonly string PredecrementU8 = "_rt_predecr_u8";
    public static readonly string PredecrementU16 = "_rt_predecr_u16";
    public static readonly string BitwiseAndU8 = "_rt_bitwise_and_u8";
    public static readonly string BitwiseAndU16 = "_rt_bitwise_and_u16";
    public static readonly string BitwiseOrU8 = "_rt_bitwise_or_u8";
    public static readonly string BitwiseOrU16 = "_rt_bitwise_or_u16";
    public static readonly string BitwiseXorU8 = "_rt_bitwise_xor_u8";
    public static readonly string BitwiseXorU16 = "_rt_bitwise_xor_u16";
    public static readonly string BitwiseNotU8 = "_rt_bitwise_not_u8";
    public static readonly string BitwiseNotU16 = "_rt_bitwise_not_u16";
}
