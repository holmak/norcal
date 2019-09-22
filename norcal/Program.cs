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

    static void Main(string[] args)
    {
        if (args.Length < 2) Error("usage: norcal src.c output.nes [--debug-output]");

        string sourcePath = args[0];
        string outputPath = args[1];
        EnableDebugOutput = args.Contains("--debug-output");

        List<Declaration> program = Parser.ParseFile(sourcePath);
        Program.WritePassOutputToFile("parse", ShowProgram(program));

        Compiler compiler = new Compiler();
        compiler.CompileProgram(program);
        compiler.WriteImage(outputPath);

        if (EnableDebugOutput)
        {
            Disassembler.Disassemble(outputPath);
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

    public static void WritePassOutputToFile(string passName, string s)
    {
        WriteDebugFile(string.Format("pass{0}-{1}.txt", NextPassNumber, passName), s);
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

// Expr is treated as a dynamically-typed tuple, with many variants:
// $empty
// $int number
// $name name
// $scope body
// $sequence exprs...
// $local declaredType name
// $addressOf expr
// $switch { condition body } (repeated any number of times)
// $for init cond next body
// $return expr
// $cast type expr
// $structCast structExpr fieldName addressExpr
// $offsetOf structExpr fieldName
// <functionName> args...
//
// The "type" property, if present, specifies the type determined by type-checking rules.

[DebuggerDisplay("{Show(),nq}")]
class Expr
{
    private readonly object[] Args;
    public readonly CType Type;

    public Expr(object[] args, CType type)
    {
        foreach (object arg in args)
        {
            if (arg == null) throw new Exception("Null in tuple.");
            if (!(arg is int || arg is string || arg is CType || arg is Expr))
            {
                throw new Exception("Unsupported type in tuple: " + arg.GetType());
            }
        }

        Args = args;
        Type = type;
    }

    public static Expr Make(params object[] args) => new Expr(args, CType.Implied);

    public bool MatchTag(string tag)
    {
        return (string)Args[0] == tag;
    }

    public bool Match(string tag)
    {
        if (Args.Length == 1 &&
            Args[0] is string && (string)Args[0] == tag)
        {
            return true;
        }

        return false;
    }

    public bool Match<T1>(string tag, out T1 var1)
    {
        if (Args.Length == 2 &&
            Args[0] is string && (string)Args[0] == tag &&
            Args[1] is T1)
        {
            var1 = (T1)Args[1];
            return true;
        }

        var1 = default(T1);
        return false;
    }

    public bool Match<T1, T2>(string tag, out T1 var1, out T2 var2)
    {
        if (Args.Length == 3 &&
            Args[0] is string && (string)Args[0] == tag &&
            Args[1] is T1 &&
            Args[2] is T2)
        {
            var1 = (T1)Args[1];
            var2 = (T2)Args[2];
            return true;
        }

        var1 = default(T1);
        var2 = default(T2);
        return false;
    }

    public bool Match<T1, T2, T3>(string tag, out T1 var1, out T2 var2, out T3 var3)
    {
        if (Args.Length == 4 &&
            Args[0] is string && (string)Args[0] == tag &&
            Args[1] is T1 &&
            Args[2] is T2 &&
            Args[3] is T3)
        {
            var1 = (T1)Args[1];
            var2 = (T2)Args[2];
            var3 = (T3)Args[3];
            return true;
        }

        var1 = default(T1);
        var2 = default(T2);
        var3 = default(T3);
        return false;
    }

    public bool Match<T1, T2, T3, T4>(string tag, out T1 var1, out T2 var2, out T3 var3, out T4 var4)
    {
        if (Args.Length == 5 &&
            Args[0] is string && (string)Args[0] == tag &&
            Args[1] is T1 &&
            Args[2] is T2 &&
            Args[3] is T3 &&
            Args[4] is T4)
        {
            var1 = (T1)Args[1];
            var2 = (T2)Args[2];
            var3 = (T3)Args[3];
            var4 = (T4)Args[4];
            return true;
        }

        var1 = default(T1);
        var2 = default(T2);
        var3 = default(T3);
        var4 = default(T4);
        return false;
    }

    public bool MatchAny<T>(string tag, out T[] vars)
    {
        if (Args.Length >= 1 &&
            Args[0] is string && (string)Args[0] == tag &&
            Args.Skip(1).All(x => x is T))
        {
            vars = Args.Skip(1).Cast<T>().ToArray();
            return true;
        }

        vars = null;
        return false;
    }

    public bool MatchAny<T>(out string tag, out T[] vars)
    {
        if (Args.Length >= 1 &&
            Args[0] is string &&
            Args.Skip(1).All(x => x is T))
        {
            tag = (string)Args[0];
            vars = Args.Skip(1).Cast<T>().ToArray();
            return true;
        }

        tag = null;
        vars = null;
        return false;
    }

    public Expr WithType(CType type)
    {
        return new Expr(Args, type);
    }

    /// <summary>
    /// Apply a function to this expression tree.
    /// </summary>
    public Expr Map(Func<Expr, Expr> f)
    {
        object[] results = new object[Args.Length];

        for (int i = 0; i < Args.Length; i++)
        {
            Expr subexpr = Args[i] as Expr;
            if (subexpr != null)
            {
                results[i] = f(subexpr);
            }
            else
            {
                // Return other elements unchanged:
                results[i] = Args[i];
            }
        }

        return Make(results).WithType(Type);
    }

    public string Show() => ShowWithOptions(false);

    public string ShowMultiline() => ShowWithOptions(true);

    string ShowWithOptions(bool multiline)
    {
        return ShowStringTree(multiline, ToStringTree());
    }

    /// <summary>
    /// The return value is a "string tree": a tree where each node is a string or an array of subtrees.
    /// </summary>
    object ToStringTree()
    {
        object[] tree = new object[Args.Length];

        for (int i = 0; i < Args.Length; i++)
        {
            int? integer = Args[i] as int?;
            string name = Args[i] as string;
            Expr subexpr = Args[i] as Expr;
            CType type = Args[i] as CType;
            if (integer != null)
            {
                int n = integer.Value;
                tree[i] = n.ToString((n < 512) ? "G" : "X");
            }
            else if (name != null)
            {
                tree[i] = name;
            }
            else if (subexpr != null)
            {
                tree[i] = subexpr.ToStringTree();
            }
            else if (type != null)
            {
                tree[i] = type.Show();
            }
            else
            {
                Program.NYI();
            }
        }

        return tree;
    }

    /// <summary>
    /// Format a "string tree" into a multi-line, indented string.
    /// Each node must be a string or another "string tree".
    /// </summary>
    static string ShowStringTree(bool multiline, object tree)
    {
        if (tree is string) return (string)tree;

        IEnumerable<object> subtrees = (IEnumerable<object>)tree;
        IEnumerable<string> substrings = subtrees.Select(x => ShowStringTree(multiline, x));
        bool small = subtrees.Count() <= 2;
        if (small || !multiline)
        {
            return "(" + string.Join(" ", substrings) + ")";
        }
        else
        {
            return "(" + string.Join("\n    ", substrings.Select(s => s.Replace("\n", "\n    "))) + ")";
        }
    }
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
    public static readonly string StructCast = "$struct_cast";
    public static readonly string OffsetOf = "$offset_of";

    // Type-generic pseudo-functions:
    public static readonly string AddGeneric = "$add_gen";
    public static readonly string SubtractGeneric = "$sub_gen";
    public static readonly string LoadGeneric = "$load_gen";
    public static readonly string StoreGeneric = "$store_gen";
    public static readonly string BoolFromGeneric = "$bool_from_gen";
    public static readonly string PredecrementGeneric = "$predecr_gen";

    // Runtime functions:
    public static readonly string AddU8 = "_rt_add_u8";
    public static readonly string AddU8Ptr = "_rt_add_u8_ptr";
    public static readonly string AddU16 = "_rt_add_u16";
    public static readonly string SubtractU8 = "_rt_sub_u8";
    public static readonly string SubtractU16 = "_rt_sub_u16";
    public static readonly string LoadU8 = "_rt_load_u8";
    public static readonly string LoadU16 = "_rt_load_u16";
    public static readonly string StoreU8 = "_rt_store_u8";
    public static readonly string StoreU16 = "_rt_store_u16";
    public static readonly string BoolFromU16 = "_rt_bool_from_u16";
    public static readonly string PredecrementU8 = "_rt_predecr_u8";
    public static readonly string PredecrementU16 = "_rt_predecr_u16";
}

enum Opcode
{
    ORA_ZP       = 0x05,
    ORA_IMM      = 0x09,
    ORA_ABS      = 0x0D,
    CLC          = 0x18,
    JSR          = 0x20,
    SEC          = 0x38,
    JMP_ABS      = 0x4C,
    RTS          = 0x60,
    ADC_ZP       = 0x65,
    ADC_ZP_X     = 0x75,
    STA_ZP       = 0x85,
    STX_ZP       = 0x86,
    DEY          = 0x88,
    STA_ABS      = 0x8D,
    STX_ABS      = 0x8E,
    STA_ZP_Y_IND = 0x91,
    STA_ZP_X     = 0x95,
    LDY_IMM      = 0xA0,
    LDA_ZP_X_IND = 0xA1,
    LDX_IMM      = 0xA2,
    LDA_ZP       = 0xA5,
    LDA_IMM      = 0xA9,
    TAX          = 0xAA,
    LDA_ABS      = 0xAD,
    LDX_ABS      = 0xAE,
    LDA_ZP_Y_IND = 0xB1,
    LDA_ZP_X     = 0xB5,
    INY          = 0xC8,
    DEX          = 0xCA,
    BNE          = 0xD0,
    SBC_ZP       = 0xE5,
    INX          = 0xE8,
    BEQ          = 0xF0,
    SBC_ZP_X     = 0xF5,
}
