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

    public static readonly string DebugOutputPath = "debug_output";

    static void Main(string[] args)
    {
        if (args.Length < 2) Error("usage: norcal src.c output.nes [--debug-output]");

        string sourcePath = args[0];
        string outputPath = args[1];
        EnableDebugOutput = args.Contains("--debug-output");

        List<Declaration> program = Parser.ParseFile(sourcePath);

        WriteDebugFile("stage0-parse.txt", ShowProgram(program));

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

    static string ShowProgram(List<Declaration> program)
    {
        StringBuilder sb = new StringBuilder();

        foreach (Declaration decl in program)
        {
            if (decl.Tag == DeclarationTag.Function)
            {
                sb.AppendFormat("{0}()\n", decl.Name);
                sb.AppendFormat(ShowExpr(decl.Body));
                sb.AppendFormat("\n\n");
            }
            else if (decl.Tag == DeclarationTag.Constant)
            {
                sb.AppendFormat("define {0} = ", decl.Name);
                sb.AppendFormat(ShowExpr(decl.Body));
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

    public static string ShowExpr(Expr e)
    {
        return ShowStringTree(ExprToStringTree(e));
    }

    /// <summary>
    /// The return value is a "string tree": a tree where each node is a string or another tree.
    /// </summary>
    static object ExprToStringTree(Expr e)
    {
        switch (e.Tag)
        {
            case ExprTag.Empty:
                return "$empty";
            case ExprTag.Int:
                return e.Int.ToString((e.Int < 512) ? "G" : "X");
            case ExprTag.Name:
                return e.Name;
            case ExprTag.Call:
                return Enumerable.Concat(new[] { e.Name }, e.Args.Select(ExprToStringTree));
            case ExprTag.Scope:
                return new[] { "$scope", ExprToStringTree(e.Args[0]) };
            case ExprTag.Sequence:
                return Enumerable.Concat(new[] { "$sequence" }, e.Args.Select(ExprToStringTree));
            case ExprTag.Local:
                return new[] { "$local", e.Name };
            case ExprTag.AddressOf:
                return new[] { "$address_of", ExprToStringTree(e.Args[0]) };
            case ExprTag.Switch:
                return Enumerable.Concat(new[] { "$switch" }, e.Args.Select(ExprToStringTree));
            case ExprTag.For:
                return Enumerable.Concat(new[] { "$for" }, e.Args.Select(ExprToStringTree));
            case ExprTag.Return:
                return new[] { "$return", ExprToStringTree(e.Args[0]) };
            case ExprTag.Cast:
                return new[] { "$cast", "<" + e.DeclaredType.Show() + ">", ExprToStringTree(e.Args[0]) };
            case ExprTag.StructCast:
                return new[] { "$struct_cast", ExprToStringTree(e.Args[0]), e.Name, ExprToStringTree(e.Args[1]) };
            case ExprTag.OffsetOf:
                return new[] { "$offset_of", ExprToStringTree(e.Args[0]), e.Name };
            default:
                throw new NotImplementedException();
        }
    }

    /// <summary>
    /// Format a "string tree" into a multi-line, indented string.
    /// Each node must be a string or another "string tree".
    /// </summary>
    static string ShowStringTree(object tree)
    {
        if (tree is string) return (string)tree;

        IEnumerable<object> subtrees = (IEnumerable<object>)tree;
        IEnumerable<string> substrings = subtrees.Select(ShowStringTree);
        bool small = subtrees.Count() <= 2;
        if (small)
        {
            return "(" + string.Join(" ", substrings) + ")";
        }
        else
        {
            return "(" + string.Join("\n    ", substrings.Select(s => s.Replace("\n", "\n    "))) + ")";
        }
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
}

[DebuggerDisplay("{DebuggerDisplay,nq}")]
partial class Expr
{
    string DebuggerDisplay => Show();

    public string Show()
    {
        switch (Tag)
        {
            case ExprTag.Empty:
                return "$empty";
            case ExprTag.Int:
                return Int.ToString((Int < 512) ? "G" : "X");
            case ExprTag.Name:
                return Name;
            case ExprTag.Call:
                return string.Format("({0} {1})", Name, string.Join(" ", Args.Select(x => x.Show())));
            case ExprTag.Scope:
                return string.Format("($scope {0})", Args[0].Show());
            case ExprTag.Sequence:
                return string.Format("($sequence {0})", string.Join(" ", Args.Select(x => x.DebuggerDisplay)));
            case ExprTag.Local:
                return string.Format("($local {0})", Name);
            case ExprTag.AddressOf:
                return string.Format("($address_of {0})", Args[0].Show());
            case ExprTag.Switch:
                return string.Format("($switch {0})", string.Join(" ", Args.Select(x => x.Show())));
            case ExprTag.For:
                return string.Format("($for {0})", string.Join(" ", Args.Select(x => x.Show())));
            case ExprTag.Return:
                return string.Format("($return {0})", Args[0].Show());
            case ExprTag.Cast:
                return string.Format("($cast {0} {1})", DeclaredType.Show(), Args[0].Show());
            case ExprTag.StructCast:
                return string.Format("($struct_cast {0} {1} {2})", Args[0].Show(), Name, Args[1].Show());
            case ExprTag.OffsetOf:
                return string.Format("($offset_of {0} {1})", Args[0].Show(), Name);
            default:
                throw new NotImplementedException();
        }
    }

    public static Expr MakeEmpty()
    {
        return new Expr
        {
            Tag = ExprTag.Empty,
        };
    }

    public static Expr MakeInt(int n, CType type)
    {
        return new Expr
        {
            Tag = ExprTag.Int,
            Int = n,
            DeclaredType = type,
        };
    }

    public static Expr MakeName(string name)
    {
        return new Expr
        {
            Tag = ExprTag.Name,
            Name = name,
        };
    }

    public static Expr MakeScope(Expr arg)
    {
        return new Expr
        {
            Tag = ExprTag.Scope,
            Args = new[] { arg },
        };
    }

    public static Expr MakeSequence(Expr[] statements)
    {
        return new Expr
        {
            Tag = ExprTag.Sequence,
            Args = statements,
        };
    }

    public static Expr MakeLocal(CType declaredType, string name)
    {
        return new Expr
        {
            Tag = ExprTag.Local,
            DeclaredType = declaredType,
            Name = name,
        };
    }

    public static Expr MakeAddressOf(Expr arg)
    {
        return new Expr
        {
            Tag = ExprTag.AddressOf,
            Args = new[] { arg },
        };
    }

    public static Expr MakeSwitch(Expr condition, Expr body) => MakeSwitch(new[] { condition, body });

    public static Expr MakeSwitch(Expr[] args)
    {
        return new Expr
        {
            Tag = ExprTag.Switch,
            Args = args,
        };
    }

    public static Expr MakeFor(Expr initialization, Expr condition, Expr induction, Expr body)
    {
        return new Expr
        {
            Tag = ExprTag.For,
            Args = new[] { initialization, condition, induction, body },
        };
    }

    public static Expr MakeReturn(Expr arg)
    {
        return new Expr
        {
            Tag = ExprTag.Return,
            Args = new[] { arg },
        };
    }

    public static Expr MakeCast(CType type, Expr expr)
    {
        return new Expr
        {
            Tag = ExprTag.Cast,
            DeclaredType = type,
            Args = new[] { expr },
        };
    }

    public static Expr MakeStructCast(Expr structExpr, string fieldName, Expr addressExpr)
    {
        return new Expr
        {
            Tag = ExprTag.StructCast,
            Args = new[] { structExpr, addressExpr },
            Name = fieldName,
        };
    }

    public static Expr MakeOffsetOf(Expr structExpr, string fieldName)
    {
        return new Expr
        {
            Tag = ExprTag.OffsetOf,
            Args = new[] { structExpr },
            Name = fieldName,
        };
    }

    public static Expr MakeCall(string function, IEnumerable<Expr> args)
    {
        Expr e = new Expr();
        e.Tag = ExprTag.Call;
        e.Name = function;
        e.Args = args.ToArray();
        return e;
    }

    public static Expr MakeCall(string function, Expr arg)
    {
        return MakeCall(function, new Expr[] { arg });
    }

    public static Expr MakeCall(string function, Expr left, Expr right)
    {
        return MakeCall(function, new Expr[] { left, right });
    }

    public bool MatchInt(out int n)
    {
        if (Tag == ExprTag.Int)
        {
            n = Int;
            return true;
        }
        else
        {
            n = 0;
            return false;
        }
    }

    public bool MatchName(out string name)
    {
        if (Tag == ExprTag.Name)
        {
            name = Name;
            return true;
        }
        else
        {
            name = null;
            return false;
        }
    }

    public bool MatchUnaryCall(string function, out Expr arg)
    {
        if (Tag == ExprTag.Call && Args.Length == 1 && Name == function)
        {
            arg = Args[0];
            return true;
        }
        else
        {
            arg = null;
            return false;
        }
    }
}

enum ExprTag
{
    Empty,      // nothing
    Int,        // number
    Name,       // name
    Scope,      // args...
    Sequence,   // args...
    Local,      // name
    AddressOf,  // arg
    Switch,     // cond, body, repeat...
    For,        // init, cond, next, body
    Return,     // arg
    Cast,       // type, expr
    StructCast, // structExpr, fieldName, addressExpr
    OffsetOf,   // structExpr, fieldName
    Call,       // name, args...
}

partial class Expr
{
    public ExprTag Tag;
    public int Int;
    public string Name;
    public CType DeclaredType;
    public Expr[] Args;
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
/// Names of special builtin functions.
/// </summary>
static class Builtins
{
    public static readonly string AddGeneric = "$add_gen";
    public static readonly string AddU8 = "$add_u8";
    public static readonly string AddU8Ptr = "$add_u8_ptr";
    public static readonly string AddU16 = "$add_u16";
    public static readonly string SubtractGeneric = "$sub_gen";
    public static readonly string SubtractU8 = "$sub_u8";
    public static readonly string SubtractU16 = "$sub_u16";
    public static readonly string LoadGeneric = "$load_gen";
    public static readonly string LoadU8 = "$load_u8";
    public static readonly string LoadU16 = "$load_u16";
    public static readonly string StoreGeneric = "$store_gen";
    public static readonly string StoreU8 = "$store_u8";
    public static readonly string StoreU16 = "$store_u16";
    public static readonly string BoolFromGeneric = "$bool_from_gen";
    public static readonly string BoolFromU16 = "$bool_from_u16";
    public static readonly string PredecrementGeneric = "$predecr_gen";
    public static readonly string PredecrementU8 = "$predecr_u8";
    public static readonly string PredecrementU16 = "$predecr_u16";
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
