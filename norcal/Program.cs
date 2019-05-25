using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

static class Program
{
    static void Main(string[] args)
    {
        if (args.Length != 2) Error("usage: norcal src.c output.nes");

        string sourcePath = args[0];
        string outputPath = args[1];

        List<Declaration> program = Parser.ParseFile(sourcePath);

        Console.WriteLine("Parser output:");
        PrintProgram(program);
        Console.WriteLine();

        Compiler compiler = new Compiler();
        compiler.CompileProgram(program);
        compiler.WriteImage(outputPath);
        Disassembler.Disassemble(outputPath);
    }

    static void PrintProgram(List<Declaration> program)
    {
        foreach (Declaration decl in program)
        {
            if (decl.Kind == DeclarationKind.Function)
            {
                Console.Write("{0}()\n    ", decl.Name);
                PrintExpr(decl.Body);
                Console.Write("\n\n");
            }
            else if (decl.Kind == DeclarationKind.Constant)
            {
                Console.Write("define {0} = ", decl.Name);
                PrintExpr(decl.Body);
                Console.Write("\n\n");
            }
            else
            {
                Panic("unhandled declaration type");
            }
        }
    }

    static void PrintExpr(Expr e)
    {
        int n;
        string name;
        if (e.MatchInt(out n))
        {
            // Guess whether the number should be printed in hex or in decimal:
            string format = (n < 512) ? "{0}" : "{0:X}";
            Console.Write(format, n);
        }
        else if (e.MatchName(out name))
        {
            Console.Write(name);
        }
        else
        {
            Console.Write("(");
            PrintExpr(e.Function);
            Console.Write(" ");
            for (int i = 0; i < e.Args.Length; i++)
            {
                PrintExpr(e.Args[i]);
                Console.Write((i < e.Args.Length - 1) ? " " : ")");
            }
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
        Environment.Exit(1);
    }

    public static void NYI() => Panic("not yet implemented");
}

[DebuggerDisplay("{DebuggerDisplay,nq}")]
partial class Expr
{
    string DebuggerDisplay
    {
        get
        {
            if (Type == ExprType.Int) return Int.ToString();
            else if (Type == ExprType.Name) return Name;
            else return string.Format("({0} ... )", Function.DebuggerDisplay);
        }
    }

    public static Expr MakeInt(int n)
    {
        Expr e = new Expr();
        e.Type = ExprType.Int;
        e.Int = n;
        return e;
    }

    public static Expr MakeName(string name)
    {
        Expr e = new Expr();
        e.Type = ExprType.Name;
        e.Name = name;
        return e;
    }

    public static Expr MakeCall(Expr function, IEnumerable<Expr> args)
    {
        Expr e = new Expr();
        e.Type = ExprType.Call;
        e.Function = function;
        e.Args = args.ToArray();
        return e;
    }

    public static Expr MakeCall(Expr func, Expr arg)
    {
        return MakeCall(func, new Expr[] { arg });
    }

    public static Expr MakeCall(Expr func, Expr left, Expr right)
    {
        return MakeCall(func, new Expr[] { left, right });
    }

    public static Expr MakeLoad(int address)
    {
        return MakeCall(MakeName("$load"), MakeInt(address));
    }

    public bool MatchInt(out int n)
    {
        if (Type == ExprType.Int)
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
        if (Type == ExprType.Name)
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
        if (Type == ExprType.Call && Function.Type == ExprType.Name && Function.Name == function && Args.Length == 1)
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

enum ExprType
{
    Int,
    Name,
    Call,
}

partial class Expr
{
    public ExprType Type;
    public int Int;
    public string Name;
    public Expr Function;
    public Expr[] Args;
}

enum DeclarationKind
{
    Function,
    Constant,
}

class Declaration
{
    public DeclarationKind Kind;
    public string TypeName;
    public string Name;
    public Expr Body;
    public List<NamedField> Fields;
}

class NamedField
{
    public string TypeName;
    public string Name;

    public NamedField(string typename, string name)
    {
        TypeName = typename;
        Name = name;
    }
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
