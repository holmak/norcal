﻿using System;
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
            // TODO: Reimplement the pretty-printing code.
            Console.Write("(...)");
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
    string DebuggerDisplay
    {
        get
        {
            switch (Tag)
            {
                case ExprTag.Int:
                    return Int.ToString();
                case ExprTag.Name:
                    return Name;
                case ExprTag.Call:
                    return string.Format("({0} ...)", Name);
                case ExprTag.Scope:
                    return string.Format("($scope {0})", Args[0].DebuggerDisplay);
                case ExprTag.Sequence:
                    return string.Format("($sequence {0})", string.Join(" ", Args.Select(x => x.DebuggerDisplay)));
                case ExprTag.Local:
                    return string.Format("($local {0})", Name);
                case ExprTag.AddressOf:
                    return string.Format("($address_of {0})", Args[0].DebuggerDisplay);
                case ExprTag.Switch:
                    return string.Format("($switch ...)");
                case ExprTag.Return:
                    return string.Format("($return {0})", Args[0].DebuggerDisplay);
                default:
                    throw new NotImplementedException();
            }
        }
    }

    public static Expr MakeInt(int n)
    {
        return new Expr
        {
            Tag = ExprTag.Int,
            Int = n,
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

    public static Expr MakeLocal(string name)
    {
        return new Expr
        {
            Tag = ExprTag.Local,
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

    public static Expr MakeSwitch(Expr condition, Expr body)
    {
        return new Expr
        {
            Tag = ExprTag.Switch,
            Args = new[] { condition, body },
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
    Int,        // number
    Name,       // name
    Scope,      // args...
    Sequence,   // args...
    Local,      // name
    AddressOf,  // arg
    Switch,     // cond, body, repeat...
    Return,     // arg
    Call,       // name, args...
}

partial class Expr
{
    public ExprTag Tag;
    public int Int;
    public string Name;
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
    public CType Type;
    public string Name;
    public Expr Body;
    public List<NamedField> Fields;
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
