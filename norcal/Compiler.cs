﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

partial class Compiler
{
    int RamNext = RamStart;
    int NextLabelNumber = 0;
    List<Symbol> Symbols = new List<Symbol>();
    List<Fixup> Fixups = new List<Fixup>();

    static readonly int RamStart = 0x300;
    static readonly int RamEnd = 0x800;

    static readonly int DestinationDiscard = -1;
    static readonly int DestinationAcc = -2;

    // Temporary pseudoregisters for intrinsic operations, such as arithmetic.
    // TODO: Once a symbol table is implemented, mark this space as allocated.
    static readonly int T0 = 0x00F0;
    static readonly int T1 = (T0 + 1);
    static readonly int T2 = (T0 + 2);
    static readonly int T3 = (T0 + 3);

    string MakeUniqueLabel()
    {
        return string.Format("@L{0}", NextLabelNumber++);
    }

    byte LowByte(int n)
    {
        return (byte)(n & 0xFF);
    }

    byte HighByte(int n)
    {
        return (byte)((n >> 8) & 0xFF);
    }

    static int SizeOf(CType type)
    {
        if (type.Tag == CTypeTag.Simple && type.SimpleType == CSimpleType.UInt16) return 2;
        else
        {
            Program.NYI();
            return 1;
        }
    }

    void DefineSymbol(SymbolKind kind, string name, int value, CType type)
    {
        // TODO: It is an error to define two things with the same name.

        Symbols.Add(new Symbol
        {
            Kind = kind,
            Name = name,
            Value = value,
            Type = type,
        });
    }

    bool FindSymbol(string name, out Symbol found)
    {
        foreach (Symbol sym in Symbols)
        {
            if (sym.Name == name)
            {
                found = sym;
                return true;
            }
        }

        found = null;
        return false;
    }

    // Allocate 'size' bytes in RAM and return the address.
    int AllocGlobal(int size)
    {
        if (RamNext + size > RamEnd) Program.Error("Not enough RAM to allocate global.");
        int address = RamNext;
        RamNext += size;
        return address;
    }

    void BeginTempScope()
    {
        // TODO
    }

    void EndTempScope()
    {
        // TODO
    }

    // TODO: Temporary variables can be freed when the current temp scope ends.
    int AllocTemp(int size)
    {
        return AllocGlobal(size);
    }

    // Return true if the expression was constant, and therefore able to be evaluated.
    bool EvaluateConstantExpression(Expr e, out int value)
    {
        // TODO: Evaluate more complex constant expressions, too.
        if (e.MatchInt(out value))
        {
            return true;
        }
        else if (e.Type == ExprType.Name)
        {
            Symbol sym;
            if (!FindSymbol(e.Name, out sym)) Program.Error("undefined symbol");

            if (sym.Kind == SymbolKind.Constant)
            {
                // TODO: Make sure the constant value is not too big.
                value = sym.Value;
                return true;
            }
        }

        value = 0;
        return false;
    }

    // Jump unconditionally.
    void EmitJump(string target)
    {
        Emit_U16(Opcode.JMP_ABS, 0);
        int fixupAddress = GetCurrentCodeAddress() - 2;
        Fixups.Add(new Fixup(FixupKind.Absolute, fixupAddress, target));
    }

    void EmitLoadImmediate(int imm, int dest, Continuation cont)
    {
        EmitComment("load immediate");
        if (dest == DestinationDiscard)
        {
            // NOP
        }
        else if (dest == DestinationAcc)
        {
            Emit_U8(Opcode.LDA_IMM, LowByte(imm));
            Emit_U8(Opcode.LDX_IMM, HighByte(imm));
        }
        else
        {
            Emit_U8(Opcode.LDA_IMM, LowByte(imm));
            Emit_U8(Opcode.LDX_IMM, HighByte(imm));
            Emit_U16(Opcode.STA_ABS, dest);
            Emit_U16(Opcode.STX_ABS, dest + 1);
        }

        if ((cont.When == JumpCondition.IfTrue && imm != 0) ||
            (cont.When == JumpCondition.IfFalse && imm == 0))
        {
            EmitJump(cont.Target);
        }
    }

    void EmitStoreAcc(int dest)
    {
        if (dest == DestinationDiscard || dest == DestinationAcc)
        {
            // NOP
        }
        else
        {
            Emit_U16(Opcode.STA_ABS, dest);
            Emit_U16(Opcode.STX_ABS, dest + 1);
        }
    }

    void EmitBranchOnAcc(Continuation cont)
    {
        if (cont.When != JumpCondition.Never)
        {
            // TODO: Values used as branch conditions must have a single-byte type, for easy comparison against zero.
            EmitComment("branch on ACC");
            Emit_U8(Opcode.ORA_IMM, 0);
            Opcode op = (cont.When == JumpCondition.IfTrue) ? Opcode.BNE : Opcode.BEQ;
            Emit_U8(op, 0);
            int fixupAddress = GetCurrentCodeAddress() - 1;
            Fixups.Add(new Fixup(FixupKind.Relative, fixupAddress, cont.Target));
        }
    }

    void EmitLoad(int address, int dest, Continuation cont)
    {
        // Even if the value is unused, always read the address; it might be a hardware register.
        Emit_U16(Opcode.LDA_ABS, address);
        Emit_U16(Opcode.LDX_ABS, address + 1);
        EmitStoreAcc(dest);
        EmitBranchOnAcc(cont);
    }

    // Fix up jumps that forward-referenced a label:
    void FixReferencesTo(string label)
    {
        int target = GetCurrentCodeAddress();
        foreach (Fixup fixup in Fixups)
        {
            if (fixup.Target == label)
            {
                if (fixup.Kind == FixupKind.Relative)
                {
                    EmitFix_S8(fixup.Location, target);
                    fixup.Kind = FixupKind.None;
                }
                else if (fixup.Kind == FixupKind.Absolute)
                {
                    EmitFix_U16(fixup.Location, target);
                    fixup.Kind = FixupKind.None;
                }
            }
        }
    }

    void CompileExpression(Expr e, int dest, Continuation cont)
    {
        int value;
        if (EvaluateConstantExpression(e, out value))
        {
            EmitLoadImmediate(value, dest, cont);
        }
        else if (e.Type == ExprType.Name)
        {
            Symbol sym;
            if (!FindSymbol(e.Name, out sym)) Program.Error("undefined symbol");
            if (sym.Kind != SymbolKind.Local) Program.NYI();
            int address = sym.Value;
            EmitLoad(address, dest, cont);
        }
        else if (e.Type == ExprType.Call)
        {
            string func;
            if (!e.Function.MatchName(out func)) Program.Panic("calling via function pointer is not yet implemented");
            int argCount = e.Args.Length;
            Expr[] args = e.Args;

            // TODO: Make sure that operators and functions are passed the correct number and type of arguments.

            // Handle certain functions as "intrinsics"; otherwise use the general function call mechanism.
            int addr;
            if (func == "$load" && EvaluateConstantExpression(args[0], out addr))
            {
                EmitLoad(addr, dest, cont);
            }
            else if (func == "$assign" && EvaluateConstantExpression(args[0], out addr))
            {
                if (dest == DestinationDiscard)
                {
                    EmitComment("assign to constant address");
                    // Let the sub-expression handle storing the data _and_ any conditional branch.
                    CompileExpression(args[1], addr, cont);
                }
                else
                {
                    EmitComment("assign to constant address, and produce the assigned value");
                    CompileExpression(args[1], DestinationAcc, Continuation.Fallthrough);
                    EmitStoreAcc(addr);
                    EmitStoreAcc(dest);
                    EmitBranchOnAcc(cont);
                }
            }
            else if (func == "$sequence")
            {
                for (int i = 0; i < args.Length; i++)
                {
                    Expr subexpr = args[i];
                    EmitComment("begin new statement");
                    // Drop the result of each expression except the last.
                    bool isLast = (i == args.Length - 1);
                    if (!isLast)
                    {
                        CompileExpression(subexpr, DestinationDiscard, Continuation.Fallthrough);
                    }
                    else
                    {
                        CompileExpression(subexpr, dest, cont);
                    }
                }
            }
            else if (func == "$local")
            {
                if (argCount != 1) Program.Panic("wrong number of args in local declaration");
                if (args[0].Type != ExprType.Name) Program.Panic("invalid local declaration node");
                int address = AllocGlobal(SizeOf(CType.UInt16));
                DefineSymbol(SymbolKind.Local, args[0].Name, address, CType.UInt16);
                if (dest != DestinationDiscard) Program.Panic("cannot store value of expression of type 'void'");
                if (cont.When != JumpCondition.Never) Program.Panic("cannot branch based on value of type 'void'");
            }
            else if (func == "$addr_of")
            {
                if (argCount != 1) Program.Panic("wrong number of args");
                if (args[0].Type == ExprType.Name)
                {
                    Symbol sym;
                    if (!FindSymbol(args[0].Name, out sym)) Program.Error("undefined symbol");
                    if (sym.Kind != SymbolKind.Local) Program.NYI();
                    int address = sym.Value;
                    EmitLoadImmediate(address, dest, cont);
                }
                else
                {
                    Program.NYI();
                }
            }
            else if (func == "$switch")
            {
                if (argCount != 2) Program.Panic("wrong number of items in switch expression");
                Expr test = args[0];
                Expr then = args[1];
                string end = MakeUniqueLabel();

                // TODO: Handle any number of clauses. For each clause:
                {
                    string nextClause = MakeUniqueLabel();
                    // If this clause's condition is false, try the next clause:
                    CompileExpression(test, DestinationDiscard, new Continuation(JumpCondition.IfFalse, nextClause));
                    // If the condition was true, execute the clause body:
                    CompileExpression(then, dest, cont);
                    // After executing the body of a clause, skip the rest of the clauses:
                    EmitJump(end);
                    EmitComment("end of switch clause");
                    FixReferencesTo(nextClause);
                }

                EmitComment("end of switch");
                FixReferencesTo(end);
            }
            else
            {
                // This is a non-intrinsic function call.

                BeginTempScope();

                // Evaluate the arguments and store the results in temporary variables.
                // TODO: (optimization) The first arg doesn't have to be simplified, since we haven't started assembling a call frame yet.
                // Optimization: Sufficiently simple arguments (such as literal ints) can skip this
                //   step and be written directly into the call frame.
                List<Expr> temps = new List<Expr>();
                foreach (Expr arg in e.Args)
                {
                    // TODO: The comment below is no longer accurate:
                    // When creating the list of "simple expressions", we can't reuse AST nodes, because
                    // they form their own list. Instead, create a new copy of the expression,
                    // unconnected to the rest of the AST.
                    int n;
                    Expr simpleArg;
                    if (EvaluateConstantExpression(arg, out n))
                    {
                        simpleArg = Expr.MakeInt(n);
                    }
                    else
                    {
                        int argSize = SizeOf(CType.UInt16);
                        int temp = AllocTemp(argSize);
                        CompileExpression(arg, temp, Continuation.Fallthrough);
                        simpleArg = Expr.MakeLoad(temp);
                    }
                    temps.Add(simpleArg);
                }

                // Copy all of the argument values from the temporaries into the function's call frame.
                // TODO: Get the call frame address (and type information) from the function's type entry.
                EmitComment("copy arguments to call frame");
                int paramAddress = T0;
                foreach (Expr temp in temps)
                {
                    int paramSize = SizeOf(CType.UInt16);
                    CompileExpression(temp, paramAddress, Continuation.Fallthrough);
                    paramAddress += paramSize;
                }

                // For builtin operations, instead of jumping to a function, emit the code inline.
                EmitComment(func);
                if (func == "$load")
                {
                    if (argCount != 1) Program.Panic("wrong number of arguments to unary operator");
                    // TODO: This would be more efficient if it loaded the high byte first.
                    Emit_U8(Opcode.LDY_IMM, 0);
                    Emit_U8(Opcode.LDA_ZP_Y_IND, T0);
                    Emit_U8(Opcode.STA_ZP, T2);
                    Emit(Opcode.INY);
                    Emit_U8(Opcode.LDA_ZP_Y_IND, T0);
                    Emit(Opcode.TAX);
                    Emit_U8(Opcode.LDA_ZP, T2);
                }
                else if (func == "$add")
                {
                    if (argCount != 2) Program.Panic("wrong number of arguments to binary operator");
                    Emit(Opcode.CLC);
                    Emit_U8(Opcode.LDA_ZP, T0);
                    Emit_U8(Opcode.ADC_ZP, T2);
                    Emit_U8(Opcode.STA_ZP, T0);
                    Emit_U8(Opcode.LDA_ZP, T1);
                    Emit_U8(Opcode.ADC_ZP, T3);
                    Emit(Opcode.TAX);
                    Emit_U8(Opcode.LDA_ZP, T0);
                }
                else if (func == "$sub")
                {
                    if (argCount != 2) Program.Panic("wrong number of arguments to binary operator");
                    Emit(Opcode.SEC);
                    Emit_U8(Opcode.LDA_ZP, T0);
                    Emit_U8(Opcode.SBC_ZP, T2);
                    Emit_U8(Opcode.STA_ZP, T0);
                    Emit_U8(Opcode.LDA_ZP, T1);
                    Emit_U8(Opcode.SBC_ZP, T3);
                    Emit(Opcode.TAX);
                    Emit_U8(Opcode.LDA_ZP, T0);
                }
                else if (func == "$assign")
                {
                    if (argCount != 2) Program.Panic("wrong number of arguments to binary operator");
                    Emit_U8(Opcode.LDY_IMM, 0);
                    Emit_U8(Opcode.LDA_ZP, T2);
                    Emit_U8(Opcode.STA_ZP_Y_IND, T0);
                    Emit(Opcode.INY);
                    Emit_U8(Opcode.LDA_ZP, T3);
                    Emit_U8(Opcode.STA_ZP_Y_IND, T0);
                }
                else
                {
                    // Check to see whether this function is defined:
                    // (It probably won't have as address assigned yet, but we can make sure it exists.)
                    Symbol sym;
                    if (!FindSymbol(func, out sym)) Program.Error("function not defined: " + func);

                    // JSR to the function:
                    Emit_U16(Opcode.JSR, 0);
                    Fixups.Add(new Fixup(FixupKind.Absolute, GetCurrentCodeAddress() - 2, func));
                }

                // The return value is placed in the accumulator.

                EmitStoreAcc(dest);
                EmitBranchOnAcc(cont);

                EndTempScope();
            }
        }
        else
        {
            Program.NYI();
        }
    }

    public void CompileProgram(List<Declaration> program)
    {
        // First pass: Read all declarations to get type information and global symbols.
        foreach (Declaration decl in program)
        {
            if (decl.Kind == DeclarationKind.Function)
            {
                List<FunctionParameterInfo> parameters = new List<FunctionParameterInfo>();
                // TODO: Allocate space for each parameter and store it in the symbol table.
                // TODO: Make the parameters part of the function type.
                CType type = CType.MakeFunction(decl.Name, parameters, decl.Type);
                DefineSymbol(SymbolKind.Constant, decl.Name, 0, type);
            }
            else if (decl.Kind == DeclarationKind.Constant)
            {
                int value;
                if (!EvaluateConstantExpression(decl.Body, out value))
                {
                    Program.Error("expression must be constant");
                }
                DefineSymbol(SymbolKind.Constant, decl.Name, value, CType.UInt16);
            }
            else
            {
                Program.Panic("unhandled declaration type");
            }
        }

        // Second pass: Generate code for each function.

        // TODO: The various vectors must jump to appropriate specially-named functions.

        foreach (Declaration decl in program)
        {
            if (decl.Kind == DeclarationKind.Function)
            {
                EmitComment("define function " + decl.Name);

                // Record the address of this function's code:
                Symbol sym;
                if (!FindSymbol(decl.Name, out sym)) Program.Panic("a symbol should already be defined for this function.");
                sym.Value = GetCurrentCodeAddress();

                CompileExpression(decl.Body, DestinationDiscard, Continuation.Fallthrough);
                Emit(Opcode.RTS);
            }
        }

        // Fix references to functions:
        foreach (Fixup fixup in Fixups)
        {
            if (fixup.Kind != FixupKind.None)
            {
                Symbol sym;
                if (!FindSymbol(fixup.Target, out sym)) Program.Error("function not defined: " + fixup.Target);
                int target = sym.Value;

                if (fixup.Kind == FixupKind.Absolute)
                {
                    EmitFix_U16(fixup.Location, target);
                    fixup.Kind = FixupKind.None;
                }
            }
        }

        // All fixups should now be fixed.
        if (Fixups.Any(x => x.Kind != FixupKind.None)) Program.Panic("some fixups remain");
    }
}

enum SymbolKind
{
    None = 0,
    Constant,
    Local,
}

class Symbol
{
    public SymbolKind Kind;
    public string Name;
    public int Value;
    public CType Type;
}

class Fixup
{
    public FixupKind Kind;
    public readonly int Location;
    public readonly string Target;

    public Fixup(FixupKind kind, int location, string target)
    {
        Kind = kind;
        Location = location;
        Target = target;
    }
}

enum FixupKind
{
    None,
    Relative,
    Absolute,
}

// type Destination = Discard | Accumulator | AbsoluteAddress Int

enum JumpCondition
{
    Never,
    IfTrue,
    IfFalse,
}

struct Continuation
{
    public readonly JumpCondition When;
    public readonly string Target;

    public Continuation(JumpCondition when, string target)
    {
        When = when;
        Target = target;
    }

    public static readonly Continuation Fallthrough = new Continuation(JumpCondition.Never, null);
}

// CType = SimpleType | FunctionType (name, params, returnType) | ...
partial class CType
{
    public CTypeTag Tag;
    public CSimpleType SimpleType;
    public string Name;
    public List<FunctionParameterInfo> FunctionParameters;
    public CType Subtype;
}

enum CTypeTag
{
    Simple,
    Function,
}

enum CSimpleType
{
    Void,
    UInt16,
}

class FunctionParameterInfo
{
    public CType Type;
    public string Name;
}

partial class CType
{
    public static readonly CType Void = MakeSimple(CSimpleType.Void);
    public static readonly CType UInt16 = MakeSimple(CSimpleType.UInt16);

    public static CType MakeSimple(CSimpleType simple)
    {
        return new CType
        {
            Tag = CTypeTag.Simple,
            SimpleType = simple,
        };
    }

    public static CType MakeFunction(string name, List<FunctionParameterInfo> parameters, CType returnType)
    {
        return new CType
        {
            Tag = CTypeTag.Function,
            Name = name,
            FunctionParameters = parameters,
            Subtype = returnType,
        };
    }

    // TODO: Implement equality testing.
}