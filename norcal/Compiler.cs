using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

partial class Compiler
{
    int RamNext = RamStart;
    int NextLabelNumber = 0;
    LexicalScope CurrentScope = new LexicalScope();
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

    public void CompileProgram(List<Declaration> program)
    {
        // First pass: Read all declarations to get type information and global symbols.
        foreach (Declaration decl in program)
        {
            if (decl.Kind == DeclarationKind.Function)
            {
                // Allocate space for each parameter and store it in the symbol table.
                int paramCount = decl.Fields.Count;
                CType[] paramTypes = new CType[paramCount];
                int[] addresses = new int[paramCount];
                for (int i = 0; i < paramCount; i++)
                {
                    CType type = decl.Fields[i].Type;
                    paramTypes[i] = type;
                    addresses[i] = AllocGlobal(SizeOf(type));
                }

                CType functionType = CType.MakeFunction(paramTypes, decl.Type);
                DefineSymbol(SymbolKind.Constant, decl.Name, 0, functionType, addresses);
            }
            else if (decl.Kind == DeclarationKind.Constant)
            {
                int value;
                if (!EvaluateConstantExpression(decl.Body, out value))
                {
                    Program.Error("expression must be constant");
                }
                DefineSymbol(SymbolKind.Constant, decl.Name, value, CType.UInt16, null);
            }
            else
            {
                Program.Panic("unhandled declaration type");
            }
        }

        // Second pass: Generate code for each function.

        foreach (Declaration decl in program)
        {
            if (decl.Kind == DeclarationKind.Function)
            {
                EmitComment("define function " + decl.Name);

                // Record the address of this function's code:
                Symbol sym;
                if (!FindSymbol(decl.Name, out sym)) Program.Panic("a symbol should already be defined for this function.");
                sym.Value = GetCurrentCodeAddress();

                BeginScope();

                // Define each of the function's parameters as a local variable:
                for (int i = 0; i < decl.Fields.Count; i++)
                {
                    NamedField f = decl.Fields[i];
                    DefineSymbol(SymbolKind.Local, f.Name, sym.ParamAddresses[i], f.Type, null);
                }

                Expr body = decl.Body;
                CheckTypes(body);
                CompileExpression(body, DestinationDiscard, Continuation.Fallthrough);
                Emit(Opcode.RTS);

                EndScope();
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

    void CheckTypes(Expr e)
    {
        // TODO: Typecheck each subexpression.

        if (e.Tag == ExprTag.Int)
        {
            // NOP
        }
        else if (e.Tag == ExprTag.Name)
        {
            // NOP
        }
        else if (e.Tag == ExprTag.Scope)
        {
            BeginScope();
            Expr arg = e.Args[0];
            CheckTypes(arg);
            EndScope();
        }
        else if (e.Tag == ExprTag.Sequence)
        {
            foreach (Expr sub in e.Args) CheckTypes(sub);
        }
        else if (e.Tag == ExprTag.Local)
        {
            DefineSymbol(SymbolKind.Local, e.Name, 0, CType.UInt16, null);
        }
        else if (e.Tag == ExprTag.AddressOf)
        {
            Expr arg = e.Args[0];
            CheckTypes(arg);
            CType type = TypeOf(arg);
            if (type.Tag != CTypeTag.Pointer) Program.Error("this expression has no address");
        }
        else if (e.Tag == ExprTag.Switch)
        {
            foreach (Expr sub in e.Args) CheckTypes(sub);
            // TODO: Each condition must have type "uint8_t".
            // TODO: Each then-clause must have the same type.
        }
        else if (e.Tag == ExprTag.Return)
        {
            Expr arg = e.Args[0];
            CheckTypes(arg);
            CType actual = TypeOf(arg);
            // TODO: Get the current function's declared return type.
            CType expected = CType.UInt16;
            if (!TypesEqual(actual, expected)) Program.Error("incorrect return type");
        }
        else if (e.Tag == ExprTag.Call)
        {
            // TODO: Replace type-generic functions with specific functions before typechecking.
            if (!e.Name.StartsWith("$"))
            {
                Symbol sym;
                if (!FindSymbol(e.Name, out sym)) Program.Error("symbol not defined: {0}", e.Name);
                if (sym.Type.Tag != CTypeTag.Function) Program.Error("symbol is not a function: {0}", e.Name);

                if (sym.Type.ParameterTypes.Length != e.Args.Length) Program.Error("wrong number of arguments to function: {0}", e.Name);

                // TODO: Check that the actual and expected parameter types match.
                for (int i = 0; i < e.Args.Length; i++)
                {
                    Expr arg = e.Args[i];
                    CheckTypes(arg);
                    if (!TypesEqual(TypeOf(arg), sym.Type.ParameterTypes[i])) Program.Error("argument to function has wrong type");
                }
            }
        }
        else
        {
            Program.Panic("type checker: unhandled case");
        }
    }

    CType TypeOf(Expr e)
    {
        if (e.Tag == ExprTag.Int)
        {
            return CType.UInt16;
        }
        else if (e.Tag == ExprTag.Name)
        {
            Symbol sym;
            if (!FindSymbol(e.Name, out sym)) Program.Error("symbol not defined: {0}", e.Name);
            return sym.Type;
        }
        else if (e.Tag == ExprTag.Scope)
        {
            Expr arg = e.Args[0];
            return TypeOf(arg);
        }
        else if (e.Tag == ExprTag.Sequence)
        {
            if (e.Args.Length > 0) return TypeOf(e.Args.Last());
            else return CType.Void;
        }
        else if (e.Tag == ExprTag.Local)
        {
            return CType.Void;
        }
        else if (e.Tag == ExprTag.AddressOf)
        {
            Expr arg = e.Args[0];
            CType type = TypeOf(arg);
            if (type.Tag != CTypeTag.Pointer) Program.Error("this expression has no address");
            return type.Subtype;
        }
        else if (e.Tag == ExprTag.Switch)
        {
            // TODO: Figure out the type.
            return CType.Void;
        }
        else if (e.Tag == ExprTag.Return)
        {
            return CType.Void;
        }
        else if (e.Tag == ExprTag.Call)
        {
            Symbol sym;
            if (!FindSymbol(e.Name, out sym)) Program.Error("symbol not defined: {0}", e.Name);
            if (sym.Type.Tag != CTypeTag.Function) Program.Error("symbol is not a function: {0}", e.Name);
            return sym.Type;
        }
        else
        {
            Program.Panic("type of: unhandled case");
            return null;
        }
    }

    static bool TypesEqual(CType a, CType b)
    {
        if (a.Tag != b.Tag) return false;
        else if (a.Tag == CTypeTag.Simple) return a.SimpleType == b.SimpleType;
        else
        {
            Program.NYI();
            return false;
        }
    }

    void CompileExpression(Expr e, int dest, Continuation cont)
    {
        int value;
        if (EvaluateConstantExpression(e, out value))
        {
            EmitLoadImmediate(value, dest, cont);
        }
        else if (e.Tag == ExprTag.Name)
        {
            Symbol sym;
            if (!FindSymbol(e.Name, out sym)) Program.Error("undefined symbol");
            if (sym.Kind != SymbolKind.Local) Program.NYI();
            int address = sym.Value;
            EmitLoad(address, dest, cont);
        }
        else if (e.Tag == ExprTag.Scope)
        {
            BeginScope();
            CompileExpression(e.Args[0], dest, cont);
            EndScope();
        }
        else if (e.Tag == ExprTag.Sequence)
        {
            for (int i = 0; i < e.Args.Length; i++)
            {
                Expr subexpr = e.Args[i];
                EmitComment("begin new statement");
                // Drop the result of each expression except the last.
                bool isLast = (i == e.Args.Length - 1);
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
        else if (e.Tag == ExprTag.Local)
        {
            DefineLocal(CType.UInt16, e.Name);
            if (dest != DestinationDiscard) Program.Panic("cannot store value of expression of type 'void'");
            if (cont.When != JumpCondition.Never) Program.Panic("cannot branch based on value of type 'void'");
        }
        else if (e.Tag == ExprTag.AddressOf)
        {
            if (e.Args.Length != 1) Program.Panic("wrong number of args");
            Expr arg = e.Args[0];
            if (arg.Tag == ExprTag.Name)
            {
                Symbol sym;
                if (!FindSymbol(arg.Name, out sym)) Program.Error("undefined symbol");
                if (sym.Kind != SymbolKind.Local) Program.Error("target of assignment must be a variable: " + arg.Name);
                int address = sym.Value;
                EmitLoadImmediate(address, dest, cont);
            }
            else
            {
                Program.NYI();
            }
        }
        else if (e.Tag == ExprTag.Switch)
        {
            if (e.Args.Length != 2) Program.Panic("wrong number of items in switch expression");
            Expr test = e.Args[0];
            Expr then = e.Args[1];
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
        else if (e.Tag == ExprTag.Return)
        {
            if (e.Args.Length != 1) Program.Panic("wrong number of items in switch expression");
            EmitComment("return");
            CompileExpression(e.Args[0], DestinationAcc, Continuation.Fallthrough);
            Emit(Opcode.RTS);
        }
        else if (e.Tag == ExprTag.Call)
        {
            // Handle certain functions as "intrinsics"; otherwise use the general function call mechanism.
            int addr;
            if (e.Name == "$load" && EvaluateConstantExpression(e.Args[0], out addr))
            {
                EmitLoad(addr, dest, cont);
            }
            else if (e.Name == "$assign" && EvaluateConstantExpression(e.Args[0], out addr))
            {
                if (dest == DestinationDiscard)
                {
                    EmitComment("assign to constant address");
                    // Let the sub-expression handle storing the data _and_ any conditional branch.
                    CompileExpression(e.Args[1], addr, cont);
                }
                else
                {
                    EmitComment("assign to constant address, and produce the assigned value");
                    CompileExpression(e.Args[1], DestinationAcc, Continuation.Fallthrough);
                    EmitStoreAcc(addr);
                    EmitStoreAcc(dest);
                    EmitBranchOnAcc(cont);
                }
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
                        simpleArg = Expr.MakeCall("$load", Expr.MakeInt(temp));
                    }
                    temps.Add(simpleArg);
                }

                // Get the call frame address (and type information) from the function's type entry.
                Symbol functionSym;
                int[] paramAddresses;
                if (FindSymbol(e.Name, out functionSym))
                {
                    paramAddresses = functionSym.ParamAddresses;
                }
                else
                {
                    // HACK: Many functions are currently just defined in the compiler, and they use
                    // a fixed set of addresses for their arguments.
                    // Even worse, they all assume that their parameters are word-sized.
                    // Currently, none take more than two parameters.
                    paramAddresses = new[] { T0, T2 };
                }

                // Copy all of the argument values from the temporaries into the function's call frame.
                EmitComment("copy arguments to call frame");
                for (int i = 0; i < temps.Count; i++)
                {
                    CompileExpression(temps[i], paramAddresses[i], Continuation.Fallthrough);
                }

                // For builtin operations, instead of jumping to a function, emit the code inline.
                EmitComment(e.Name);
                if (e.Name == "$load")
                {
                    if (e.Args.Length != 1) Program.Panic("wrong number of arguments to unary operator");
                    // TODO: This would be more efficient if it loaded the high byte first.
                    Emit_U8(Opcode.LDY_IMM, 0);
                    Emit_U8(Opcode.LDA_ZP_Y_IND, T0);
                    Emit_U8(Opcode.STA_ZP, T2);
                    Emit(Opcode.INY);
                    Emit_U8(Opcode.LDA_ZP_Y_IND, T0);
                    Emit(Opcode.TAX);
                    Emit_U8(Opcode.LDA_ZP, T2);
                }
                else if (e.Name == "$add")
                {
                    if (e.Args.Length != 2) Program.Panic("wrong number of arguments to binary operator");
                    Emit(Opcode.CLC);
                    Emit_U8(Opcode.LDA_ZP, T0);
                    Emit_U8(Opcode.ADC_ZP, T2);
                    Emit_U8(Opcode.STA_ZP, T0);
                    Emit_U8(Opcode.LDA_ZP, T1);
                    Emit_U8(Opcode.ADC_ZP, T3);
                    Emit(Opcode.TAX);
                    Emit_U8(Opcode.LDA_ZP, T0);
                }
                else if (e.Name == "$sub")
                {
                    if (e.Args.Length != 2) Program.Panic("wrong number of arguments to binary operator");
                    Emit(Opcode.SEC);
                    Emit_U8(Opcode.LDA_ZP, T0);
                    Emit_U8(Opcode.SBC_ZP, T2);
                    Emit_U8(Opcode.STA_ZP, T0);
                    Emit_U8(Opcode.LDA_ZP, T1);
                    Emit_U8(Opcode.SBC_ZP, T3);
                    Emit(Opcode.TAX);
                    Emit_U8(Opcode.LDA_ZP, T0);
                }
                else if (e.Name == "$assign")
                {
                    if (e.Args.Length != 2) Program.Panic("wrong number of arguments to binary operator");
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
                    if (!FindSymbol(e.Name, out sym)) Program.Error("function not defined: " + e.Name);

                    // JSR to the function:
                    Emit_U16(Opcode.JSR, 0);
                    Fixups.Add(new Fixup(FixupKind.Absolute, GetCurrentCodeAddress() - 2, e.Name));
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

    // Return true if the expression was constant, and therefore able to be evaluated.
    bool EvaluateConstantExpression(Expr e, out int value)
    {
        // TODO: Evaluate more complex constant expressions, too.
        if (e.MatchInt(out value))
        {
            return true;
        }
        else if (e.Tag == ExprTag.Name)
        {
            Symbol sym;
            if (!FindSymbol(e.Name, out sym)) Program.Error("undefined symbol: {0}", e.Name);

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

    void DefineLocal(CType type, string name)
    {
        int address = AllocGlobal(SizeOf(type));
        DefineSymbol(SymbolKind.Local, name, address, type, null);
    }

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

    void DefineSymbol(SymbolKind kind, string name, int value, CType type, int[] paramAddresses)
    {
        // It is an error to define two things with the same name in the same scope.
        if (CurrentScope.Symbols.Any(x => x.Name == name))
        {
            Program.Error("symbols cannot be redefined: {0}", name);
        }

        CurrentScope.Symbols.Add(new Symbol
        {
            Kind = kind,
            Name = name,
            Value = value,
            Type = type,
            ParamAddresses = paramAddresses,
        });
    }

    bool FindSymbol(string name, out Symbol found)
    {
        // Inspect all scopes, starting from the innermost.
        for (LexicalScope scope = CurrentScope; scope != null; scope = scope.Outer)
        {
            foreach (Symbol sym in scope.Symbols)
            {
                if (sym.Name == name)
                {
                    found = sym;
                    return true;
                }
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
        // TODO: Implement temporaries.
    }

    void EndTempScope()
    {
        // TODO: Implement temporaries.
    }

    void BeginScope()
    {
        LexicalScope outer = CurrentScope;
        CurrentScope = new LexicalScope();
        CurrentScope.Outer = outer;
    }

    void EndScope()
    {
        CurrentScope = CurrentScope.Outer;
    }

    // TODO: Temporary variables can be freed when the current temp scope ends.
    int AllocTemp(int size)
    {
        return AllocGlobal(size);
    }
}

enum SymbolKind
{
    Constant,
    Local,
}

[DebuggerDisplay("{Kind} {Name} = 0x{Value,h}")]
class Symbol
{
    public SymbolKind Kind;
    public string Name;
    public int Value;
    public CType Type;
    public int[] ParamAddresses;
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

// CType = SimpleType | FunctionType (name, params, returnType) | Pointer subtype
partial class CType
{
    public CTypeTag Tag;
    public CSimpleType SimpleType;
    public string Name;
    public CType[] ParameterTypes;
    public CType Subtype;
}

enum CTypeTag
{
    Simple,
    Function,
    Pointer,
}

enum CSimpleType
{
    Void,
    UInt16,
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

    public static CType MakeFunction(CType[] parameterTypes, CType returnType)
    {
        return new CType
        {
            Tag = CTypeTag.Function,
            Name = null,
            ParameterTypes = parameterTypes,
            Subtype = returnType,
        };
    }

    public static CType MakePointer(CType subtype)
    {
        return new CType
        {
            Tag = CTypeTag.Pointer,
            Subtype = subtype,
        };
    }

    public override bool Equals(object obj)
    {
        throw new NotSupportedException();
    }

    public override int GetHashCode()
    {
        throw new NotSupportedException();
    }
}

class LexicalScope
{
    public LexicalScope Outer;
    public List<Symbol> Symbols = new List<Symbol>();
}
