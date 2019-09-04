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
        // HACK: Many functions are currently just defined in the compiler, and they use
        // a fixed set of addresses for their arguments.
        // Even worse, they all assume that their parameters are word-sized.
        // Currently, none take more than two parameters.
        int[] builtinParamAddresses = new[] { T0, T2 };

        // Define the types of the builtin functions:
        DefineSymbol(SymbolTag.Constant, Builtins.LoadU8, 0, CType.MakeFunction(new[] { CType.MakePointer(CType.UInt8) }, CType.UInt8), BuiltinParamAddresses(1));
        DefineSymbol(SymbolTag.Constant, Builtins.LoadU16, 0, CType.MakeFunction(new[] { CType.MakePointer(CType.UInt16) }, CType.UInt16), BuiltinParamAddresses(1));
        DefineSymbol(SymbolTag.Constant, Builtins.StoreU8, 0, CType.MakeFunction(new[] { CType.MakePointer(CType.UInt8), CType.UInt8 }, CType.UInt8), BuiltinParamAddresses(2));
        DefineSymbol(SymbolTag.Constant, Builtins.StoreU16, 0, CType.MakeFunction(new[] { CType.MakePointer(CType.UInt16), CType.UInt16 }, CType.UInt16), BuiltinParamAddresses(2));
        DefineSymbol(SymbolTag.Constant, Builtins.AddU8, 0, CType.MakeFunction(new[] { CType.UInt8, CType.UInt8 }, CType.UInt8), BuiltinParamAddresses(2));
        DefineSymbol(SymbolTag.Constant, Builtins.AddU16, 0, CType.MakeFunction(new[] { CType.UInt16, CType.UInt16 }, CType.UInt16), BuiltinParamAddresses(2));
        DefineSymbol(SymbolTag.Constant, Builtins.SubtractU8, 0, CType.MakeFunction(new[] { CType.UInt8, CType.UInt8 }, CType.UInt8), BuiltinParamAddresses(2));
        DefineSymbol(SymbolTag.Constant, Builtins.SubtractU16, 0, CType.MakeFunction(new[] { CType.UInt16, CType.UInt16 }, CType.UInt16), BuiltinParamAddresses(2));
        DefineSymbol(SymbolTag.Constant, Builtins.BoolFromU16, 0, CType.MakeFunction(new[] { CType.UInt16 }, CType.UInt8), BuiltinParamAddresses(1));

        // First pass: Read all declarations to get type information and global symbols.
        foreach (Declaration decl in program)
        {
            if (decl.Tag == DeclarationTag.Function)
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
                DefineSymbol(SymbolTag.Constant, decl.Name, 0, functionType, addresses);
            }
            else if (decl.Tag == DeclarationTag.Constant)
            {
                // TODO: Make sure the declared type matches the actual type.
                CType type;
                int value;
                if (!EvaluateConstantExpression(decl.Body, out value, out type))
                {
                    Program.Error("expression must be constant");
                }
                DefineSymbol(SymbolTag.Constant, decl.Name, value, decl.Type, null);
            }
            else if (decl.Tag == DeclarationTag.Variable)
            {
                DefineVariable(decl.Type, decl.Name);
            }
            else
            {
                Program.Panic("unhandled declaration type");
            }
        }

        // Second pass: Generate code for each function.

        foreach (Declaration decl in program)
        {
            if (decl.Tag == DeclarationTag.Function)
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
                    DefineSymbol(SymbolTag.Variable, f.Name, sym.ParamAddresses[i], f.Type, null);
                }

                Expr body = decl.Body;
                body = ReplaceGenericFunctions(body);
                CheckTypes(body);
                CompileExpression(body, DestinationDiscard, Continuation.Fallthrough);
                Emit(Opcode.RTS);

                EndScope();
            }
        }

        // Fix references to functions:
        foreach (Fixup fixup in Fixups)
        {
            if (fixup.Tag != FixupTag.None)
            {
                Symbol sym;
                if (!FindSymbol(fixup.Target, out sym)) Program.Error("function not defined: " + fixup.Target);
                int target = sym.Value;

                if (fixup.Tag != FixupTag.Absolute) Program.Panic("function fixups should always be absolute");
                EmitFix_U16(fixup.Location, target);
                fixup.Tag = FixupTag.None;
            }
        }

        // All fixups should now be fixed.
        if (Fixups.Any(x => x.Tag != FixupTag.None)) Program.Panic("some fixups remain");
    }

    Expr ReplaceGenericFunctions(Expr e)
    {
        if (e.Tag == ExprTag.Int)
        {
            return e;
        }
        else if (e.Tag == ExprTag.Name)
        {
            return e;
        }
        else if (e.Tag == ExprTag.Scope)
        {
            BeginScope();
            Expr arg = e.Args[0];
            e = ReplaceGenericFunctions(arg);
            EndScope();
            return Expr.MakeScope(e);
        }
        else if (e.Tag == ExprTag.Sequence)
        {
            return Expr.MakeSequence(e.Args.Select(ReplaceGenericFunctions).ToArray());
        }
        else if (e.Tag == ExprTag.Local)
        {
            DefineSymbol(SymbolTag.Variable, e.Name, 0, e.DeclaredType, null);
            return e;
        }
        else if (e.Tag == ExprTag.AddressOf)
        {
            Expr arg = e.Args[0];
            return Expr.MakeAddressOf(ReplaceGenericFunctions(arg));
        }
        else if (e.Tag == ExprTag.Switch)
        {
            return Expr.MakeSwitch(e.Args.Select(ReplaceGenericFunctions).ToArray());
        }
        else if (e.Tag == ExprTag.For)
        {
            return Expr.MakeFor(
                ReplaceGenericFunctions(e.Args[0]),
                ReplaceGenericFunctions(e.Args[1]),
                ReplaceGenericFunctions(e.Args[2]),
                ReplaceGenericFunctions(e.Args[3]));
        }
        else if (e.Tag == ExprTag.Return)
        {
            Expr arg = e.Args[0];
            return ReplaceGenericFunctions(arg);
        }
        else if (e.Tag == ExprTag.Call)
        {
            Expr[] args = e.Args.Select(ReplaceGenericFunctions).ToArray();

            if (e.Name == Builtins.LoadGeneric)
            {
                Expr source = args[0];

                CType addressType = TypeOf(source);
                if (addressType.Tag != CTypeTag.Pointer) Program.Error("load address must have pointer type");
                CType returnType = addressType.Subtype;

                string specificName = null;
                if (TypesEqual(returnType, CType.UInt16)) specificName = Builtins.LoadU16;
                else Program.NYI();

                return Expr.MakeCall(specificName, source);
            }
            else if (e.Name == Builtins.StoreGeneric)
            {
                Expr left = args[0];
                Expr right = args[1];
                CType leftType = TypeOf(left);

                CType addressType = TypeOf(left);
                if (addressType.Tag != CTypeTag.Pointer) Program.Error("store address must have pointer type");
                CType expectedTypeOfValue = addressType.Subtype;

                right = ChangeTypeIfPossible(right, expectedTypeOfValue);
                CType actualType = TypeOf(right);
                if (!TypesEqual(expectedTypeOfValue, actualType)) Program.Error("types in assignment must match");

                string specificName = null;
                if (TypesEqual(actualType, CType.UInt8)) specificName = Builtins.StoreU8;
                else if (TypesEqual(actualType, CType.UInt16)) specificName = Builtins.StoreU16;
                else Program.NYI();

                return Expr.MakeCall(specificName, left, right);
            }
            else if (e.Name == Builtins.AddGeneric)
            {
                Expr left = args[0];
                Expr right = args[1];
                CType leftType = TypeOf(left);

                right = ChangeTypeIfPossible(right, leftType);
                if (!TypesEqual(leftType, TypeOf(right))) Program.Error("types in binary expression must match");

                string specificName = null;
                if (TypesEqual(leftType, CType.UInt8)) specificName = Builtins.AddU8;
                else if (TypesEqual(leftType, CType.UInt16)) specificName = Builtins.AddU16;
                else Program.NYI();

                return Expr.MakeCall(specificName, left, right);
            }
            else if (e.Name == Builtins.SubtractGeneric)
            {
                Expr left = args[0];
                Expr right = args[1];
                CType leftType = TypeOf(left);

                right = ChangeTypeIfPossible(args[1], leftType);
                if (!TypesEqual(leftType, TypeOf(right))) Program.Error("types in binary expression must match");

                string specificName = null;
                if (TypesEqual(leftType, CType.UInt8)) specificName = Builtins.SubtractU8;
                else if (TypesEqual(leftType, CType.UInt16)) specificName = Builtins.SubtractU16;
                else Program.NYI();

                return Expr.MakeCall(specificName, left, right);
            }
            else if (e.Name == Builtins.BoolFromGeneric)
            {
                Expr source = args[0];
                CType sourceType = TypeOf(source);

                string specificName = null;
                if (TypesEqual(sourceType, CType.UInt16)) specificName = Builtins.BoolFromU16;
                else Program.NYI();

                return Expr.MakeCall(specificName, args);
            }
            else
            {
                return Expr.MakeCall(e.Name, args);
            }
        }
        else
        {
            Program.Panic("unhandled case");
            return null;
        }
    }

    /// <summary>
    /// It is possible to interpret some expressions, such as integer literals, as any of several types.
    /// If possible, change the type of the given expression to the expected type.
    /// </summary>
    Expr ChangeTypeIfPossible(Expr e, CType expected)
    {
        // Make sure the actual value is small enough to fit.
        if (TypesEqual(expected, CType.UInt8) && e.Tag == ExprTag.Int)
        {
            int value = e.Int;
            if (value >= 0 && value <= byte.MaxValue)
            {
                return Expr.MakeInt(value, CType.UInt8);
            }
        }

        return e;
    }

    void CheckTypes(Expr e)
    {
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
            DefineSymbol(SymbolTag.Variable, e.Name, 0, e.DeclaredType, null);
        }
        else if (e.Tag == ExprTag.AddressOf)
        {
            Expr arg = e.Args[0];
            CheckTypes(arg);
            if (arg.Tag != ExprTag.Name) Program.NYI();
            Symbol sym;
            if (!FindSymbol(arg.Name, out sym)) Program.Error("symbol not defined: {0}", arg.Name);
            if (sym.Tag != SymbolTag.Variable) Program.Error("cannot take address of a constant: {0}", arg.Name);
        }
        else if (e.Tag == ExprTag.Switch)
        {
            foreach (Expr sub in e.Args) CheckTypes(sub);
            // TODO: Each condition must have type "uint8_t".
            // TODO: Each then-clause must have the same type.
        }
        else if (e.Tag == ExprTag.For)
        {
            foreach (Expr sub in e.Args) CheckTypes(sub);
            // TODO: Are there any other rules?
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
            Symbol sym;
            if (!FindSymbol(e.Name, out sym)) Program.Error("symbol not defined: {0}", e.Name);
            if (sym.Type.Tag != CTypeTag.Function) Program.Error("symbol is not a function: {0}", e.Name);
            if (sym.Type.ParameterTypes.Length != e.Args.Length) Program.Error("wrong number of arguments to function: {0}", e.Name);
            // Check that each of the actual and expected parameter types match:
            for (int i = 0; i < e.Args.Length; i++)
            {
                Expr arg = e.Args[i];
                CheckTypes(arg);
                CType argType = TypeOf(arg);
                if (!TypesEqual(argType, sym.Type.ParameterTypes[i])) Program.Error("argument to function has wrong type");
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
            return e.DeclaredType;
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
            CType subtype = TypeOf(arg);
            return CType.MakePointer(subtype);
        }
        else if (e.Tag == ExprTag.Switch)
        {
            // TODO: Figure out the type.
            return CType.Void;
        }
        else if (e.Tag == ExprTag.For)
        {
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
            // The type of a function call expression is the function's return type:
            return sym.Type.Subtype;
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
        else if (a.Tag == CTypeTag.Pointer) return TypesEqual(a.Subtype, b.Subtype);
        else
        {
            Program.NYI();
            return false;
        }
    }

    void CompileExpression(Expr e, int dest, Continuation cont)
    {
        int value;
        CType type;
        if (EvaluateConstantExpression(e, out value, out type))
        {
            EmitLoadImmediate(value, type, dest, cont);
        }
        else if (e.Tag == ExprTag.Name)
        {
            Symbol sym;
            if (!FindSymbol(e.Name, out sym)) Program.Error("undefined symbol");
            if (sym.Tag != SymbolTag.Variable) Program.NYI();
            int address = sym.Value;
            EmitLoad(address, sym.Type, dest, cont);
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
            DefineVariable(e.DeclaredType, e.Name);
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
                if (sym.Tag != SymbolTag.Variable) Program.Error("target of assignment must be a variable: " + arg.Name);
                int address = sym.Value;
                EmitLoadImmediate(address, CType.MakePointer(TypeOf(arg)), dest, cont);
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
        else if (e.Tag == ExprTag.For)
        {
            if (e.Args.Length != 4) Program.Panic("wrong number of items in for expression");
            Expr init = e.Args[0];
            Expr test = e.Args[1];
            Expr induction = e.Args[2];
            Expr body = e.Args[3];
            string bottom = MakeUniqueLabel();

            EmitComment("for loop (prologue)");
            BeginScope();
            CompileExpression(init, DestinationDiscard, Continuation.Fallthrough);
            EmitComment("for loop");
            int top = GetCurrentCodeAddress();
            CompileExpression(test, DestinationDiscard, new Continuation(JumpCondition.IfFalse, bottom));
            CompileExpression(body, DestinationDiscard, Continuation.Fallthrough);
            CompileExpression(induction, DestinationDiscard, Continuation.Fallthrough);
            EmitJump(top);
            EndScope();
            EmitComment("for loop (end)");
            FixReferencesTo(bottom);

            if (cont.When != JumpCondition.Never) Program.NYI();
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
            int addr;
            if ((e.Name == Builtins.LoadU8 || e.Name == Builtins.LoadU16) && EvaluateConstantExpression(e.Args[0], out addr, out type))
            {
                // Loads from constant addresses must be optimized, because this pattern is used to copy data from
                // temporary variables into call frames; we can't generate a call to "load_***" in the middle of
                // generating some other call.

                EmitLoad(addr, type, dest, cont);
            }
            else if ((e.Name == Builtins.StoreU8 || e.Name == Builtins.StoreU16) && EvaluateConstantExpression(e.Args[0], out addr, out type))
            {
                // This case is not essential, but generates better code.

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
                    EmitStoreAcc(type, addr);
                    EmitStoreAcc(type, dest);
                    EmitBranchOnAcc(cont);
                }
            }
            else
            {
                // This is the general-purpose way of calling functions.

                Symbol functionSym;
                if (!FindSymbol(e.Name, out functionSym)) Program.Error("function is not defined: " + e.Name);

                // Get the call frame address (and type information) from the function's type entry.
                if (functionSym.Type.Tag != CTypeTag.Function) Program.Error("only functions can be called: " + e.Name);
                CType[] paramTypes = functionSym.Type.ParameterTypes;
                int[] paramAddresses = functionSym.ParamAddresses;

                if (paramTypes.Length != paramAddresses.Length) Program.Panic("in function symbol, the number of types doesn't match the number of argument locations: " + e.Name);
                if (e.Args.Length != paramTypes.Length) Program.Error("wrong number of arguments to function: " + e.Name);

                BeginTempScope();
                EmitComment("prepare call: function " + e.Name);

                // Evaluate the arguments and store the results in temporary variables.
                // TODO: (optimization) The first arg doesn't have to be simplified, since we haven't started assembling a call frame yet.
                // Optimization: Sufficiently simple arguments (such as literal ints) can skip this
                //   step and be written directly into the call frame.
                List<Expr> temps = new List<Expr>();
                for (int i = 0; i < paramTypes.Length; i++)
                {
                    Expr arg = e.Args[i];
                    CType argType = paramTypes[i];

                    int n;
                    CType ignored;
                    Expr simpleArg;
                    if (EvaluateConstantExpression(arg, out n, out ignored))
                    {
                        simpleArg = Expr.MakeInt(n, argType);
                    }
                    else if (arg.Tag == ExprTag.Name)
                    {
                        // An identifier is simple enough that it doesn't need to be loaded into a temporary.
                        simpleArg = arg;
                    }
                    else
                    {
                        EmitComment("prepare call: create temporary for argument " + i);
                        int argSize = SizeOf(argType);
                        int temp = AllocTemp(argSize);
                        CompileExpression(arg, temp, Continuation.Fallthrough);
                        simpleArg = Expr.MakeCall(GetLoadFunctionForType(argType), Expr.MakeInt(temp, argType));
                    }
                    temps.Add(simpleArg);
                }

                // Copy all of the argument values from the temporaries into the function's call frame.
                for (int i = 0; i < temps.Count; i++)
                {
                    EmitComment("prepare call: load argument " + i);
                    CompileExpression(temps[i], paramAddresses[i], Continuation.Fallthrough);
                }

                // For builtin operations, instead of jumping to a function, emit the code inline.
                EmitComment("function body: " + e.Name);
                if (e.Name == Builtins.LoadU16)
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
                else if (e.Name == Builtins.AddU8)
                {
                    if (e.Args.Length != 2) Program.Panic("wrong number of arguments to binary operator");
                    Emit(Opcode.CLC);
                    Emit_U8(Opcode.LDA_ZP, T0);
                    Emit_U8(Opcode.ADC_ZP, T2);
                }
                else if (e.Name == Builtins.AddU16)
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
                else if (e.Name == Builtins.SubtractU8)
                {
                    if (e.Args.Length != 2) Program.Panic("wrong number of arguments to binary operator");
                    Emit(Opcode.SEC);
                    Emit_U8(Opcode.LDA_ZP, T0);
                    Emit_U8(Opcode.SBC_ZP, T2);
                }
                else if (e.Name == Builtins.SubtractU16)
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
                else if (e.Name == Builtins.StoreU16)
                {
                    if (e.Args.Length != 2) Program.Panic("wrong number of arguments to binary operator");
                    Emit_U8(Opcode.LDY_IMM, 0);
                    Emit_U8(Opcode.LDA_ZP, T2);
                    Emit_U8(Opcode.STA_ZP_Y_IND, T0);
                    Emit(Opcode.INY);
                    Emit_U8(Opcode.LDA_ZP, T3);
                    Emit_U8(Opcode.STA_ZP_Y_IND, T0);
                }
                else if (e.Name == Builtins.StoreU8)
                {
                    if (e.Args.Length != 2) Program.Panic("wrong number of arguments to binary operator");
                    Emit_U8(Opcode.LDY_IMM, 0);
                    Emit_U8(Opcode.LDA_ZP, T2);
                    Emit_U8(Opcode.STA_ZP_Y_IND, T0);
                }
                else if (e.Name == Builtins.BoolFromU16)
                {
                    if (e.Args.Length != 1) Program.Panic("wrong number of arguments to unary operator");
                    Emit_U8(Opcode.LDA_ZP, T0);
                    Emit_U8(Opcode.ORA_ZP, T1);
                }
                else
                {
                    // Check to see whether this function is defined:
                    // (It probably won't have as address assigned yet, but we can make sure it exists.)
                    Symbol sym;
                    if (!FindSymbol(e.Name, out sym)) Program.Error("function not defined: " + e.Name);

                    // JSR to the function:
                    Emit_U16(Opcode.JSR, 0);
                    Fixups.Add(new Fixup(FixupTag.Absolute, GetCurrentCodeAddress() - 2, e.Name));
                }

                // The return value is placed in the accumulator.

                CType functionReturnType = functionSym.Type.Subtype;
                EmitStoreAcc(functionReturnType, dest);
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
    bool EvaluateConstantExpression(Expr e, out int value, out CType type)
    {
        // TODO: Evaluate more complex constant expressions, too.
        if (e.Tag == ExprTag.Int)
        {
            value = e.Int;
            type = e.DeclaredType;
            return true;
        }
        else if (e.Tag == ExprTag.Name)
        {
            Symbol sym;
            if (!FindSymbol(e.Name, out sym)) Program.Error("undefined symbol: {0}", e.Name);

            if (sym.Tag == SymbolTag.Constant)
            {
                // TODO: Make sure the constant value is not too big.
                value = sym.Value;
                type = sym.Type;
                return true;
            }
        }
        else if (e.Tag == ExprTag.AddressOf)
        {
            Expr arg = e.Args[0];
            if (arg.Tag == ExprTag.Name)
            {
                Symbol sym;
                if (!FindSymbol(arg.Name, out sym)) Program.Error("undefined symbol: {0}", e.Name);

                if (sym.Tag == SymbolTag.Variable)
                {
                    value = sym.Value;
                    type = CType.MakePointer(sym.Type);
                    return true;
                }
            }
        }

        value = 0;
        type = null;
        return false;
    }

    void EmitJump(int target)
    {
        Emit_U16(Opcode.JMP_ABS, target);
    }

    void EmitJump(string label)
    {
        Emit_U16(Opcode.JMP_ABS, 0);
        int fixupAddress = GetCurrentCodeAddress() - 2;
        Fixups.Add(new Fixup(FixupTag.Absolute, fixupAddress, label));
    }

    void EmitLoadImmediate(int imm, CType type, int dest, Continuation cont)
    {
        int width = SizeOf(type);
        if (dest == DestinationDiscard)
        {
            // NOP
        }
        else if (dest == DestinationAcc)
        {
            Emit_U8(Opcode.LDA_IMM, LowByte(imm));
            if (width == 2) Emit_U8(Opcode.LDX_IMM, HighByte(imm));
        }
        else
        {
            Emit_U8(Opcode.LDA_IMM, LowByte(imm));
            Emit_U16(Opcode.STA_ABS, dest);
            if (width == 2)
            {
                Emit_U8(Opcode.LDX_IMM, HighByte(imm));
                Emit_U16(Opcode.STX_ABS, dest + 1);
            }
        }

        if ((cont.When == JumpCondition.IfTrue && imm != 0) ||
            (cont.When == JumpCondition.IfFalse && imm == 0))
        {
            EmitJump(cont.Target);
        }
    }

    void EmitStoreAcc(CType type, int dest)
    {
        if (dest == DestinationDiscard || dest == DestinationAcc)
        {
            // NOP
        }
        else
        {
            int width = SizeOf(type);
            Emit_U16(Opcode.STA_ABS, dest);
            if (width == 2) Emit_U16(Opcode.STX_ABS, dest + 1);
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
            Fixups.Add(new Fixup(FixupTag.Relative, fixupAddress, cont.Target));
        }
    }

    void EmitLoad(int address, CType type, int dest, Continuation cont)
    {
        int width = SizeOf(type);
        // Even if the value is unused, always read the address; it might be a hardware register.
        Emit_U16(Opcode.LDA_ABS, address);
        if (width == 2) Emit_U16(Opcode.LDX_ABS, address + 1);
        EmitStoreAcc(type, dest);
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
                if (fixup.Tag == FixupTag.Relative)
                {
                    EmitFix_S8(fixup.Location, target);
                    fixup.Tag = FixupTag.None;
                }
                else if (fixup.Tag == FixupTag.Absolute)
                {
                    EmitFix_U16(fixup.Location, target);
                    fixup.Tag = FixupTag.None;
                }
            }
        }
    }

    void DefineVariable(CType type, string name)
    {
        int address = AllocGlobal(SizeOf(type));
        DefineSymbol(SymbolTag.Variable, name, address, type, null);
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
        if (type.Tag == CTypeTag.Simple)
        {
            if (type.SimpleType == CSimpleType.UInt8) return 1;
            else if (type.SimpleType == CSimpleType.UInt16) return 2;
        }
        else if (type.Tag == CTypeTag.Pointer)
        {
            return 2;
        }

        Program.NYI();
        return 1;
    }

    static string GetLoadFunctionForType(CType type)
    {
        int width = SizeOf(type);
        if (width == 1) return Builtins.LoadU8;
        else if (width == 2) return Builtins.LoadU16;
        Program.NYI();
        return null;
    }

    void DefineSymbol(SymbolTag tag, string name, int value, CType type, int[] paramAddresses)
    {
        // It is an error to define two things with the same name in the same scope.
        if (CurrentScope.Symbols.Any(x => x.Name == name))
        {
            Program.Error("symbols cannot be redefined: {0}", name);
        }

        CurrentScope.Symbols.Add(new Symbol
        {
            Tag = tag,
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

    // TODO: This is a hack, used to define builtin functions.
    int[] BuiltinParamAddresses(int count)
    {
        if (count == 1) return new[] { T0 };
        else if (count == 2) return new[] { T0, T2 };
        Program.Panic("unhandled case");
        return null;
    }
}

[DebuggerDisplay("{Tag} {Name} = 0x{Value,h} ({Type.Show(),nq})")]
class Symbol
{
    public SymbolTag Tag;
    public string Name;
    public int Value;
    public CType Type;
    public int[] ParamAddresses;
}

enum SymbolTag
{
    Constant,
    Variable,
}

class Fixup
{
    public FixupTag Tag;
    public readonly int Location;
    public readonly string Target;

    public Fixup(FixupTag tag, int location, string target)
    {
        Tag = tag;
        Location = location;
        Target = target;
    }
}

enum FixupTag
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
    UInt8,
    UInt16,
}

[DebuggerDisplay("{Show(),nq}")]
partial class CType
{
    public static readonly CType Void = MakeSimple(CSimpleType.Void);
    public static readonly CType UInt8 = MakeSimple(CSimpleType.UInt8);
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

    public string Show()
    {
        if (Tag == CTypeTag.Simple) return SimpleType.ToString();
        else if (Tag == CTypeTag.Pointer) return "pointer to " + Subtype.Show();
        else if (Tag == CTypeTag.Function)
        {
            var paramTypes = ParameterTypes.Select(x => x.Show());
            return string.Format("function({0}) {1}", string.Join(", ", paramTypes), Subtype.Show());
        }
        else throw new NotImplementedException();
    }
}

class LexicalScope
{
    public LexicalScope Outer;
    public List<Symbol> Symbols = new List<Symbol>();
}
