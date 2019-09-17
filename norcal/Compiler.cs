using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

partial class Compiler
{
    int RamNext = RamStart;
    int NextLabelNumber = 0;
    Dictionary<string, CFunctionInfo> Functions = new Dictionary<string, CFunctionInfo>();
    List<Fixup> Fixups = new List<Fixup>();
    Dictionary<string, CStructInfo> StructTypes = new Dictionary<string, CStructInfo>();

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

    static readonly string[] LoadFunctions = new[]
    {
        Builtins.LoadU8,
        Builtins.LoadU16,
    };

    public void CompileProgram(List<Declaration> program)
    {
        // HACK: Many functions are currently just defined in the compiler, and they use
        // a fixed set of addresses for their arguments.
        // Even worse, they all assume that their parameters are word-sized.
        // Currently, none take more than two parameters.
        int[] builtinParamAddresses = new[] { T0, T2 };

        // Define the types of the builtin functions:
        DefineFunction(Builtins.LoadU8, new[] { CType.MakePointer(CType.UInt8) }, CType.UInt8, 0, BuiltinParamAddresses(1));
        DefineFunction(Builtins.LoadU16, new[] { CType.MakePointer(CType.UInt16) }, CType.UInt16, 0, BuiltinParamAddresses(1));
        DefineFunction(Builtins.StoreU8, new[] { CType.MakePointer(CType.UInt8), CType.UInt8 }, CType.UInt8, 0, BuiltinParamAddresses(2));
        DefineFunction(Builtins.StoreU16, new[] { CType.MakePointer(CType.UInt16), CType.UInt16 }, CType.UInt16, 0, BuiltinParamAddresses(2));
        DefineFunction(Builtins.AddU8, new[] { CType.UInt8, CType.UInt8 }, CType.UInt8, 0, BuiltinParamAddresses(2));
        DefineFunction(Builtins.AddU8Ptr, new[] { CType.UInt8Ptr, CType.UInt16 }, CType.UInt8Ptr, 0, BuiltinParamAddresses(2));
        DefineFunction(Builtins.AddU16, new[] { CType.UInt16, CType.UInt16 }, CType.UInt16, 0, BuiltinParamAddresses(2));
        DefineFunction(Builtins.SubtractU8, new[] { CType.UInt8, CType.UInt8 }, CType.UInt8, 0, BuiltinParamAddresses(2));
        DefineFunction(Builtins.SubtractU16, new[] { CType.UInt16, CType.UInt16 }, CType.UInt16, 0, BuiltinParamAddresses(2));
        DefineFunction(Builtins.BoolFromU16, new[] { CType.UInt16 }, CType.UInt8, 0, BuiltinParamAddresses(1));

        LexicalScope globalScope = new LexicalScope();

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
                DefineFunction(decl.Name, paramTypes, decl.Type, 0, addresses);
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
                DefineSymbol(globalScope, SymbolTag.Constant, decl.Name, value, decl.Type);
            }
            else if (decl.Tag == DeclarationTag.Variable)
            {
                DefineVariable(globalScope, decl.Type, decl.Name);
            }
            else if (decl.Tag == DeclarationTag.Struct)
            {
                CField[] fields = new CField[decl.Fields.Count];
                int offset = 0;
                for (int i = 0; i < fields.Length; i++)
                {
                    fields[i] = new CField
                    {
                        Type = decl.Fields[i].Type,
                        Name = decl.Fields[i].Name,
                        Offset = offset,
                    };
                    offset += SizeOf(fields[i].Type);
                }
                StructTypes.Add(decl.Name, new CStructInfo
                {
                    TotalSize = offset,
                    Fields = fields,
                });
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
                CFunctionInfo functionInfo;
                if (!Functions.TryGetValue(decl.Name, out functionInfo)) Program.Panic("this function should already be defined");
                functionInfo.Address = GetCurrentCodeAddress();

                LexicalScope functionScope = PushScope(globalScope);

                // Define each of the function's parameters as a local variable:
                for (int i = 0; i < decl.Fields.Count; i++)
                {
                    NamedField f = decl.Fields[i];
                    DefineSymbol(functionScope, SymbolTag.Variable, f.Name, functionInfo.ParameterAddresses[i], f.Type);
                }

                Expr body = decl.Body;
                body = ReplaceNamedVariables(body, functionScope);
                Program.WriteDebugFile("stage1-replace-vars.txt", Program.ShowExpr(body));
                body = ReplaceAddressOf(body);
                Program.WriteDebugFile("stage2-replace-addressof.txt", Program.ShowExpr(body));
                body = ReplaceGenericFunctions(body);
                Program.WriteDebugFile("stage3-replace-generics.txt", Program.ShowExpr(body));
                CheckTypes(body);
                CompileExpression(body, DestinationDiscard, Continuation.Fallthrough);
                Emit(Opcode.RTS);
            }
        }

        // Fix references to functions:
        foreach (Fixup fixup in Fixups)
        {
            if (fixup.Tag != FixupTag.None)
            {
                CFunctionInfo functionInfo;
                if (!Functions.TryGetValue(fixup.Target, out functionInfo)) Program.Error("function not defined: " + fixup.Target);
                int target = functionInfo.Address;

                if (fixup.Tag != FixupTag.Absolute) Program.Panic("function fixups should always be absolute");
                EmitFix_U16(fixup.Location, target);
                fixup.Tag = FixupTag.None;
            }
        }

        // All fixups should now be fixed.
        if (Fixups.Any(x => x.Tag != FixupTag.None)) Program.Panic("some fixups remain");
    }

    Expr ReplaceNamedVariables(Expr e, LexicalScope scope)
    {
        if (e.Tag == ExprTag.Name)
        {
            Symbol sym;
            if (!FindSymbol(scope, e.Name, out sym)) Program.Error("symbol not defined: {0}", e.Name);
            if (sym.Tag == SymbolTag.Constant)
            {
                // TODO: Make sure the constant value fits in the specified type.
                return Expr.MakeInt(sym.Value, sym.Type);
            }
            else if (sym.Tag == SymbolTag.Variable)
            {
                // Load the variable with a function of appropriate width:
                string loadFunction = null;
                if (SizeOf(sym.Type) == 1) loadFunction = Builtins.LoadU8;
                else if (SizeOf(sym.Type) == 2) loadFunction = Builtins.LoadU16;
                else Program.UnhandledCase();

                return Expr.MakeCall(loadFunction, Expr.MakeInt(sym.Value, CType.MakePointer(sym.Type)));
            }
            Program.UnhandledCase();
            return null;
        }
        else if (e.Tag == ExprTag.Scope)
        {
            Expr arg = e.Args[0];
            e = ReplaceNamedVariables(arg, PushScope(scope));
            // Remove the "scope" node, which isn't needed once all the variables have been replaced:
            return e;
        }
        else if (e.Tag == ExprTag.Local)
        {
            DefineVariable(scope, e.DeclaredType, e.Name);
            // There is no need to keep the declaration node:
            return Expr.MakeEmpty();
        }
        else
        {
            return e.Map(x => ReplaceNamedVariables(x, scope));
        }
    }

    Expr ReplaceAddressOf(Expr e)
    {
        if (e.Tag == ExprTag.AddressOf)
        {
            Expr loadCall = e.Args[0];
            if (loadCall.Tag == ExprTag.Call && LoadFunctions.Contains(loadCall.Name))
            {
                return ReplaceAddressOf(loadCall.Args[0]);
            }

            // TODO: Show the appropriate one of these two error messages:
            Program.Error("cannot take address of this expression, or, cannot assign to constants");
            return null;
        }
        else if (e.Tag == ExprTag.Name || e.Tag == ExprTag.Scope || e.Tag == ExprTag.Local)
        {
            InvalidNode(e);
            return null;
        }
        else
        {
            return e.Map(ReplaceAddressOf);
        }
    }

    Expr ReplaceGenericFunctions(Expr e)
    {
        if (e.Tag == ExprTag.Call)
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
        else if (e.Tag == ExprTag.Name || e.Tag == ExprTag.Scope || e.Tag == ExprTag.Local)
        {
            InvalidNode(e);
            return null;
        }
        else
        {
            return e.Map(ReplaceGenericFunctions);
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
        if (e.Tag == ExprTag.Empty)
        {
            // NOP
        }
        else if (e.Tag == ExprTag.Int)
        {
            // TODO: This should have had a specific type assigned to it by now.
        }
        else if (e.Tag == ExprTag.Name)
        {
            // NOP
        }
        else if (e.Tag == ExprTag.Scope)
        {
            InvalidNode(e);
        }
        else if (e.Tag == ExprTag.Sequence)
        {
            foreach (Expr sub in e.Args) CheckTypes(sub);
        }
        else if (e.Tag == ExprTag.Local)
        {
            InvalidNode(e);
        }
        else if (e.Tag == ExprTag.AddressOf)
        {
            InvalidNode(e);
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
        else if (e.Tag == ExprTag.Cast)
        {
            CheckTypes(e.Args[0]);
        }
        else if (e.Tag == ExprTag.StructCast)
        {
            CheckTypes(e.Args[0]);
            CheckTypes(e.Args[1]);
        }
        else if (e.Tag == ExprTag.OffsetOf)
        {
            CheckTypes(e.Args[0]);
        }
        else if (e.Tag == ExprTag.Call)
        {
            CFunctionInfo functionInfo;
            if (!Functions.TryGetValue(e.Name, out functionInfo)) Program.Error("function not defined: {0}", e.Name);
            if (functionInfo.ParameterTypes.Length != e.Args.Length) Program.Error("wrong number of arguments to function: {0}", e.Name);
            // Check that each of the actual and expected parameter types match:
            for (int i = 0; i < e.Args.Length; i++)
            {
                Expr arg = e.Args[i];
                CheckTypes(arg);
                CType argType = TypeOf(arg);
                if (!TypesEqual(argType, functionInfo.ParameterTypes[i])) Program.Error("argument to function has wrong type");
            }
        }
        else
        {
            Program.Panic("type checker: unhandled case");
        }
    }

    CType TypeOf(Expr e)
    {
        if (e.Tag == ExprTag.Empty)
        {
            return CType.Void;
        }
        else if (e.Tag == ExprTag.Int)
        {
            return e.DeclaredType;
        }
        else if (e.Tag == ExprTag.Name)
        {
            InvalidNode(e);
            return null;
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
        else if (e.Tag == ExprTag.Cast)
        {
            return e.DeclaredType;
        }
        else if (e.Tag == ExprTag.StructCast)
        {
            CField field = GetFieldInfo(e.Args[0], e.Name);
            return CType.MakePointer(field.Type);
        }
        else if (e.Tag == ExprTag.OffsetOf)
        {
            // Field offsets are always measured in bytes:
            return CType.UInt16;
        }
        else if (e.Tag == ExprTag.Call)
        {
            CFunctionInfo functionInfo;
            if (!Functions.TryGetValue(e.Name, out functionInfo)) Program.Error("function not defined: {0}", e.Name);
            return functionInfo.ReturnType;
        }
        else
        {
            Program.Panic("type of: unhandled case");
            return null;
        }
    }

    CStructInfo GetStructInfo(string name)
    {
        CStructInfo info;
        if (!StructTypes.TryGetValue(name, out info)) Program.Error("struct not defined: {0}", name);
        return info;
    }

    CField GetFieldInfo(Expr structExpr, string fieldName)
    {
        CType structType = TypeOf(structExpr);
        if (structType.Tag != CTypeTag.Struct) Program.Error("left side must have struct type");
        CStructInfo structInfo = GetStructInfo(structType.Name);
        CField field = structInfo.Fields.FirstOrDefault(x => x.Name == fieldName);
        if (field == null) Program.Error("type does not contain a field with this name: {0}", fieldName);
        return field;
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
        else if (e.Tag == ExprTag.Empty)
        {
            // NOP
        }
        else if (e.Tag == ExprTag.Name)
        {
            InvalidNode(e);
        }
        else if (e.Tag == ExprTag.Scope)
        {
            InvalidNode(e);
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
            InvalidNode(e);
        }
        else if (e.Tag == ExprTag.AddressOf)
        {
            InvalidNode(e);
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
            CompileExpression(init, DestinationDiscard, Continuation.Fallthrough);
            EmitComment("for loop");
            int top = GetCurrentCodeAddress();
            CompileExpression(test, DestinationDiscard, new Continuation(JumpCondition.IfFalse, bottom));
            CompileExpression(body, DestinationDiscard, Continuation.Fallthrough);
            CompileExpression(induction, DestinationDiscard, Continuation.Fallthrough);
            EmitJump(top);
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
        else if (e.Tag == ExprTag.Cast)
        {
            CompileExpression(e.Args[0], dest, cont);
        }
        else if (e.Tag == ExprTag.StructCast)
        {
            CompileExpression(e.Args[1], dest, cont);
        }
        else if (e.Tag == ExprTag.OffsetOf)
        {
            Program.Panic("field offsets should be calculated as constants");
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

                CFunctionInfo functionInfo;
                if (!Functions.TryGetValue(e.Name, out functionInfo)) Program.Error("function not defined: " + e.Name);

                // Get the call frame address (and type information) from the function's type entry.
                CType[] paramTypes = functionInfo.ParameterTypes;
                int[] paramAddresses = functionInfo.ParameterAddresses;

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
                else if (e.Name == Builtins.AddU16 || e.Name == Builtins.AddU8Ptr)
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
                    // JSR to the function:
                    Emit_U16(Opcode.JSR, 0);
                    Fixups.Add(new Fixup(FixupTag.Absolute, GetCurrentCodeAddress() - 2, e.Name));
                }

                // The return value is placed in the accumulator.
                EmitStoreAcc(functionInfo.ReturnType, dest);
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
        else if (e.Tag == ExprTag.OffsetOf)
        {
            CField field = GetFieldInfo(e.Args[0], e.Name);
            value = field.Offset;
            type = field.Type;
            return true;
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

    void DefineVariable(LexicalScope scope, CType type, string name)
    {
        int address = AllocGlobal(SizeOf(type));
        DefineSymbol(scope, SymbolTag.Variable, name, address, type);
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

    int SizeOf(CType type)
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
        else if (type.Tag == CTypeTag.Struct)
        {
            CStructInfo info = GetStructInfo(type.Name);
            return info.TotalSize;
        }

        Program.NYI();
        return 1;
    }

    string GetLoadFunctionForType(CType type)
    {
        int width = SizeOf(type);
        if (width == 1) return Builtins.LoadU8;
        else if (width == 2) return Builtins.LoadU16;
        Program.NYI();
        return null;
    }

    void DefineFunction(string name, CType[] paramTypes, CType returnType, int address, int[] paramAddresses)
    {
        Functions.Add(name, new CFunctionInfo
        {
            ParameterTypes = paramTypes,
            ReturnType = returnType,
            Address = address,
            ParameterAddresses = paramAddresses,
        });
    }

    void DefineSymbol(LexicalScope scope, SymbolTag tag, string name, int value, CType type)
    {
        // It is an error to define two things with the same name in the same scope.
        if (scope.Symbols.Any(x => x.Name == name))
        {
            Program.Error("symbols cannot be redefined: {0}", name);
        }

        scope.Symbols.Add(new Symbol
        {
            Tag = tag,
            Name = name,
            Value = value,
            Type = type,
        });
    }

    bool FindSymbol(LexicalScope scope, string name, out Symbol found)
    {
        // Inspect all scopes, starting from the innermost.
        for (; scope != null; scope = scope.Outer)
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

    static LexicalScope PushScope(LexicalScope scope)
    {
        LexicalScope inner = new LexicalScope();
        inner.Outer = scope;
        return inner;
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
        Program.UnhandledCase();
        return null;
    }

    static void InvalidNode(Expr e) => Program.Panic("invalid '{0}' node encountered", e.Tag);
}

[DebuggerDisplay("{Tag} {Name} = 0x{Value,h} ({Type.Show(),nq})")]
class Symbol
{
    public SymbolTag Tag;
    public string Name;
    public int Value;
    public CType Type;
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

// One of:
// - SimpleType
// - Pointer subtype
// - Struct name
partial class CType
{
    public CTypeTag Tag;
    public CSimpleType SimpleType;
    public string Name;
    public CType Subtype;
}

enum CTypeTag
{
    Simple,
    Pointer,
    Struct,
}

enum CSimpleType
{
    Void,
    UInt8,
    UInt16,
}

class CStructInfo
{
    public int TotalSize;
    public CField[] Fields;
}

class CField
{
    public CType Type;
    public string Name;
    public int Offset;
}

partial class CFunctionInfo
{
    public CType[] ParameterTypes;
    public CType ReturnType;
    public int Address;
    public int[] ParameterAddresses;
}

[DebuggerDisplay("{Show(),nq}")]
partial class CType
{
    public static readonly CType Void = MakeSimple(CSimpleType.Void);
    public static readonly CType UInt8 = MakeSimple(CSimpleType.UInt8);
    public static readonly CType UInt8Ptr = MakePointer(UInt8);
    public static readonly CType UInt16 = MakeSimple(CSimpleType.UInt16);

    public static CType MakeSimple(CSimpleType simple)
    {
        return new CType
        {
            Tag = CTypeTag.Simple,
            SimpleType = simple,
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

    public static CType MakeStruct(string name)
    {
        return new CType
        {
            Tag = CTypeTag.Struct,
            Name = name,
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
        else if (Tag == CTypeTag.Struct) return "struct " + Name;
        else throw new NotImplementedException();
    }
}

[DebuggerDisplay("{Show(),nq}")]
partial class CFunctionInfo
{
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
        var paramTypes = ParameterTypes.Select(x => x.Show());
        return string.Format("function({0}) {1}", string.Join(", ", paramTypes), ReturnType.Show());
    }
}

class LexicalScope
{
    public LexicalScope Outer;
    public List<Symbol> Symbols = new List<Symbol>();
}
