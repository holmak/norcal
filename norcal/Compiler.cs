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
        Tag.LoadU8,
        Tag.LoadU16,
    };

    public void CompileProgram(List<Declaration> program)
    {
        // HACK: Many functions are currently just defined in the compiler, and they use
        // a fixed set of addresses for their arguments.
        // Even worse, they all assume that their parameters are word-sized.
        // Currently, none take more than two parameters.
        int[] builtinParamAddresses = new[] { T0, T2 };

        // Define the types of the builtin functions:
        DefineFunction(Tag.LoadU8, new[] { CType.MakePointer(CType.UInt8) }, CType.UInt8, 0, BuiltinParamAddresses(1));
        DefineFunction(Tag.LoadU16, new[] { CType.MakePointer(CType.UInt16) }, CType.UInt16, 0, BuiltinParamAddresses(1));
        DefineFunction(Tag.StoreU8, new[] { CType.MakePointer(CType.UInt8), CType.UInt8 }, CType.UInt8, 0, BuiltinParamAddresses(2));
        DefineFunction(Tag.StoreU16, new[] { CType.MakePointer(CType.UInt16), CType.UInt16 }, CType.UInt16, 0, BuiltinParamAddresses(2));
        DefineFunction(Tag.AddU8, new[] { CType.UInt8, CType.UInt8 }, CType.UInt8, 0, BuiltinParamAddresses(2));
        DefineFunction(Tag.AddU8Ptr, new[] { CType.UInt8Ptr, CType.UInt16 }, CType.UInt8Ptr, 0, BuiltinParamAddresses(2));
        DefineFunction(Tag.AddU16, new[] { CType.UInt16, CType.UInt16 }, CType.UInt16, 0, BuiltinParamAddresses(2));
        DefineFunction(Tag.SubtractU8, new[] { CType.UInt8, CType.UInt8 }, CType.UInt8, 0, BuiltinParamAddresses(2));
        DefineFunction(Tag.SubtractU16, new[] { CType.UInt16, CType.UInt16 }, CType.UInt16, 0, BuiltinParamAddresses(2));
        DefineFunction(Tag.BoolFromU16, new[] { CType.UInt16 }, CType.UInt8, 0, BuiltinParamAddresses(1));

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
                Program.WritePassOutputToFile("replace-vars", body.ShowMultiline());
                body = ReplaceFields(body);
                Program.WritePassOutputToFile("replace-fields", body.ShowMultiline());
                body = ReplaceAddressOf(body);
                Program.WritePassOutputToFile("replace-addressof", body.ShowMultiline());
                body = ReplaceGenericFunctions(body);
                Program.WritePassOutputToFile("replace-generics", body.ShowMultiline());
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
        string name;
        Expr body;
        CType type;
        if (e.Match(Tag.Name, out name))
        {
            Symbol sym;
            if (!FindSymbol(scope, name, out sym)) Program.Error("symbol not defined: {0}", name);
            if (sym.Tag == SymbolTag.Constant)
            {
                // TODO: Make sure the constant value fits in the specified type.
                return Expr.Make(Tag.Int, sym.Value, sym.Type);
            }
            else if (sym.Tag == SymbolTag.Variable)
            {
                Expr address = Expr.Make(Tag.Int, sym.Value, CType.MakePointer(sym.Type));
                return Expr.Make(Tag.LoadGeneric, address);
            }
            Program.UnhandledCase();
            return null;
        }
        else if (e.Match(Tag.Scope, out body))
        {
            // Remove the "scope" node, which isn't needed once all the variables have been replaced:
            return ReplaceNamedVariables(body, PushScope(scope));
        }
        else if (e.Match(Tag.Local, out type, out name))
        {
            DefineVariable(scope, type, name);
            // There is no need to keep the declaration node:
            return Expr.Make(Tag.Empty);
        }
        else
        {
            return e.Map(x => ReplaceNamedVariables(x, scope));
        }
    }

    Expr ReplaceAddressOf(Expr e)
    {
        Expr lvalue;
        if (e.Match(Tag.AddressOf, out lvalue))
        {
            Expr addressExpr;
            if (lvalue.Match(Tag.LoadGeneric, out addressExpr))
            {
                return ReplaceAddressOf(addressExpr);
            }

            // TODO: Show the appropriate one of these two error messages:
            Program.Error("cannot take address of this expression, or, cannot assign to constants");
            return null;
        }
        else
        {
            ReportInvalidNodes(e, Tag.Name, Tag.Scope, Tag.Local);
            return e.Map(ReplaceAddressOf);
        }
    }

    Expr ReplaceFields(Expr e)
    {
        Expr left;
        string fieldName;
        if (e.Match(Tag.Field, out left, out fieldName))
        {
            left = ReplaceFields(left);

            CType structType = TypeOf(left);
            if (structType.Tag != CTypeTag.Struct) Program.Error("left side must have struct type");
            CStructInfo structInfo = GetStructInfo(structType.Name);
            CField fieldInfo = structInfo.Fields.FirstOrDefault(x => x.Name == fieldName);
            if (fieldInfo == null) Program.Error("type does not contain a field with this name: {0}", fieldName);

            return Expr.Make(
                Tag.Cast,
                CType.MakePointer(fieldInfo.Type),
                Expr.Make(
                    Tag.AddU16,
                    Expr.Make(
                        Tag.Cast,
                        CType.UInt8Ptr,
                        Expr.Make(Tag.AddressOf, left)),
                    Expr.Make(
                        Tag.Int,
                        fieldInfo.Offset,
                        CType.UInt16)));
        }
        else
        {
            return e.Map(ReplaceFields);
        }
    }

    Expr ReplaceGenericFunctions(Expr e)
    {
        // Recursively apply this pass to all arguments:
        e = e.Map(ReplaceGenericFunctions);

        Expr left, right;
        if (e.Match(Tag.LoadGeneric, out left))
        {
            CType addressType = TypeOf(left);
            if (addressType.Tag != CTypeTag.Pointer) Program.Error("load address must have pointer type");
            CType returnType = addressType.Subtype;

            string specificName = null;
            if (TypesEqual(returnType, CType.UInt8)) specificName = Tag.LoadU8;
            else if (TypesEqual(returnType, CType.UInt16)) specificName = Tag.LoadU16;
            else Program.NYI();

            return Expr.Make(specificName, left);
        }
        else if (e.Match(Tag.StoreGeneric, out left, out right))
        {
            CType leftType = TypeOf(left);

            CType addressType = TypeOf(left);
            if (addressType.Tag != CTypeTag.Pointer) Program.Error("store address must have pointer type");
            CType expectedTypeOfValue = addressType.Subtype;

            right = ChangeTypeIfPossible(right, expectedTypeOfValue);
            CType actualType = TypeOf(right);
            if (!TypesEqual(expectedTypeOfValue, actualType)) Program.Error("types in assignment must match");

            string specificName = null;
            if (TypesEqual(actualType, CType.UInt8)) specificName = Tag.StoreU8;
            else if (TypesEqual(actualType, CType.UInt16)) specificName = Tag.StoreU16;
            else Program.NYI();

            return Expr.Make(specificName, left, right);
        }
        else if (e.Match(Tag.AddGeneric, out left, out right))
        {
            CType leftType = TypeOf(left);

            right = ChangeTypeIfPossible(right, leftType);
            if (!TypesEqual(leftType, TypeOf(right))) Program.Error("types in binary expression must match");

            string specificName = null;
            if (TypesEqual(leftType, CType.UInt8)) specificName = Tag.AddU8;
            else if (TypesEqual(leftType, CType.UInt16)) specificName = Tag.AddU16;
            else Program.NYI();

            return Expr.Make(specificName, left, right);
        }
        else if (e.Match(Tag.SubtractGeneric, out left, out right))
        {
            CType leftType = TypeOf(left);

            right = ChangeTypeIfPossible(right, leftType);
            if (!TypesEqual(leftType, TypeOf(right))) Program.Error("types in binary expression must match");

            string specificName = null;
            if (TypesEqual(leftType, CType.UInt8)) specificName = Tag.SubtractU8;
            else if (TypesEqual(leftType, CType.UInt16)) specificName = Tag.SubtractU16;
            else Program.NYI();

            return Expr.Make(specificName, left, right);
        }
        else if (e.Match(Tag.BoolFromGeneric, out left))
        {
            CType sourceType = TypeOf(left);

            string specificName = null;
            if (TypesEqual(sourceType, CType.UInt16)) specificName = Tag.BoolFromU16;
            else Program.NYI();

            return Expr.Make(specificName, left);
        }
        else
        {
            ReportInvalidNodes(e, Tag.Name, Tag.Scope, Tag.Local);
            return e;
        }
    }

    /// <summary>
    /// It is possible to interpret some expressions, such as integer literals, as any of several types.
    /// If possible, change the type of the given expression to the expected type.
    /// </summary>
    Expr ChangeTypeIfPossible(Expr e, CType expected)
    {
        // Make sure the actual value is small enough to fit.
        int value;
        CType type;
        if (TypesEqual(expected, CType.UInt8) && e.Match(Tag.Int, out value, out type))
        {
            if (value >= 0 && value <= byte.MaxValue)
            {
                return Expr.Make(Tag.Int, value, CType.UInt8);
            }
        }

        return e;
    }

    void CheckTypes(Expr e)
    {
        ReportInvalidNodes(e, Tag.Scope, Tag.Local, Tag.AddressOf);

        string name;
        int value;
        CType type;
        Expr[] args;
        Expr arg;

        // Typecheck each subexpression:
        if (e.MatchAny(out name, out args))
        {
            foreach (Expr sub in args.OfType<Expr>())
            {
                CheckTypes(sub);
            }
        }

        if (e.Match(Tag.Int, out value, out type))
        {
            // This should have had a specific type assigned to it by now:
            if (TypesEqual(type, CType.Implied)) Program.Panic("integer literal has not been given a type");
        }
        else if (e.MatchAny(Tag.Switch, out args))
        {
            // TODO: Each condition must have type "uint8_t".
            // TODO: Each then-clause must have the same type.
        }
        else if (e.Match(Tag.For))
        {
            // TODO: Are there any special rules?
        }
        else if (e.Match(Tag.Return, out arg))
        {
            CType actual = TypeOf(arg);
            // TODO: Get the current function's declared return type.
            CType expected = CType.UInt16;
            if (!TypesEqual(actual, expected)) Program.Error("incorrect return type");
        }
        else if (e.Match(Tag.Cast, out type, out arg))
        {
            // TODO: Make sure that the cast is allowed.
        }
        else if (e.MatchAny(out name, out args))
        {
            // Don't try to typecheck special AST nodes, which start with "$".
            if (!name.StartsWith("$"))
            {
                CFunctionInfo functionInfo;
                if (!Functions.TryGetValue(name, out functionInfo)) Program.Error("function not defined: {0}", name);
                if (functionInfo.ParameterTypes.Length != args.Length) Program.Error("wrong number of arguments to function: {0}", name);
                // Check that each of the actual and expected parameter types match:
                for (int i = 0; i < args.Length; i++)
                {
                    CType argType = TypeOf(args[i]);
                    if (!TypesEqual(argType, functionInfo.ParameterTypes[i])) Program.Error("argument to function has wrong type");
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
        ReportInvalidNodes(e, Tag.Name, Tag.Local);

        int value;
        string name;
        CType type;
        Expr body, subexpr;
        Expr[] args;
        if (e.Match(Tag.Empty))
        {
            return CType.Void;
        }
        else if (e.Match(Tag.Int, out value, out type))
        {
            return type;
        }
        else if (e.Match(Tag.Scope, out body))
        {
            return TypeOf(body);
        }
        else if (e.MatchAny(Tag.Sequence, out args))
        {
            if (args.Length > 0) return TypeOf(args.Last());
            else return CType.Void;
        }
        else if (e.Match(Tag.AddressOf, out subexpr))
        {
            return CType.MakePointer(TypeOf(subexpr));
        }
        else if (e.MatchAny(Tag.Switch, out args))
        {
            // TODO: Figure out the type.
            return CType.Void;
        }
        else if (e.MatchTag(Tag.For) || e.MatchTag(Tag.Return))
        {
            return CType.Void;
        }
        else if (e.Match(Tag.Cast, out type, out subexpr))
        {
            return type;
        }
        else if (e.Match(Tag.LoadGeneric, out subexpr))
        {
            CType innerType = TypeOf(subexpr);
            if (innerType.Tag != CTypeTag.Pointer) Program.Error("an expression with pointer type is required");
            return innerType.Subtype;
        }
        else if (e.MatchAny(out name, out args))
        {
            CFunctionInfo functionInfo;
            if (!Functions.TryGetValue(name, out functionInfo)) Program.Error("function not defined: {0}", name);
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
        string functionName;
        CType type;
        Expr[] args;
        Expr test, then, init, induction, body, subexpr;
        if (EvaluateConstantExpression(e, out value, out type))
        {
            EmitLoadImmediate(value, type, dest, cont);
        }
        else if (e.Match(Tag.Empty))
        {
            // NOP
        }
        else if (e.MatchAny(Tag.Sequence, out args))
        {
            for (int i = 0; i < args.Length; i++)
            {
                EmitComment("begin new statement");
                // Drop the result of each expression except the last.
                bool isLast = (i == args.Length - 1);
                if (!isLast)
                {
                    CompileExpression(args[i], DestinationDiscard, Continuation.Fallthrough);
                }
                else
                {
                    CompileExpression(args[i], dest, cont);
                }
            }
        }
        else if (e.Match(Tag.Switch, out test, out then))
        {
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
        else if (e.Match(Tag.For, out init, out test, out induction, out body))
        {
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
        else if (e.Match(Tag.Return, out subexpr))
        {
            EmitComment("return");
            CompileExpression(subexpr, DestinationAcc, Continuation.Fallthrough);
            Emit(Opcode.RTS);
        }
        else if (e.Match(Tag.Cast, out type, out subexpr))
        {
            // TODO: Shouldn't casts be removed before now?
            CompileExpression(subexpr, dest, cont);
        }
        else if (e.MatchAny(out functionName, out args))
        {
            int addr;
            if ((functionName == Tag.LoadU8 || functionName == Tag.LoadU16) && EvaluateConstantExpression(args[0], out addr, out type))
            {
                // Loads from constant addresses must be optimized, because this pattern is used to copy data from
                // temporary variables into call frames; we can't generate a call to "load_***" in the middle of
                // generating some other call.

                EmitLoad(addr, type, dest, cont);
            }
            else if ((functionName == Tag.StoreU8 || functionName == Tag.StoreU16) && EvaluateConstantExpression(args[0], out addr, out type))
            {
                // This case is not essential, but generates better code.

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
                    EmitStoreAcc(type, addr);
                    EmitStoreAcc(type, dest);
                    EmitBranchOnAcc(cont);
                }
            }
            else
            {
                // This is the general-purpose way of calling functions.

                CFunctionInfo functionInfo;
                if (!Functions.TryGetValue(functionName, out functionInfo)) Program.Error("function not defined: " + functionName);

                // Get the call frame address (and type information) from the function's type entry.
                CType[] paramTypes = functionInfo.ParameterTypes;
                int[] paramAddresses = functionInfo.ParameterAddresses;

                if (paramTypes.Length != paramAddresses.Length) Program.Panic("in function symbol, the number of types doesn't match the number of argument locations: " + functionName);
                if (args.Length != paramTypes.Length) Program.Error("wrong number of arguments to function: " + functionName);

                BeginTempScope();
                EmitComment("prepare call: function " + functionName);

                // Evaluate the arguments and store the results in temporary variables.
                // TODO: (optimization) The first arg doesn't have to be simplified, since we haven't started assembling a call frame yet.
                // Optimization: Sufficiently simple arguments (such as literal ints) can skip this
                //   step and be written directly into the call frame.
                List<Expr> temps = new List<Expr>();
                for (int i = 0; i < paramTypes.Length; i++)
                {
                    Expr arg = args[i];
                    CType argType = paramTypes[i];

                    int n;
                    CType ignored;
                    Expr simpleArg;
                    if (EvaluateConstantExpression(arg, out n, out ignored))
                    {
                        simpleArg = Expr.Make(Tag.Int, n, argType);
                    }
                    else
                    {
                        EmitComment("prepare call: create temporary for argument " + i);
                        int argSize = SizeOf(argType);
                        int temp = AllocTemp(argSize);
                        CompileExpression(arg, temp, Continuation.Fallthrough);
                        simpleArg = Expr.Make(GetLoadFunctionForType(argType), Expr.Make(Tag.Int, temp, argType));
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
                EmitComment("function body: " + functionName);
                if (functionName == Tag.LoadU16)
                {
                    if (args.Length != 1) Program.Panic("wrong number of arguments to unary operator");
                    // TODO: This would be more efficient if it loaded the high byte first.
                    Emit_U8(Opcode.LDY_IMM, 0);
                    Emit_U8(Opcode.LDA_ZP_Y_IND, T0);
                    Emit_U8(Opcode.STA_ZP, T2);
                    Emit(Opcode.INY);
                    Emit_U8(Opcode.LDA_ZP_Y_IND, T0);
                    Emit(Opcode.TAX);
                    Emit_U8(Opcode.LDA_ZP, T2);
                }
                else if (functionName == Tag.AddU8)
                {
                    if (args.Length != 2) Program.Panic("wrong number of arguments to binary operator");
                    Emit(Opcode.CLC);
                    Emit_U8(Opcode.LDA_ZP, T0);
                    Emit_U8(Opcode.ADC_ZP, T2);
                }
                else if (functionName == Tag.AddU16 || functionName == Tag.AddU8Ptr)
                {
                    if (args.Length != 2) Program.Panic("wrong number of arguments to binary operator");
                    Emit(Opcode.CLC);
                    Emit_U8(Opcode.LDA_ZP, T0);
                    Emit_U8(Opcode.ADC_ZP, T2);
                    Emit_U8(Opcode.STA_ZP, T0);
                    Emit_U8(Opcode.LDA_ZP, T1);
                    Emit_U8(Opcode.ADC_ZP, T3);
                    Emit(Opcode.TAX);
                    Emit_U8(Opcode.LDA_ZP, T0);
                }
                else if (functionName == Tag.SubtractU8)
                {
                    if (args.Length != 2) Program.Panic("wrong number of arguments to binary operator");
                    Emit(Opcode.SEC);
                    Emit_U8(Opcode.LDA_ZP, T0);
                    Emit_U8(Opcode.SBC_ZP, T2);
                }
                else if (functionName == Tag.SubtractU16)
                {
                    if (args.Length != 2) Program.Panic("wrong number of arguments to binary operator");
                    Emit(Opcode.SEC);
                    Emit_U8(Opcode.LDA_ZP, T0);
                    Emit_U8(Opcode.SBC_ZP, T2);
                    Emit_U8(Opcode.STA_ZP, T0);
                    Emit_U8(Opcode.LDA_ZP, T1);
                    Emit_U8(Opcode.SBC_ZP, T3);
                    Emit(Opcode.TAX);
                    Emit_U8(Opcode.LDA_ZP, T0);
                }
                else if (functionName == Tag.StoreU16)
                {
                    if (args.Length != 2) Program.Panic("wrong number of arguments to binary operator");
                    Emit_U8(Opcode.LDY_IMM, 0);
                    Emit_U8(Opcode.LDA_ZP, T2);
                    Emit_U8(Opcode.STA_ZP_Y_IND, T0);
                    Emit(Opcode.INY);
                    Emit_U8(Opcode.LDA_ZP, T3);
                    Emit_U8(Opcode.STA_ZP_Y_IND, T0);
                }
                else if (functionName == Tag.StoreU8)
                {
                    if (args.Length != 2) Program.Panic("wrong number of arguments to binary operator");
                    Emit_U8(Opcode.LDY_IMM, 0);
                    Emit_U8(Opcode.LDA_ZP, T2);
                    Emit_U8(Opcode.STA_ZP_Y_IND, T0);
                }
                else if (functionName == Tag.BoolFromU16)
                {
                    if (args.Length != 1) Program.Panic("wrong number of arguments to unary operator");
                    Emit_U8(Opcode.LDA_ZP, T0);
                    Emit_U8(Opcode.ORA_ZP, T1);
                }
                else
                {
                    // JSR to the function:
                    Emit_U16(Opcode.JSR, 0);
                    Fixups.Add(new Fixup(FixupTag.Absolute, GetCurrentCodeAddress() - 2, functionName));
                }

                // The return value is placed in the accumulator.
                EmitStoreAcc(functionInfo.ReturnType, dest);
                EmitBranchOnAcc(cont);

                EndTempScope();
            }
        }
        else
        {
            Program.UnhandledCase();
        }
    }

    // Return true if the expression was constant, and therefore able to be evaluated.
    bool EvaluateConstantExpression(Expr e, out int value, out CType type)
    {
        // TODO: Evaluate more complex constant expressions, too.
        // TODO: Should this be partly or entirely replaced by a constant-folding pass?
        if (e.Match(Tag.Int, out value, out type))
        {
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
        if (width == 1) return Tag.LoadU8;
        else if (width == 2) return Tag.LoadU16;
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

    static void ReportInvalidNodes(Expr e, params string[] tags)
    {
        foreach (string tag in tags)
        {
            if (e.MatchTag(tag))
            {
                Program.Panic("invalid '{0}' node encountered", tag);
            }
        }
    }
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
    Implied,
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
    /// <summary>
    /// This means that the typechecker hasn't assigned a type to this node yet.
    /// </summary>
    public static readonly CType Implied = MakeSimple(CSimpleType.Implied);

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
