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

    public void CompileProgram(List<Declaration> program)
    {
        // HACK: Many functions are currently just defined in the compiler, and they use
        // a fixed set of addresses for their arguments.
        // Even worse, they all assume that their parameters are word-sized.
        // Currently, none take more than two parameters.
        int[] builtinParamAddresses = new[] { T0, T2 };

        // Define the types of the builtin functions:
        DeclareFunction(Tag.LoadU8, new[] { CType.MakePointer(CType.UInt8) }, CType.UInt8, BuiltinParamAddresses(1));
        DeclareFunction(Tag.LoadU16, new[] { CType.MakePointer(CType.UInt16) }, CType.UInt16, BuiltinParamAddresses(1));
        DeclareFunction(Tag.StoreU8, new[] { CType.MakePointer(CType.UInt8), CType.UInt8 }, CType.UInt8, BuiltinParamAddresses(2));
        DeclareFunction(Tag.StoreU16, new[] { CType.MakePointer(CType.UInt16), CType.UInt16 }, CType.UInt16, BuiltinParamAddresses(2));
        DeclareFunction(Tag.LessThanU8, new[] { CType.UInt8, CType.UInt8 }, CType.UInt8, BuiltinParamAddresses(2));
        DeclareFunction(Tag.LessThanU16, new[] { CType.UInt16, CType.UInt16 }, CType.UInt16, BuiltinParamAddresses(2));
        DeclareFunction(Tag.GreaterThanU8, new[] { CType.UInt8, CType.UInt8 }, CType.UInt8, BuiltinParamAddresses(2));
        DeclareFunction(Tag.GreaterThanU16, new[] { CType.UInt16, CType.UInt16 }, CType.UInt16, BuiltinParamAddresses(2));
        DeclareFunction(Tag.BoolFromU16, new[] { CType.UInt16 }, CType.UInt8, BuiltinParamAddresses(1));

        // HACK: If you don't define an interrupt handler, it will target address zero.
        // TODO: What should the compiler do if an interrupt handler isn't defined? Is it an error?
        Emit(Asm.Label, "nmi");
        Emit(Asm.Label, "reset");
        Emit(Asm.Label, "brk");

        // Pass: Declare global symbols and replace symbols with addresses
        program = ApplyFirstPass(program);

        // Simplification passes:
        program = ApplyPass("replace-fields", ReplaceFields, program);
        program = ApplyPass("replace-address-of", ReplaceAddressOf, program);
        program = ApplyPass("replace-generics", ReplaceGenericFunctions, program);

        // Pass: Typechecking
        foreach (Declaration decl in program)
        {
            if (decl.Tag == DeclarationTag.Function)
            {
                CFunctionInfo functionInfo = GetFunctionInfo(decl.Name);
                CheckTypes(decl.Body, functionInfo.ReturnType);
            }
        }

        // Post-typechecking passes:
        program = ApplyPass("fold-constants", SimplifyConstantExpressions, program);
        program = ApplyPass("simplify-casts", SimplifyCasts, program);

        // Pass: Codegen
        foreach (Declaration decl in program)
        {
            if (decl.Tag == DeclarationTag.Function)
            {
                Emit(Asm.Function, decl.Name);
                CompileExpression(decl.Body, DestinationDiscard, Continuation.Fallthrough);
                Emit("RTS");
            }
        }

        // TODO: Make sure interrupt-handling functions have the right type signature and are defined.

        // Put the interrupt vector table at the end of ROM:
        Emit(Asm.SkipTo, 0xFFFA);
        Emit(Asm.Word, "nmi");
        Emit(Asm.Word, "reset");
        Emit(Asm.Word, "brk");

        // Show the final assembly code:
        StringBuilder sb = new StringBuilder();
        foreach (Expr e in Assembly)
        {
            string line;

            string mnemonic, text;
            int operand;
            if (e.Match(Asm.Comment, out text)) line = "\t; " + text;
            else if (e.Match(Asm.Label, out text)) line = text + ":";
            else if (e.Match(Asm.Function, out text)) line = string.Format("function {0}:", text);
            else if (e.Match(out mnemonic)) line = string.Format("\t{0}", mnemonic);
            else if (e.Match(out mnemonic, out operand)) line = string.Format("\t{0} ${1:X}", mnemonic, operand);
            else if (e.Match(out mnemonic, out text)) line = string.Format("\t{0} {1}", mnemonic, text);
            else line = '\t' + e.Show();

            sb.AppendLine(line);
        }
        Program.WritePassOutputToFile("assembly", sb.ToString());
    }

    List<Declaration> ApplyFirstPass(List<Declaration> program)
    {
        LexicalScope globalScope = new LexicalScope();

        List<Declaration> newProgram = new List<Declaration>();
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
                DeclareFunction(decl.Name, paramTypes, decl.Type, addresses);
                EmitComment("declare function " + decl.Name);

                // Define each of the function's parameters as a local variable:
                LexicalScope functionScope = PushScope(globalScope);
                for (int i = 0; i < decl.Fields.Count; i++)
                {
                    NamedField f = decl.Fields[i];
                    DefineSymbol(functionScope, SymbolTag.Variable, f.Name, addresses[i], f.Type);
                }

                newProgram.Add(new Declaration
                {
                    Tag = decl.Tag,
                    Type = decl.Type,
                    Name = decl.Name,
                    Body = ReplaceNamedVariables(decl.Body, functionScope),
                    Fields = decl.Fields,
                });
            }
            else if (decl.Tag == DeclarationTag.Constant)
            {
                // TODO: Make sure the declared type matches the actual type.
                CType type;
                int value;
                if (!decl.Body.Match(Tag.Int, out value, out type))
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
                Program.UnhandledCase();
            }
        }
        Program.WritePassOutputToFile("replace-vars", newProgram);
        return newProgram;
    }

    /// <summary>
    /// This overload is for the majority of passes which don't need to know the function's return type.
    /// </summary>
    List<Declaration> ApplyPass(string passName, Func<Expr, Expr> apply, List<Declaration> program)
    {
        return ApplyPass(passName, (expr, returnType) => apply(expr), program);
    }

    List<Declaration> ApplyPass(string passName, Func<Expr, CType, Expr> apply, List<Declaration> program)
    {
        List<Declaration> newProgram = new List<Declaration>();
        foreach (Declaration decl in program)
        {
            if (decl.Tag == DeclarationTag.Function)
            {
                newProgram.Add(new Declaration
                {
                    Tag = decl.Tag,
                    Type = decl.Type,
                    Name = decl.Name,
                    Body = apply(decl.Body, decl.Type),
                    Fields = decl.Fields,
                });
            }
            else
            {
                newProgram.Add(decl);
            }
        }
        Program.WritePassOutputToFile(passName, newProgram);
        return newProgram;
    }

    Expr ReplaceNamedVariables(Expr e, LexicalScope scope)
    {
        string name, mnemonic;
        Expr body, operand;
        CType type;
        int offset;
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
        else if (e.Match(Tag.Asm, out mnemonic, out operand) && operand.Match(Tag.AsmOperand, out name, out offset))
        {
            Symbol sym;
            if (!FindSymbol(scope, name, out sym)) Program.Error("symbol not defined: {0}", name);

            int address;
            if (sym.Tag == SymbolTag.Constant || sym.Tag == SymbolTag.Variable)
            {
                // TODO: Make sure the constant value fits in the specified type.
                address = sym.Value;
            }
            else
            {
                Program.UnhandledCase();
                address = 0;
            }

            return Expr.Make(Tag.Asm, mnemonic, address + offset);
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
        Expr left, indexExpr;
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
        else if (e.Match(Tag.Index, out left, out indexExpr))
        {
            left = ReplaceFields(left);
            indexExpr = ReplaceFields(indexExpr);
            return Expr.Make(
                Tag.AddGeneric,
                Expr.Make(Tag.AddressOf, left),
                indexExpr);
        }
        else
        {
            return e.Map(ReplaceFields);
        }
    }

    static Dictionary<string, string> UnaryOperators = new Dictionary<string, string>
    {
        { Tag.BitwiseNotGeneric, "bitwise_not" },
    };

    static Dictionary<string, string> SymmetricBinaryOperators = new Dictionary<string, string>
    {
        { Tag.SubtractGeneric, "sub" },
        { Tag.MultiplyGeneric, "mul" },
        { Tag.DivideGeneric, "div" },
        { Tag.ModulusGeneric, "mod" },
        { Tag.LessThanGeneric, "lt" },
        { Tag.GreaterThanGeneric, "gt" },
        { Tag.BitwiseAndGeneric, "bitwise_and" },
        { Tag.BitwiseOrGeneric, "bitwise_or" },
        { Tag.BitwiseXorGeneric, "bitwise_xor" },
    };

    Expr ReplaceGenericFunctions(Expr e, CType returnType)
    {
        // Recursively apply this pass to all arguments:
        e = e.Map(x => ReplaceGenericFunctions(x, returnType));

        string tag, functionBaseName;
        Expr left, right;
        if (e.Match(Tag.Return, out left))
        {
            if (!TryToChangeType(ref left, returnType)) Program.Error("return expression has wrong type");
            return Expr.Make(Tag.Return, left);
        }
        else if (e.Match(Tag.LoadGeneric, out left))
        {
            CType addressType = TypeOf(left);
            if (addressType.Tag != CTypeTag.Pointer) Program.Error("load address must have pointer type");
            CType resultType = addressType.Subtype;
            return Expr.Make(GetLoadFunctionForType(resultType), left);
        }
        else if (e.Match(Tag.StoreGeneric, out left, out right))
        {
            CType leftType = TypeOf(left);

            CType addressType = TypeOf(left);
            if (addressType.Tag != CTypeTag.Pointer) Program.Error("store address must have pointer type");
            CType expectedTypeOfValue = addressType.Subtype;
            if (!TryToChangeType(ref right, expectedTypeOfValue)) Program.Error("types in assignment must match");
            return Expr.Make(GetStoreFunctionForType(expectedTypeOfValue), left, right);
        }
        else if (e.Match(Tag.AddGeneric, out left, out right))
        {
            CType leftType = TypeOf(left);
            CType rightType = TypeOf(right);

            if (leftType.IsPointer)
            {
                if (!TryToChangeType(ref right, CType.UInt16)) Program.Panic("pointer offset must have integer type");
                int elementSize = SizeOf(leftType.Subtype);
                return Expr.Make(
                    Tag.Cast,
                    leftType,
                    Expr.Make(
                        Tag.AddU16,
                        left,
                        Expr.Make(
                            Tag.MultiplyU16,
                            right,
                            Expr.Make(
                                Tag.Int,
                                elementSize,
                                CType.UInt16))));
            }
            else if (rightType.IsPointer)
            {
                // TODO: Swap the operands and use the usual (pointer + offset) logic.
                Program.NYI();
                return null;
            }
            else
            {
                // The operands must have the same type, and it must be an integer type. (You can't add pointers together.)
                if (!TryToChangeType(ref right, leftType)) Program.Error("types in binary expression must match");

                string specificName = null;
                if (leftType == CType.UInt8) specificName = Tag.AddU8;
                else if (leftType == CType.UInt16) specificName = Tag.AddU16;
                else Program.NYI();

                return Expr.Make(specificName, left, right);
            }
        }
        else if (e.Match(out tag, out left) && UnaryOperators.TryGetValue(tag, out functionBaseName))
        {
            CType leftType = TypeOf(left);
            return Expr.Make(string.Format("_rt_{0}_{1}", functionBaseName, GetTypeCode(leftType)), left);
        }
        else if (e.Match(out tag, out left, out right) && SymmetricBinaryOperators.TryGetValue(tag, out functionBaseName))
        {
            CType leftType = TypeOf(left);
            if (!TryToChangeType(ref right, leftType)) Program.Error("types in binary expression must match");
            return Expr.Make(string.Format("_rt_{0}_{1}", functionBaseName, GetTypeCode(leftType)), left, right);
        }
        else if (e.Match(Tag.BoolFromGeneric, out left))
        {
            CType sourceType = TypeOf(left);

            string specificName = null;
            if (sourceType == CType.UInt16) specificName = Tag.BoolFromU16;
            else Program.NYI();

            return Expr.Make(specificName, left);
        }
        else
        {
            ReportInvalidNodes(e, Tag.Name, Tag.Scope, Tag.Local);
            return e;
        }
    }

    static string GetTypeCode(CType type)
    {
        if (type == CType.UInt8) return "u8";
        else if (type == CType.UInt16) return "u16";
        Program.NYI();
        return null;
    }

    /// <summary>
    /// It is possible to interpret some expressions, such as integer literals, as any of several types.
    /// If possible, change the type of the given expression to the expected type.
    /// </summary>
    bool TryToChangeType(ref Expr e, CType expectedType)
    {
        int value;
        CType type;
        if (TypeOf(e) == expectedType)
        {
            return true;
        }
        else if (e.Match(Tag.Int, out value, out type))
        {
            // Make sure the actual value is small enough to fit.
            if (expectedType == CType.UInt8 && value >= 0 && value <= byte.MaxValue)
            {
                e = Expr.Make(Tag.Int, value, CType.UInt8);
                return true;
            }
            else if (expectedType == CType.UInt16 && value >= 0 && value <= ushort.MaxValue)
            {
                e = Expr.Make(Tag.Int, value, CType.UInt16);
                return true;
            }
        }

        return false;
    }

    void CheckTypes(Expr e, CType returnType)
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
                CheckTypes(sub, returnType);
            }
        }

        if (e.Match(Tag.Int, out value, out type))
        {
            // This should have had a specific type assigned to it by now:
            if (type == CType.Implied) Program.Panic("integer literal has not been given a type");
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
            if (actual != returnType) Program.Error("return expression has wrong type");
        }
        else if (e.Match(Tag.Cast, out type, out arg))
        {
            // TODO: Make sure that the cast is allowed.
        }
        else if (e.MatchTag(Tag.Asm))
        {
            // Ignore inline assembly.
        }
        else if (e.MatchAny(out name, out args))
        {
            // Don't try to typecheck special AST nodes, which start with "$".
            if (!name.StartsWith("$"))
            {
                CFunctionInfo functionInfo = GetFunctionInfo(name);
                if (functionInfo.ParameterTypes.Length != args.Length) Program.Error("wrong number of arguments to function: {0}", name);
                // Check that each of the actual and expected parameter types match:
                for (int i = 0; i < args.Length; i++)
                {
                    CType argType = TypeOf(args[i]);
                    if (argType != functionInfo.ParameterTypes[i]) Program.Error("argument to function has wrong type");
                }
            }
        }
        else
        {
            Program.Panic("type checker: unhandled case");
        }
    }

    Expr SimplifyConstantExpressions(Expr e)
    {
        // Recursively apply this pass to all arguments:
        e = e.Map(SimplifyConstantExpressions);

        Expr left, right;
        CType type;
        int a, b;

        if (e.Match(Tag.AddU16, out left, out right) &&
            left.Match(Tag.Int, out a, out type) &&
            right.Match(Tag.Int, out b, out type))
        {
            return Expr.Make(Tag.Int, (a + b) % (1 << 16), CType.UInt16);
        }
        else if (e.Match(Tag.MultiplyU16, out left, out right) &&
            left.Match(Tag.Int, out a, out type) &&
            right.Match(Tag.Int, out b, out type))
        {
            return Expr.Make(Tag.Int, (a * b) % (1 << 16), CType.UInt16);
        }
        else
        {
            ReportInvalidNodes(e, Tag.Name, Tag.Scope, Tag.Local);
            return e;
        }
    }

    // Simplify the AST by combining casts that are applied to simple integer constants.
    Expr SimplifyCasts(Expr e)
    {
        // Recursively apply this pass to all arguments:
        e = e.Map(SimplifyCasts);

        Expr subexpr;
        CType imposedType, ignoredType;
        int value;

        if (e.Match(Tag.Cast, out imposedType, out subexpr) &&
            subexpr.Match(Tag.Int, out value, out ignoredType))
        {
            return Expr.Make(Tag.Int, value, imposedType);
        }
        else
        {
            return e;
        }
    }

    CType TypeOf(Expr e)
    {
        // In most contexts, a pointer to an array "decays" into a pointer to the first element of the array.
        CType type = TypeOfWithoutDecay(e);
        CType decayedType;
        if (type.IsPointer && type.Subtype.IsArray)
        {
            decayedType = CType.MakePointer(type.Subtype.Subtype);
        }
        else
        {
            decayedType = type;
        }
        return decayedType;
    }

    CType TypeOfWithoutDecay(Expr e)
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
            return GetFunctionInfo(name).ReturnType;
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

    void CompileExpression(Expr e, int dest, Continuation cont)
    {
        int value;
        string functionName;
        CType type;
        Expr[] args;
        Expr test, then, init, induction, body, subexpr;
        if (e.Match(Tag.Int, out value, out type))
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
                Emit("JMP", end);
                EmitComment("end of switch clause");
                Emit(Asm.Label, nextClause);
            }

            EmitComment("end of switch");
            Emit(Asm.Label, end);
        }
        else if (e.Match(Tag.For, out init, out test, out induction, out body))
        {
            string bottom = MakeUniqueLabel();

            EmitComment("for loop (prologue)");
            CompileExpression(init, DestinationDiscard, Continuation.Fallthrough);
            EmitComment("for loop");
            string top = MakeUniqueLabel();
            Emit(Asm.Label, top);
            CompileExpression(test, DestinationDiscard, new Continuation(JumpCondition.IfFalse, bottom));
            CompileExpression(body, DestinationDiscard, Continuation.Fallthrough);
            CompileExpression(induction, DestinationDiscard, Continuation.Fallthrough);
            Emit("JMP", top);
            EmitComment("for loop (end)");
            Emit(Asm.Label, bottom);

            if (cont.When != JumpCondition.Never) Program.NYI();
        }
        else if (e.Match(Tag.Return, out subexpr))
        {
            EmitComment("return");
            CompileExpression(subexpr, DestinationAcc, Continuation.Fallthrough);
            Emit("RTS");
        }
        else if (e.Match(Tag.Cast, out type, out subexpr))
        {
            CompileExpression(subexpr, dest, cont);
        }
        else if (e.MatchTag(Tag.Asm))
        {
            // Copy assembly instructions almost verbatim:
            Emit(e.GetArgs().Skip(1).ToArray());
        }
        else if (e.MatchAny(out functionName, out args))
        {
            int addr;
            if ((functionName == Tag.LoadU8 || functionName == Tag.LoadU16) && args[0].Match(Tag.Int, out addr, out type))
            {
                // Loads from constant addresses must be optimized, because this pattern is used to copy data from
                // temporary variables into call frames; we can't generate a call to "load_***" in the middle of
                // generating some other call.

                EmitLoad(addr, type, dest, cont);
            }
            else if ((functionName == Tag.StoreU8 || functionName == Tag.StoreU16) && args[0].Match(Tag.Int, out addr, out type))
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
                    if (arg.Match(Tag.Int, out n, out ignored))
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
                EmitComment("function call: " + functionName);
                if (functionName == Tag.LoadU16)
                {
                    if (args.Length != 1) Program.Panic("wrong number of arguments to unary operator");
                    // TODO: This would be more efficient if it loaded the high byte first.
                    Emit("LDY", 0, Asm.Immediate);
                    Emit("LDA", T0, Asm.IndirectY);
                    Emit("STA", T2);
                    Emit("INY");
                    Emit("LDA", T0, Asm.IndirectY);
                    Emit("TAX");
                    Emit("LDA", T2);
                }
                else if (functionName == Tag.LoadU8)
                {
                    if (args.Length != 1) Program.Panic("wrong number of arguments to unary operator");
                    Emit("LDY", 0, Asm.Immediate);
                    Emit("LDA", T0, Asm.IndirectY);
                }
                else if (functionName == Tag.LessThanU8)
                {
                    if (args.Length != 2) Program.Panic("wrong number of arguments to binary operator");
                    Emit("SEC");
                    Emit("LDA", T0);
                    Emit("CMP", T2);
                    // The carry flag will be *clear* if T0 < T2.
                    // Load the corresponding boolean value:
                    Emit("LDA", 0, Asm.Immediate);
                    string skip = MakeUniqueLabel();
                    Emit("BCS", skip);
                    Emit("LDA", 1, Asm.Immediate);
                    Emit(Asm.Label, skip);
                }
                else if (functionName == Tag.GreaterThanU8)
                {
                    if (args.Length != 2) Program.Panic("wrong number of arguments to binary operator");
                    Emit("SEC");
                    Emit("LDA", T2);
                    Emit("CMP", T0);
                    // The carry flag will be *clear* if T0 > T2.
                    // Load the corresponding boolean value:
                    Emit("LDA", 0, Asm.Immediate);
                    string skip = MakeUniqueLabel();
                    Emit("BCS", skip);
                    Emit("LDA", 1, Asm.Immediate);
                    Emit(Asm.Label, skip);
                }
                else if (functionName == Tag.StoreU16)
                {
                    if (args.Length != 2) Program.Panic("wrong number of arguments to binary operator");
                    Emit("LDY", 0, Asm.Immediate);
                    Emit("LDA", T2);
                    Emit("STA", T0, Asm.IndirectY);
                    Emit("INY");
                    Emit("LDA", T3);
                    Emit("STA", T0, Asm.IndirectY);
                }
                else if (functionName == Tag.StoreU8)
                {
                    if (args.Length != 2) Program.Panic("wrong number of arguments to binary operator");
                    Emit("LDY", 0, Asm.Immediate);
                    Emit("LDA", T2);
                    Emit("STA", T0, Asm.IndirectY);
                }
                else if (functionName == Tag.BoolFromU16)
                {
                    if (args.Length != 1) Program.Panic("wrong number of arguments to unary operator");
                    Emit("LDA", T0);
                    Emit("ORA", T1);
                }
                else
                {
                    // JSR to the function:
                    Emit("JSR", functionName);
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

    void EmitLoadImmediate(int imm, CType type, int dest, Continuation cont)
    {
        int width = SizeOf(type);
        if (dest == DestinationDiscard)
        {
            // NOP
        }
        else if (dest == DestinationAcc)
        {
            Emit("LDA", (int)LowByte(imm), Asm.Immediate);
            if (width == 2) Emit("LDX", (int)HighByte(imm), Asm.Immediate);
        }
        else
        {
            Emit("LDA", (int)LowByte(imm), Asm.Immediate);
            Emit("STA", dest);
            if (width == 2)
            {
                Emit("LDX", (int)HighByte(imm), Asm.Immediate);
                Emit("STX", dest + 1);
            }
        }

        if ((cont.When == JumpCondition.IfTrue && imm != 0) ||
            (cont.When == JumpCondition.IfFalse && imm == 0))
        {
            Emit("JMP", cont.Target);
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
            Emit("STA", dest);
            if (width == 2) Emit("STX", dest + 1);
        }
    }

    void EmitBranchOnAcc(Continuation cont)
    {
        if (cont.When != JumpCondition.Never)
        {
            // TODO: Values used as branch conditions must have a single-byte type, for easy comparison against zero.
            EmitComment("branch on ACC");
            Emit("ORA", 0, Asm.Immediate);
            string op = (cont.When == JumpCondition.IfTrue) ? "BNE" : "BEQ";
            Emit(op, cont.Target);
        }
    }

    void EmitLoad(int address, CType type, int dest, Continuation cont)
    {
        int width = SizeOf(type);
        // Even if the value is unused, always read the address; it might be a hardware register.
        Emit("LDA", address);
        if (width == 2) Emit("LDX", address + 1);
        EmitStoreAcc(type, dest);
        EmitBranchOnAcc(cont);
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

    static byte LowByte(int n)
    {
        return (byte)(n & 0xFF);
    }

    static byte HighByte(int n)
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
        else if (type.Tag == CTypeTag.Array)
        {
            return type.Dimension * SizeOf(type.Subtype);
        }

        Program.NYI();
        return 1;
    }

    string GetLoadFunctionForType(CType type)
    {
        int size = SizeOf(type);
        if (size == 1) return Tag.LoadU8;
        if (size == 2) return Tag.LoadU16;
        Program.NYI();
        return null;
    }

    string GetStoreFunctionForType(CType type)
    {
        int size = SizeOf(type);
        if (size == 1) return Tag.StoreU8;
        if (size == 2) return Tag.StoreU16;
        Program.NYI();
        return null;
    }

    void DeclareFunction(string name, CType[] paramTypes, CType returnType, int[] paramAddresses)
    {
        // TODO: It should be allowed to declare a function multiple times as long as the types match.
        if (Functions.ContainsKey(name))
        {
            Program.Error("function already declared: " + name);
        }

        Functions[name] = new CFunctionInfo
        {
            ParameterTypes = paramTypes,
            ReturnType = returnType,
            ParameterAddresses = paramAddresses,
        };
    }

    CFunctionInfo GetFunctionInfo(string name)
    {
        CFunctionInfo info;
        if (Functions.TryGetValue(name, out info)) return info;
        Program.Error("function not declared: {0}", name);
        return null;
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
// - Array elementType dimension
partial class CType
{
    public CTypeTag Tag;
    public CSimpleType SimpleType;
    public string Name;
    public CType Subtype;
    public int Dimension;
}

enum CTypeTag
{
    Simple,
    Pointer,
    Struct,
    Array,
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
    public int[] ParameterAddresses;
}

[DebuggerDisplay("{Show(),nq}")]
partial class CType : IEquatable<CType>
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

    public static CType MakeArray(CType elementType, int dimension)
    {
        return new CType
        {
            Tag = CTypeTag.Array,
            Subtype = elementType,
            Dimension = dimension,
        };
    }

    public bool IsPointer => Tag == CTypeTag.Pointer;
    public bool IsStruct => Tag == CTypeTag.Struct;
    public bool IsArray => Tag == CTypeTag.Array;

    public override bool Equals(object obj)
    {
        // Don't bother supporting equality-testing with arbitrary other types.
        throw new NotSupportedException();
    }

    public override int GetHashCode()
    {
        throw new NotSupportedException();
    }

    public bool Equals(CType other)
    {
        if (Tag != other.Tag) return false;
        else if (Tag == CTypeTag.Simple) return SimpleType.Equals(other.SimpleType);
        else if (Tag == CTypeTag.Pointer) return Subtype.Equals(other.Subtype);
        else if (Tag == CTypeTag.Struct) return Name == other.Name;
        else
        {
            Program.NYI();
            return false;
        }
    }

    public static bool operator ==(CType a, CType b) => a.Equals(b);
    public static bool operator !=(CType a, CType b) => !a.Equals(b);

    public string Show()
    {
        if (Tag == CTypeTag.Simple) return SimpleType.ToString();
        else if (Tag == CTypeTag.Pointer) return "pointer to " + Subtype.Show();
        else if (Tag == CTypeTag.Struct) return "struct " + Name;
        else if (Tag == CTypeTag.Array) return string.Format("array[{1}] of {0}", Subtype.Show(), Dimension);
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
