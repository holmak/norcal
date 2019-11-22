﻿using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

partial class Compiler
{
    int ZeroPageNext = ZeroPageStart;
    int RamNext = RamStart;
    int NextLabelNumber = 0;
    Dictionary<string, CFunctionInfo> Functions = new Dictionary<string, CFunctionInfo>();
    Dictionary<string, CStructInfo> StructTypes = new Dictionary<string, CStructInfo>();

    static readonly int ZeroPageStart = 0x000;
    static readonly int ZeroPageEnd = 0x100;
    static readonly int RamStart = 0x300;
    static readonly int RamEnd = 0x800;

    static readonly int DestinationDiscard = -1;
    static readonly int DestinationAcc = -2;

    public void CompileProgram(List<Expr> program)
    {
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
        foreach (FunctionDeclaration function in EnumerateFunctions(program))
        {
            CFunctionInfo functionInfo = GetFunctionInfo(function.Name);
            CheckTypes(function.Body, functionInfo.ReturnType);
        }

        // Post-typechecking passes:
        program = ApplyPass("replace-loops", x => ReplaceLoops(x, null), program);
        program = ApplyPass("fold-constants", SimplifyConstantExpressions, program);
        program = ApplyPass("simplify-casts", SimplifyCasts, program);
        program = RemoveUncalledFunctionsAndDetectRecursion(program, new[] { "nmi", "reset", "brk" });

        // Pass: Codegen
        foreach (FunctionDeclaration function in EnumerateFunctions(program))
        {
            Emit(Asm.Function, function.Name);
            CompileExpression(function.Body, DestinationDiscard, Continuation.Fallthrough);
            Emit("RTS");
        }

        // TODO: Make sure interrupt-handling functions have the right type signature and are defined.

        // Put the interrupt vector table at the end of ROM:
        Emit(Asm.SkipTo, 0xFFFA);
        Emit(Asm.Word, "nmi");
        Emit(Asm.Word, "reset");
        Emit(Asm.Word, "brk");

        // Show the final assembly code:
        if (Program.EnableDebugOutput)
        {
            StringBuilder sb = new StringBuilder();
            foreach (Expr e in Assembly)
            {
                string line;

                string mnemonic, text, mode;
                int operand;
                if (e.Match(Asm.Comment, out text)) line = "\t; " + text;
                else if (e.Match(Asm.Label, out text)) line = text + ":";
                else if (e.Match(Asm.Function, out text)) line = string.Format("\nfunction {0}:", text);
                else if (e.Match(out mnemonic)) line = string.Format("\t{0}", mnemonic);
                else if (e.Match(out mnemonic, out text)) line = string.Format("\t{0} {1}", mnemonic, text);
                else if (e.Match(out mnemonic, out operand, out mode))
                {
                    string format;
                    if (mode == Asm.Absolute) format = "\t{0} ${1:X}";
                    else if (mode == Asm.Immediate) format = "\t{0} #${1:X}";
                    else if (mode == Asm.IndirectY) format = "\t{0} (${1:X}),Y";
                    else format = "\t{0} ${1:X} ???";
                    line = string.Format(format, mnemonic, operand);
                }
                else line = '\t' + e.Show();

                sb.AppendLine(line);
            }
            Program.WritePassOutputToFile("assembly", sb.ToString());
        }
    }

    /// <summary>
    /// Enumerate the function declarations from a list.
    /// </summary>
    IEnumerable<FunctionDeclaration> EnumerateFunctions(IEnumerable<Expr> declarations)
    {
        foreach (Expr decl in declarations)
        {
            FunctionDeclaration function;
            if (MatchFunctionDeclaration(decl, out function))
            {
                yield return function;
            }
        }
    }

    bool MatchFunctionDeclaration(Expr declaration, out FunctionDeclaration function)
    {
        function = new FunctionDeclaration();
        return declaration.Match(Tag.Function, out function.ReturnType, out function.Name, out function.Parameters, out function.Body);
    }

    List<Expr> ApplyFirstPass(List<Expr> program)
    {
        LexicalScope globalScope = new LexicalScope();

        List<Expr> newProgram = new List<Expr>();
        foreach (Expr decl in program)
        {
            MemoryRegion region;
            CType returnType, type;
            string name;
            FieldInfo[] fields;
            Expr body;
            if (decl.Match(Tag.Function, out returnType, out name, out fields, out body))
            {
                // Allocate space for each parameter and store it in the symbol table.
                CType[] paramTypes = new CType[fields.Length];
                int[] addresses = new int[fields.Length];
                for (int i = 0; i < fields.Length; i++)
                {
                    CType fieldType = fields[i].Type;
                    paramTypes[i] = fieldType;
                    addresses[i] = AllocGlobal(fields[i].Region, SizeOf(fieldType));
                }
                DeclareFunction(name, paramTypes, returnType, addresses);
                EmitComment("declare function " + name);

                // Define each of the function's parameters as a local variable:
                LexicalScope functionScope = PushScope(globalScope);
                for (int i = 0; i < fields.Length; i++)
                {
                    FieldInfo f = fields[i];
                    DefineSymbol(functionScope, SymbolTag.Variable, f.Name, addresses[i], f.Type);
                }

                newProgram.Add(Expr.Make(
                    Tag.Function,
                    returnType,
                    name,
                    fields,
                    ReplaceNamedVariables(body, functionScope)));
            }
            else if (decl.Match(Tag.Constant, out type, out name, out body))
            {
                // TODO: Make sure the declared type matches the actual type.
                CType valueType;
                int value;
                if (!body.Match(Tag.Int, out value, out valueType))
                {
                    Program.Error("expression must be constant");
                }
                DefineSymbol(globalScope, SymbolTag.Constant, name, value, type);
            }
            else if (decl.Match(Tag.Variable, out region, out type, out name))
            {
                DefineVariable(globalScope, region, type, name);
            }
            else if (decl.Match(Tag.Struct, out name, out fields))
            {
                CField[] structFields = new CField[fields.Length];
                int offset = 0;
                for (int i = 0; i < fields.Length; i++)
                {
                    structFields[i] = new CField
                    {
                        Type = fields[i].Type,
                        Name = fields[i].Name,
                        Offset = offset,
                    };
                    offset += SizeOf(fields[i].Type);
                }
                StructTypes.Add(name, new CStructInfo
                {
                    TotalSize = offset,
                    Fields = structFields,
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
    List<Expr> ApplyPass(string passName, Func<Expr, Expr> apply, List<Expr> program)
    {
        return ApplyPass(passName, (expr, returnType) => apply(expr), program);
    }

    List<Expr> ApplyPass(string passName, Func<Expr, CType, Expr> apply, List<Expr> program)
    {
        List<Expr> newProgram = new List<Expr>();
        foreach (Expr decl in program)
        {
            CType returnType;
            string name;
            FieldInfo[] parameters;
            Expr body;
            if (decl.Match(Tag.Function, out returnType, out name, out parameters, out body))
            {
                newProgram.Add(Expr.Make(
                    Tag.Function,
                    returnType,
                    name,
                    parameters,
                    apply(body, returnType)));
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
        string name, mnemonic, mode;
        Expr body, operand;
        MemoryRegion region;
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
        else if (e.Match(Tag.Local, out region, out type, out name))
        {
            DefineVariable(scope, region, type, name);
            // There is no need to keep the declaration node:
            return Expr.Make(Tag.Empty);
        }
        else if (e.Match(Tag.Asm, out mnemonic, out operand, out mode) && operand.Match(Tag.AsmOperand, out name, out offset))
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

            return Expr.Make(Tag.Asm, mnemonic, address + offset, mode);
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
        { Tag.BoolFromGeneric, "bool" },
        { Tag.BitwiseNotGeneric, "bitwise_not" },
        { Tag.PredecrementGeneric, "predec" },
        { Tag.PostdecrementGeneric, "postdec" },
        { Tag.PreincrementGeneric, "preinc" },
        { Tag.PostincrementGeneric, "postinc" },
    };

    static Dictionary<string, string> SymmetricBinaryOperators = new Dictionary<string, string>
    {
        { Tag.SubtractGeneric, "sub" },
        { Tag.MultiplyGeneric, "mul" },
        { Tag.DivideGeneric, "div" },
        { Tag.ModulusGeneric, "mod" },
        { Tag.EqualGeneric, "eq" },
        { Tag.NotEqualGeneric, "ne" },
        { Tag.LessThanGeneric, "lt" },
        { Tag.LessThanOrEqualGeneric, "le" },
        { Tag.GreaterThanGeneric, "gt" },
        { Tag.GreaterThanOrEqualGeneric, "ge" },
        { Tag.BitwiseAndGeneric, "bitwise_and" },
        { Tag.BitwiseOrGeneric, "bitwise_or" },
        { Tag.BitwiseXorGeneric, "bitwise_xor" },
        { Tag.ShiftLeftGeneric, "shift_left" },
        { Tag.ShiftRightGeneric, "shift_right" },
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
        else
        {
            return e;
        }
    }

    static string GetTypeCode(CType type)
    {
        if (type == CType.UInt8 || type == CType.UInt8Ptr) return "u8";
        else if (type == CType.UInt16 || type == CType.UInt16Ptr) return "u16";
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
        else if (e.MatchTag(Tag.Label))
        {
            // NOP
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

    List<Expr> RemoveUncalledFunctionsAndDetectRecursion(List<Expr> program, string[] functionsToNeverRemove)
    {
        List<Expr> newProgram = new List<Expr>(program);

        // Figure out which functions each function could call:
        Dictionary<string, HashSet<string>> callGraph = new Dictionary<string, HashSet<string>>();
        foreach (FunctionDeclaration function in EnumerateFunctions(program))
        {
            HashSet<string> calledFunctions = new HashSet<string>();
            callGraph[function.Name] = calledFunctions;

            Action<Expr> findCalls = null;
            findCalls = (expr) =>
            {
                string tag = expr.GetArgs().First() as string;
                if (tag != null && !tag.StartsWith("$"))
                {
                    calledFunctions.Add(tag);
                }

                foreach (object arg in expr.GetArgs())
                {
                    Expr child = arg as Expr;
                    if (child != null)
                    {
                        findCalls(child);
                    }
                }
            };

            findCalls(function.Body);
        }

        // Repeatedly remove uncalled functions from the program (but not interrupt vectors):
        {
            KeepSearching:
            foreach (string func in callGraph.Keys)
            {
                if (functionsToNeverRemove.Contains(func))
                {
                    continue;
                }

                if (!callGraph.Values.Any(x => x.Contains(func)))
                {
                    callGraph.Remove(func);
                    newProgram.RemoveAll(decl =>
                    {
                        FunctionDeclaration function;
                        return MatchFunctionDeclaration(decl, out function) && function.Name == func;
                    });
                    goto KeepSearching;
                }
            }
        }

        // Find all of the remaining functions that have no incoming calls:
        Queue<string> rootFunctions = new Queue<string>();
        foreach (string func in functionsToNeverRemove)
        {
            if (callGraph.ContainsKey(func))
            {
                rootFunctions.Enqueue(func);
            }
        }

        // Perform a topological sort, discarding the actual sorted list:
        while (rootFunctions.Count > 0)
        {
            string func = rootFunctions.Dequeue();
            HashSet<string> callees = callGraph[func];
            callGraph.Remove(func);

            foreach (string otherFunc in callees)
            {
                if (!callGraph.Values.Any(x => x.Contains(otherFunc)))
                {
                    rootFunctions.Enqueue(otherFunc);
                }
            }
        }

        // If any functions remain in the call graph it means a cycle exists:
        if (callGraph.Count > 0)
        {
            // Repeatedly remove leaf node functions in an attempt to clean up the error message:
            KeepSearching:
            foreach (var funcAndCallees in callGraph)
            {
                if (funcAndCallees.Value.Count == 0)
                {
                    foreach (var callees in callGraph.Values)
                    {
                        callees.Remove(funcAndCallees.Key);
                    }

                    callGraph.Remove(funcAndCallees.Key);
                    goto KeepSearching;
                }
            }

            Program.Error("functions called recursively: {0}", string.Join(", ", callGraph.Keys));
        }

        Program.WritePassOutputToFile("remove-uncalled-functions", newProgram);
        return newProgram;
    }

    Expr ReplaceLoops(Expr e, LoopScope loop)
    {
        Expr init, test, induction, body;
        if (e.Match(Tag.Continue))
        {
            return Expr.Make(Tag.Goto, loop.ContinueLabel);
        }
        else if (e.Match(Tag.Break))
        {
            return Expr.Make(Tag.Goto, loop.BreakLabel);
        }
        else if (e.Match(Tag.For, out init, out test, out induction, out body))
        {
            loop = new LoopScope
            {
                Outer = loop,
                ContinueLabel = MakeUniqueLabel("for_top_"),
                BreakLabel = MakeUniqueLabel("for_bottom_"),
            };

            return Expr.Make(Tag.Sequence,
                init,
                Expr.Make(Tag.Label, loop.ContinueLabel),
                Expr.Make(Tag.GotoIfNot, test, loop.BreakLabel),
                ReplaceLoops(body, loop),
                induction,
                Expr.Make(Tag.Goto, loop.ContinueLabel),
                Expr.Make(Tag.Label, loop.BreakLabel));
        }
        else
        {
            // Recursively apply this pass to all arguments:
            return e.Map(x => ReplaceLoops(x, loop));
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
        string functionName, target;
        CType type, pointerType;
        Expr[] args;
        Expr test, then, subexpr;
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
        else if (e.MatchAny(Tag.Switch, out args))
        {
            string end = MakeUniqueLabel();

            for (int i = 0; i < args.Length; i += 2)
            {
                string nextClause = MakeUniqueLabel();
                // If this clause's condition is false, try the next clause:
                CompileExpression(args[i], DestinationDiscard, new Continuation(JumpCondition.IfFalse, nextClause));
                // If the condition was true, execute the clause body:
                CompileExpression(args[i + 1], dest, cont);
                // After executing the body of a clause, skip the rest of the clauses:
                Emit("JMP", end);
                EmitComment("end of switch clause");
                Emit(Asm.Label, nextClause);
            }

            EmitComment("end of switch");
            Emit(Asm.Label, end);
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
        else if (e.Match(Tag.Label, out target))
        {
            Emit(Asm.Label, target);
        }
        else if (e.Match(Tag.Goto, out target))
        {
            Emit("JMP", target);
        }
        else if (e.Match(Tag.GotoIf, out subexpr, out target))
        {
            CompileExpression(subexpr, DestinationDiscard, new Continuation(JumpCondition.IfTrue, target));
        }
        else if (e.Match(Tag.GotoIfNot, out subexpr, out target))
        {
            CompileExpression(subexpr, DestinationDiscard, new Continuation(JumpCondition.IfFalse, target));
        }
        else if (e.MatchTag(Tag.Asm))
        {
            // Copy assembly instructions almost verbatim:
            Emit(e.GetArgs().Skip(1).ToArray());
        }
        else if (e.MatchAny(out functionName, out args))
        {
            int addr;
            if ((functionName == Tag.LoadU8 || functionName == Tag.LoadU16) && args[0].Match(Tag.Int, out addr, out pointerType))
            {
                // Loads from constant addresses must be optimized, because this pattern is used to copy data from
                // temporary variables into call frames; we can't generate a call to "load_***" in the middle of
                // generating some other call.

                CType valueType = GetTypePointedAt(pointerType);
                EmitLoad(addr, valueType, dest, cont);
            }
            else if ((functionName == Tag.StoreU8 || functionName == Tag.StoreU16) && args[0].Match(Tag.Int, out addr, out pointerType))
            {
                // This case is not essential, but generates better code.

                CType valueType = GetTypePointedAt(pointerType);
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
                    EmitStoreAcc(valueType, addr);
                    EmitStoreAcc(valueType, dest);
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
                        int temp = AllocTemp(MemoryRegion.Ram, argSize);
                        CompileExpression(arg, temp, Continuation.Fallthrough);
                        simpleArg = Expr.Make(GetLoadFunctionForType(argType), Expr.Make(Tag.Int, temp, CType.MakePointer(argType)));
                    }
                    temps.Add(simpleArg);
                }

                // Copy all of the argument values from the temporaries into the function's call frame.
                for (int i = 0; i < temps.Count; i++)
                {
                    EmitComment("prepare call: load argument " + i);
                    CompileExpression(temps[i], paramAddresses[i], Continuation.Fallthrough);
                }

                EmitComment("call " + functionName);
                Emit("JSR", functionName);

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

    CType GetTypePointedAt(CType pointerType)
    {
        if (!pointerType.IsPointer) Program.Panic("a pointer type is required");
        return pointerType.Subtype;
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
            Emit("STA", dest, Asm.Absolute);
            if (width == 2)
            {
                Emit("LDX", (int)HighByte(imm), Asm.Immediate);
                Emit("STX", dest + 1, Asm.Absolute);
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
            Emit("STA", dest, Asm.Absolute);
            if (width == 2) Emit("STX", dest + 1, Asm.Absolute);
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
        Emit("LDA", address, Asm.Absolute);
        if (width == 2) Emit("LDX", address + 1, Asm.Absolute);
        EmitStoreAcc(type, dest);
        EmitBranchOnAcc(cont);
    }

    void DefineVariable(LexicalScope scope, MemoryRegion region, CType type, string name)
    {
        int address = AllocGlobal(region, SizeOf(type));
        DefineSymbol(scope, SymbolTag.Variable, name, address, type);
    }

    string MakeUniqueLabel() => MakeUniqueLabel("L");

    string MakeUniqueLabel(string prefix)
    {
        return string.Format("@{0}{1}", prefix, NextLabelNumber++);
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
    int AllocGlobal(MemoryRegion region, int size)
    {
        int address;
        if (region == MemoryRegion.ZeroPage)
        {
            if (ZeroPageNext + size > ZeroPageEnd) Program.Error("Not enough zero page RAM to allocate global.");
            address = ZeroPageNext;
            ZeroPageNext += size;
        }
        else if (region == MemoryRegion.Ram)
        {
            if (RamNext + size > RamEnd) Program.Error("Not enough RAM to allocate global.");
            address = RamNext;
            RamNext += size;
        }
        else
        {
            Program.NYI();
            address = -1;
        }

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
    int AllocTemp(MemoryRegion region, int size)
    {
        return AllocGlobal(region, size);
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

class LoopScope
{
    public LoopScope Outer;
    public string ContinueLabel;
    public string BreakLabel;
}

enum MemoryRegion
{
    ZeroPage,
    Ram,
}

class FunctionDeclaration
{
    public CType ReturnType;
    public string Name;
    public FieldInfo[] Parameters;
    public Expr Body;
}
