﻿using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

class CodeGenerator
{
    FilePosition SourcePosition = FilePosition.Unknown;
    List<Expr> Output = new List<Expr>();
    Symbol TempPointer;
    Dictionary<string, CFunctionInfo> Functions = new Dictionary<string, CFunctionInfo>();
    Dictionary<string, AggregateInfo> AggregateTypes = new Dictionary<string, AggregateInfo>();

    // The current function:
    string CurrentFunctionName = null;
    CType CurrentFunctionReturnType = null;
    int FrameSize = 0;

    // Local scope info:
    LexicalScope CurrentScope;
    LoopScope Loop;

    // The return value always goes at the top of the current call frame, and is word-sized.
    static readonly Symbol ReturnValue = new Symbol(SymbolTag.Local, 2, CType.UInt16, "$return_value");

    public static List<Expr> CompileAll(Expr program)
    {
        CodeGenerator converter = new CodeGenerator();
        converter.CompileProgram(program);
        return converter.Output;
    }

    void CompileProgram(Expr program)
    {
        CurrentScope = new LexicalScope(null);

        TempPointer = DeclareGlobal(MemoryRegion.ZeroPage, CType.UInt8Ptr, "$temp_ptr");

        Expr[] declarations;
        if (!program.MatchAny(Tag.Sequence, out declarations))
        {
            Program.Panic("The top level of the syntax tree must be a sequence.");
        }

        string name, functionName;
        int number;
        CType type, returnType;
        MemoryRegion region;
        int[] values;
        Expr body;

        // Pass: Process all struct and union declarations.
        foreach (Expr decl in declarations)
        {
            FieldInfo[] parsedFields;
            if (decl.Match(Tag.Struct, out name, out parsedFields) || decl.Match(Tag.Union, out name, out parsedFields))
            {
                bool union = decl.MatchTag(Tag.Union);

                // Calculate the offset of each field:
                FieldInfo[] fields = new FieldInfo[parsedFields.Length];
                int offset = 0;
                int maxSize = 1;
                for (int i = 0; i < parsedFields.Length; i++)
                {
                    fields[i] = new FieldInfo(parsedFields[i].Type, parsedFields[i].Name, offset);
                    int fieldSize = SizeOf(parsedFields[i].Type);
                    if (!union) offset += fieldSize;
                    maxSize = Math.Max(maxSize, fieldSize);
                }

                AggregateInfo info;
                if (union)
                {
                    info = new AggregateInfo(AggregateLayout.Union, maxSize, fields);
                }
                else
                {
                    info = new AggregateInfo(AggregateLayout.Struct, offset, fields);
                }

                AggregateTypes.Add(name, info);
            }
        }

        // Pass: Process all other global declarations.
        foreach (Expr decl in declarations)
        {
            FieldInfo[] parsedFields;
            if (decl.Match(Tag.Function, out returnType, out functionName, out parsedFields))
            {
                if (Functions.ContainsKey(functionName)) Error("function is already defined: " + functionName);

                // Calculate the offset of each parameter:
                FieldInfo[] fields = new FieldInfo[parsedFields.Length];
                int offset = 0;
                for (int i = 0; i < parsedFields.Length; i++)
                {
                    fields[i] = new FieldInfo(parsedFields[i].Type, parsedFields[i].Name, offset);
                    int fieldSize = SizeOf(parsedFields[i].Type);
                    offset += fieldSize;
                }

                CFunctionInfo function = new CFunctionInfo
                {
                    Parameters = fields,
                    ReturnType = returnType,
                };
                Functions.Add(functionName, function);
            }
            else if (decl.Match(Tag.Constant, out type, out name, out number))
            {
                // TODO: Make sure the value fits in the specified type.
                DeclareSymbol(new Symbol(SymbolTag.Constant, number, type, name));
                Emit(Tag.Constant, name, number);
            }
            else if (decl.Match(Tag.Variable, out region, out type, out name))
            {
                DeclareGlobal(region, type, name);
            }
            else if (decl.Match(Tag.ReadonlyData, out type, out name, out values))
            {
                if (type.IsArray)
                {
                    // If the array size is unspecified, automatically make it match the number of provided values.
                    if (type.Dimension == 0)
                    {
                        type = CType.MakeArray(type.Subtype, values.Length);
                    }

                    if (values.Length > type.Dimension)
                    {
                        Program.Error(
                            decl.Source,
                            "declared size of array ({0}) is too small for the number of specified values ({1})",
                            type.Dimension, values.Length);
                    }
                }
                else if (values.Length != 1)
                {
                    Program.Panic(decl.Source, "non-array initializers must contain exactly one value");
                }

                // Convert the data to raw bytes:
                byte[] bytes = new byte[SizeOf(type)];
                if (type.IsArray && type.Subtype == CType.UInt8)
                {
                    for (int i = 0; i < type.Dimension; i++)
                    {
                        // Unspecified elements are initialized to zero.
                        bytes[i] = (i < values.Length) ? (byte)values[i] : (byte)0;
                    }
                }
                else
                {
                    Program.NYI();
                }

                DeclareSymbol(new Symbol(SymbolTag.Constant, 0, type, name));
                Emit(Tag.ReadonlyData, name, bytes);
            }
        }

        // Pass: Generate code for function bodies.
        foreach (Expr decl in declarations)
        {
            FieldInfo[] fields;
            if (decl.Match(Tag.Function, out returnType, out name, out fields, out body))
            {
                Emit(Tag.Function, name);
                BeginScope();

                CurrentFunctionName = name;
                CurrentFunctionReturnType = returnType;
                FrameSize = 0;

                foreach (FieldInfo field in fields)
                {
                    DeclareLocal(field.Type, field.Name, isParameter: true);
                }

                // Two bytes must be reserved at the top of the frame for the return value.
                // If the parameters didn't reserve enough space, add more now.
                while (FrameSize < 2)
                {
                    EmitAsm("DEX");
                    AdjustFrameSize(+1);
                }

                Compile(body);
                ReturnFromFunction();
                EndScope();
            }
        }

        // Put the interrupt vector table at the end of ROM:
        Emit(Tag.SkipTo, 0xFFFA);
        Emit(Tag.Word, "nmi");
        Emit(Tag.Word, "reset");
        Emit(Tag.Word, "brk");
    }

    void Compile(Expr expr)
    {
        Expr subexpr, left, right;
        Expr[] block;
        CType type;
        string name, op;
        if (expr.MatchAny(Tag.Sequence, out block))
        {
            foreach (Expr stmt in block)
            {
                EmitComment("STATEMENT: " + stmt.Show());
                Compile(stmt);
            }
        }
        else if (expr.Match(Tag.Variable, out type, out name))
        {
            DeclareLocal(type, name, isParameter: false);
        }
        else if (expr.Match(Tag.Assign, out left, out right))
        {
            CType leftType = TypeOf(left);
            NaivePushAddressOf(left);
            NaivePush(right);
            NaiveStore(SizeOf(leftType));
        }
        else if (expr.Match(Tag.AssignModify, out op, out left, out right))
        {
            EmitComment("assign modified");
            CType leftType = TypeOf(left);
            NaivePushAddressOf(left);
            NaiveLoadNondestructive();
            NaivePush(right);
            NaiveCallBinaryOperator(op);
            NaiveStore(SizeOf(leftType));
        }
        else if (expr.Match(Tag.Return, out subexpr))
        {
            Symbol result = NaivePush(subexpr);
            EmitAsm("LDA", LowByte(result));
            EmitAsm("STA", LowByte(ReturnValue));
            EmitAsm("LDA", HighByte(result));
            EmitAsm("STA", HighByte(ReturnValue));
            NaivePopTemporary();
            ReturnFromFunction();
        }
        else if (expr.MatchTag(Tag.Asm) || expr.MatchTag(Tag.Label))
        {
            // Pass assembly through unchanged:
            Emit(expr);
        }
        else
        {
            Program.UnhandledCase();
        }
    }

    void ReturnFromFunction()
    {
        EmitComment("epilogue");
        for (int i = 0; i < FrameSize; i++)
        {
            EmitAsm("INX");
        }
        EmitAsm("RTS");
    }

    void NaivePushAddressOf(Expr expr)
    {
        string name;
        Expr subexpr;
        if (expr.Match(Tag.Name, out name))
        {
            Symbol sym = FindSymbol(name);
            if (sym.Tag == SymbolTag.Constant)
            {
                Error("constants cannot be used as lvalues: {0}", name);
            }
            else if (sym.Tag == SymbolTag.Global)
            {
                EmitComment("push address of global '{0}'", sym.Name);
                Symbol temp = NaivePushTemporary();
                EmitAsm("LDA", new AsmOperand(LowByte(sym.Value), AddressMode.Immediate).WithComment("#<" + sym.Name));
                EmitAsm("STA", LowByte(temp));
                EmitAsm("LDA", new AsmOperand(HighByte(sym.Value), AddressMode.Immediate).WithComment("#>" + sym.Name));
                EmitAsm("STA", HighByte(temp));
            }
            else if (sym.Tag == SymbolTag.Local)
            {
                EmitComment("push address of local '{0}'", sym.Name);
                Symbol temp = NaivePushTemporary();
                // Calculate the address of the local; it will be in the zero page:
                EmitAsm("TXA");
                EmitAsm("CLC");
                EmitAsm("ADC", new AsmOperand(OffsetOfLocal(sym), AddressMode.Immediate).WithComment("#<" + sym.Name));
                EmitAsm("STA", LowByte(temp));
                EmitAsm("LDA", new AsmOperand(0, AddressMode.Immediate).WithComment("#>" + sym.Name));
                EmitAsm("STA", HighByte(temp));
            }
            else
            {
                Program.UnhandledCase();
            }
        }
        else if (expr.Match(Tag.Load, out subexpr))
        {
            NaivePush(subexpr);
        }
        else
        {
            Error("expression cannot be used as an lvalue: {0}", expr.Show());
        }
    }

    Symbol NaivePush(Expr expr)
    {
        int number;
        string name;
        Expr subexpr, left, right;
        if (expr.Match(Tag.Integer, out number))
        {
            EmitComment("push integer");
            Symbol temp = NaivePushTemporary();
            EmitAsm("LDA", new AsmOperand(LowByte(number), AddressMode.Immediate));
            EmitAsm("STA", LowByte(temp));
            EmitAsm("LDA", new AsmOperand(HighByte(number), AddressMode.Immediate));
            EmitAsm("STA", HighByte(temp));
        }
        else if (expr.Match(Tag.Name, out name))
        {
            Symbol sym = FindSymbol(name);
            if (sym.Tag == SymbolTag.Constant) EmitComment("push constant '{0}'", sym.Name);
            else if (sym.Tag == SymbolTag.Global) EmitComment("push global '{0}'", sym.Name);
            else if (sym.Tag == SymbolTag.Local) EmitComment("push local '{0}'", sym.Name);
            else Program.UnhandledCase();

            Symbol temp = NaivePushTemporary();
            EmitAsm("LDA", LowByte(sym));
            EmitAsm("STA", LowByte(temp));
            EmitAsm("LDA", HighByte(sym));
            EmitAsm("STA", HighByte(temp));
        }
        else if (expr.Match(Tag.Load, out subexpr))
        {
            NaivePush(subexpr);

            // TODO: Use appropriately-sized load command.

            EmitAsm("JSR", new AsmOperand("_rt_load_u16", AddressMode.Absolute));
        }
        else if (expr.Match(Tag.Add, out left, out right))
        {
            NaivePush(left);
            NaivePush(right);
            EmitAsm("JSR", new AsmOperand("_rt_add_u16", AddressMode.Absolute));
            AdjustFrameSize(-2);
        }
        else if (expr.Match(Tag.Subtract, out left, out right))
        {
            NaivePush(left);
            NaivePush(right);
            EmitAsm("JSR", new AsmOperand("_rt_sub_u16", AddressMode.Absolute));
            AdjustFrameSize(-2);
        }
        else
        {
            Program.UnhandledCase();
        }

        // The result is always pushed onto the stack:
        // TODO: Use the appropriate result type.
        CType resultType = CType.UInt16;
        return new Symbol(SymbolTag.Local, FrameSize, resultType, "$result");
    }

    void NaiveConvert(Conversion rule, CType currentType, CType desiredType)
    {
        // TODO
        if (currentType != desiredType)
        {
            EmitComment("convert TOS");
        }
    }

    void NaiveLoadNondestructive()
    {
        // Reserve space for the result, which acts like a second, dummy argument:
        NaivePushTemporary();
        EmitAsm("JSR", new AsmOperand("_rt_load_nondestructive_u16", AddressMode.Absolute));
    }

    void NaiveStore(int size)
    {
        EmitComment("store {0} bytes", size);

        string op = null;
        if (size == 1) op = "_rt_store_u8";
        else if (size == 2) op = "_rt_store_u16";
        else Program.UnhandledCase();

        EmitAsm("JSR", new AsmOperand(op, AddressMode.Absolute));
        AdjustFrameSize(-4);
    }

    void NaiveCallBinaryOperator(string op)
    {
        EmitComment("call operator '{0}'", op);
    }

    Symbol NaivePushTemporary()
    {
        EmitAsm("DEX");
        EmitAsm("DEX");
        AdjustFrameSize(+2);
        return new Symbol(SymbolTag.Local, FrameSize, CType.UInt16, "$temp");
    }

    void NaivePopTemporary()
    {
        EmitAsm("INX");
        EmitAsm("INX");
        AdjustFrameSize(-2);
    }

    int OffsetOfLocal(Symbol sym)
    {
        if (sym.Tag != SymbolTag.Local) Program.Panic("a local symbol is required");
        return FrameSize - sym.Value;
    }

    void AdjustFrameSize(int delta)
    {
        EmitComment("frame size {0} {1} => {2}", delta > 0 ? "+" : "-", Math.Abs(delta), FrameSize + delta);
        FrameSize += delta;
    }

    AsmOperand LowByte(Symbol sym)
    {
        if (sym.Tag == SymbolTag.Constant)
        {
            return new AsmOperand(LowByte(sym.Value), AddressMode.Immediate).WithComment("#<" + sym.Name);
        }
        else if (sym.Tag == SymbolTag.Global)
        {
            return new AsmOperand(sym.Value, AddressMode.Absolute).WithComment(sym.Name);
        }
        else if (sym.Tag == SymbolTag.Local)
        {
            return new AsmOperand(OffsetOfLocal(sym), AddressMode.ZeroPageX).WithComment(sym.Name);
        }
        else
        {
            Program.UnhandledCase();
            return null;
        }
    }

    AsmOperand HighByte(Symbol sym)
    {
        int size = SizeOf(sym.Type);

        if (size < 2)
        {
            return new AsmOperand(0, AddressMode.Immediate).WithComment("...zero-extended");
        }
        else if (sym.Tag == SymbolTag.Constant)
        {
            return new AsmOperand(HighByte(sym.Value), AddressMode.Immediate).WithComment("#>" + sym.Name);
        }
        else if (sym.Tag == SymbolTag.Global)
        {
            return new AsmOperand(sym.Value + 1, AddressMode.Absolute).WithComment(sym.Name + "+1");
        }
        else if (sym.Tag == SymbolTag.Local)
        {
            return new AsmOperand(OffsetOfLocal(sym) + 1, AddressMode.ZeroPageX).WithComment(sym.Name + "+1");
        }
        else
        {
            Program.UnhandledCase();
            return null;
        }
    }

    static int LowByte(int n)
    {
        return n & 0xFF;
    }

    static int HighByte(int n)
    {
        return (n >> 8) & 0xFF;
    }

    CType TypeOf(Expr expr)
    {
        string name;
        Expr left, right, subexpr;
        if (expr.MatchTag(Tag.Integer))
        {
            return CType.UInt16;
        }
        else if (expr.Match(Tag.Name, out name))
        {
            return FindSymbol(name).Type;
        }
        else if (expr.Match(Tag.Load, out subexpr))
        {
            return DereferencePointerType(TypeOf(subexpr));
        }
        else if (expr.Match(Tag.Add, out left, out right) ||
            expr.Match(Tag.Subtract, out left, out right))
        {
            return FindCommonType(TypeOf(left), TypeOf(right));
        }
        else
        {
            Program.UnhandledCase();
            return null;
        }
    }

    void Emit(params object[] args)
    {
        Emit(Expr.Make(args));
    }

    void Emit(Expr e)
    {
        Output.Add(e.WithSource(SourcePosition));
    }

    void EmitAsm(string mnemonic) => Emit(Expr.MakeAsm(mnemonic));

    void EmitAsm(string mnemonic, AsmOperand operand) => Emit(Expr.MakeAsm(mnemonic, operand));

    void EmitComment(string format, params object[] args)
    {
        Emit(Tag.Comment, string.Format(format, args));
    }

    void BeginScope()
    {
        CurrentScope = new LexicalScope(CurrentScope);
    }

    void EndScope()
    {
        CurrentScope = CurrentScope.Outer;
    }

    Symbol FindSymbol(string name)
    {
        Symbol sym;

        for (LexicalScope scope = CurrentScope; scope != null; scope = scope.Outer)
        {
            if (scope.Symbols.TryGetValue(name, out sym))
            {
                return sym;
            }
        }

        Error("reference to undefined symbol: {0}", name);
        return null;
    }

    Symbol DeclareGlobal(MemoryRegion region, CType type, string name)
    {
        // TODO: Allocate an address from the appropriate region.

        return DeclareSymbol(new Symbol(SymbolTag.Global, 0x80, type, name));
    }

    Symbol DeclareLocal(CType type, string name, bool isParameter)
    {
        // Parameters always reserve two bytes:
        int size = isParameter ? SizeOf(CType.UInt16) : SizeOf(type);
        if (size < 1) Program.Panic("locals must have nonzero size");
        AdjustFrameSize(size);
        int offset = FrameSize;
        return DeclareSymbol(new Symbol(SymbolTag.Local, offset, type, name));
    }

    Symbol DeclareTemporary(CType type)
    {
        int i = 0;
        while (true)
        {
            string name = string.Format("$temp{0}", i);
            if (!CurrentScope.Symbols.ContainsKey(name))
            {
                return DeclareLocal(type, name, isParameter: false);
            }
            i += 1;
        }
    }

    Symbol DeclareSymbol(Symbol r)
    {
        // It is an error to define two things with the same name in the same scope.
        if (CurrentScope.Symbols.ContainsKey(r.Name))
        {
            Error("symbols cannot be redefined: {0}", r.Name);
        }

        CurrentScope.Symbols.Add(r.Name, r);
        EmitComment("symbol {0} is {1}", r.Name, r);
        return r;
    }

    static CType FindCommonType(CType left, CType right)
    {
        if (left == right)
        {
            return left;
        }
        else if ((left == CType.UInt8 && right == CType.UInt16) ||
            (left == CType.UInt16 && right == CType.UInt8))
        {
            return CType.UInt16;
        }
        else
        {
            Program.NYI();
            return null;
        }
    }

    CType DereferencePointerType(CType pointer)
    {
        if (!pointer.IsPointer) Error("a pointer type is required");
        return pointer.Subtype;
    }

    AggregateInfo GetAggregateInfo(string name)
    {
        AggregateInfo info;
        if (!AggregateTypes.TryGetValue(name, out info)) Error("struct or union not defined: {0}", name);
        return info;
    }

    static string AggregateLayoutToString(AggregateLayout layout)
    {
        if (layout == AggregateLayout.Struct) return "struct";
        else if (layout == AggregateLayout.Union) return "union";

        Program.UnhandledCase();
        return null;
    }

    int SizeOf(CType type)
    {
        if (type.IsSimple)
        {
            if (type.SimpleType == CSimpleType.Void) return 0;
            else if (type.SimpleType == CSimpleType.UInt8) return 1;
            else if (type.SimpleType == CSimpleType.UInt16) return 2;
            else
            {
                Program.Panic("cannot get size of an 'implied' type");
                return 1;
            }
        }
        else if (type.IsPointer)
        {
            return 2;
        }
        else if (type.IsStructOrUnion)
        {
            AggregateInfo info = GetAggregateInfo(type.Name);
            return info.TotalSize;
        }
        else if (type.IsArray)
        {
            return type.Dimension * SizeOf(type.Subtype);
        }

        Program.NYI();
        return 1;
    }

    void EmitCall(string functionName, int argCount)
    {
        // Copy the arguments into the function's call frame:
        CFunctionInfo function;
        if (!Functions.TryGetValue(functionName, out function))
        {
            Error("function not implemented: {0}", functionName);
        }

        if (argCount != function.Parameters.Length)
        {
            Error("wrong number of arguments in call to '{0}': {1} required; {2} specified",
                functionName, function.Parameters.Length, argCount);
        }

        for (int i = 0; i < function.Parameters.Length; i++)
        {
            //OperandReference arg = Peek(i);
            FieldInfo param = function.Parameters[function.Parameters.Length - 1 - i];
            //ConvertType(Conversion.Implicit, arg, param.Type);
            //Store(arg, paramName);
        }

        // TODO: The return value goes... somewhere.

        EmitAsm("JSR", new AsmOperand(functionName, AddressMode.Absolute));
    }

    [DebuggerStepThrough]
    void Warning(string format, params object[] args)
    {
        Program.Warning(SourcePosition, format, args);
    }

    [DebuggerStepThrough]
    void Error(string format, params object[] args)
    {
        Program.Error(SourcePosition, format, args);
    }

    static readonly Dictionary<string, string> UnaryRuntimeOperators = new Dictionary<string, string>
    {
        { Tag.BitwiseNot, "bitwise_not" },
        { Tag.LogicalNot, "logical_not" },
    };

    static readonly Dictionary<string, string> BinaryRuntimeOperators = new Dictionary<string, string>
    {
        { Tag.Multiply, "mul" },
        { Tag.Divide, "div" },
        { Tag.Modulus, "mod" },
        { Tag.Equal, "eq" },
        { Tag.NotEqual, "ne" },
        { Tag.LessThan, "lt" },
        { Tag.GreaterThan, "gt" },
        { Tag.LessThanOrEqual, "le" },
        { Tag.GreaterThanOrEqual, "ge" },
        { Tag.BitwiseAnd, "bitwise_and" },
        { Tag.BitwiseOr, "bitwise_or" },
        { Tag.BitwiseXor, "bitwise_xor" },
        { Tag.ShiftLeft, "shift_left" },
        { Tag.ShiftRight, "shift_right" },
    };

    static readonly string[] BinaryOperatorsThatAllowPointers = new string[]
    {
        Tag.Equal,
        Tag.NotEqual,
        Tag.LessThan,
        Tag.GreaterThan,
        Tag.LessThanOrEqual,
        Tag.GreaterThanOrEqual,
    };

    static string GetRuntimeFunctionName(string operation, CType type)
    {
        string typeSuffix = null;

        if (type == CType.UInt8) typeSuffix = "u8";
        else if (type == CType.UInt16) typeSuffix = "u16";
        else Program.Panic("invalid combination of operator and type: {0}, {1}", operation, type.Show());

        return "_rt_" + operation + "_" + typeSuffix;
    }
}

class Symbol
{
    public readonly SymbolTag Tag;
    public readonly int Value;
    public readonly CType Type;
    public readonly string Name = "<unnamed>";

    public Symbol(SymbolTag tag, int value, CType type, string name)
    {
        Tag = tag;
        Value = value;
        Type = type;
        Name = name;
    }

    public override string ToString()
    {
        return string.Format("Symbol({0}, {1}, {2}, {3})", Tag, Value, Type.Show(), Name);
    }
}

enum SymbolTag
{
    Constant,
    Global,
    Local,
}

class Continuation
{
    public static readonly Continuation Fallthrough = null;
}

class LoopScope
{
    public LoopScope Outer;
    public string ContinueLabel;
    public string BreakLabel;
}

class LexicalScope
{
    public readonly LexicalScope Outer;
    public readonly Dictionary<string, Symbol> Symbols = new Dictionary<string, Symbol>();

    public LexicalScope(LexicalScope outer)
    {
        Outer = outer;
    }
}

enum Conversion
{
    Implicit,
    Explicit,
}
