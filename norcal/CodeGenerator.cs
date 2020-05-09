using System;
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

    // Global allocation:
    // (Reserve the top half of zero page for the parameter stack.)
    AllocationRegion ZeroPageRegion = new AllocationRegion("zero page RAM", 0, 0x80);
    AllocationRegion OamRegion = new AllocationRegion("OAM", 0x100, 0x200);
    AllocationRegion RamRegion = new AllocationRegion("RAM", 0x300, 0x800);

    // The current function:
    string CurrentFunctionName = null;
    CType ReturnType = null;
    // The return value always goes in the top two bytes of the call frame.
    Symbol ReturnValue = null;
    int FrameSize = 0;

    // Local scope info:
    LexicalScope CurrentScope;
    LoopScope Loop;

    // Option: Show more information.
    static readonly bool ShowVerboseComments = false;

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
            if (decl.Match(Tag.Function, out returnType, out functionName, out parsedFields, out body))
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
                ReturnType = returnType;
                FrameSize = 0;

                if (name == "reset")
                {
                    EmitComment("program setup");
                    // The stack pointer begins at the top of zero page.
                    EmitAsm("LDX", new AsmOperand(0, AddressMode.Immediate));
                }

                // Always declare a local variable to represent the return value.
                // (This is mostly only useful for assembly code.)
                ReturnValue = DeclareSymbol(new Symbol(SymbolTag.Local, 2, CType.UInt16, "__result"));

                foreach (FieldInfo field in fields)
                {
                    DeclareLocal(field.Type, field.Name, isParameter: true);
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
        Expr init, test, induct, body;
        Expr[] block, parts;
        CType type;
        string name, op, mnemonic;
        AsmOperand operand;
        if (expr.MatchAny(Tag.Sequence, out block))
        {
            foreach (Expr stmt in block)
            {
                EmitVerboseComment("STATEMENT: " + stmt.Show());
                Compile(stmt);
            }
        }
        else if (expr.Match(Tag.Variable, out type, out name))
        {
            DeclareLocal(type, name, isParameter: false);
        }
        else if (expr.MatchTag(Tag.Label))
        {
            // Pass through:
            Emit(expr);
        }
        else if (expr.Match(Tag.Asm, out mnemonic, out operand))
        {
            // If the operand refers to a local variable, replace it with an appropriate stack-relative operand.
            AsmOperand fixedOperand = operand;
            Symbol sym;
            if (operand.Base.HasValue && TryFindSymbol(operand.Base.Value, out sym) && sym.Tag == SymbolTag.Local)
            {
                // Use the offset of the local variable plus any offset specified by the assembly source code:
                int totalOffset = OffsetOfLocal(sym) + operand.Offset;

                AddressMode actualMode = operand.Mode;
                if (operand.Mode == AddressMode.Absolute) actualMode = AddressMode.ZeroPageX;
                else if (operand.Mode == AddressMode.Indirect) actualMode = AddressMode.IndirectX;
                else Error("invalid address mode for local variable");

                string comment = sym.Name;
                if (operand.Offset != 0) comment += ("+" + operand.Offset);

                fixedOperand = new AsmOperand(totalOffset, actualMode).WithComment(comment);
            }
            Emit(Tag.Asm, mnemonic, fixedOperand);
        }
        else if (expr.Match(Tag.For, out init, out test, out induct, out body))
        {
            BeginScope();
            Compile(init);
            EmitComment("for_top");
            Compile(test);
            EmitComment("break if done");
            Compile(body);
            Compile(induct);
            EmitComment("jump to top");
            EndScope();
        }
        else if (expr.MatchAny(Tag.If, out parts))
        {
            EmitComment("if...");
            foreach (Expr p in parts) Compile(p);
            EmitComment("end if");
        }
        else if (expr.Match(Tag.Assign, out left, out right))
        {
            int leftSize = SizeOf(TypeOf(left));
            int rightSize = SizeOf(TypeOf(right));
            if (leftSize > 2) NYI(left, "type is too large for assignment");
            if (rightSize > 2) NYI(right, "type is too large for assignment");

            Symbol leftSymbol, rightSymbol;
            if (TryGetSymbol(left, out leftSymbol) && TryGetSymbol(right, out rightSymbol))
            {
                EmitAsm("LDA", LowByte(rightSymbol));
                EmitAsm("STA", LowByte(leftSymbol));

                if (leftSize == 2)
                {
                    EmitAsm("LDA", HighByte(rightSymbol));
                    EmitAsm("STA", HighByte(leftSymbol));
                }
            }
            else
            {
                NYI(expr);
            }
        }
        else
        {
            NYI(expr);
        }
    }

    /// <summary>
    /// If this is a simple enough expression, return the corresponding symbol.
    /// </summary>
    bool TryGetSymbol(Expr expr, out Symbol symbol)
    {
        string name;
        if (expr.Match(Tag.Name, out name))
        {
            symbol = FindSymbol(name);
            return true;
        }
        else
        {
            symbol = null;
            return false;
        }
    }

    void ReturnFromFunction()
    {
        EmitComment("epilogue");
        // Discard all locals, except for the return value.
        for (int i = 0; i < FrameSize - 2; i++)
        {
            EmitAsm("INX");
        }
        EmitAsm("RTS");
    }

    int OffsetOfLocal(Symbol sym)
    {
        if (sym.Tag != SymbolTag.Local) Program.Panic("a local symbol is required");
        return FrameSize - sym.Value;
    }

    void AdjustFrameSize(int delta)
    {
        if (delta != 0)
        {
            EmitVerboseComment("frame size {0} {1} => {2}", delta > 0 ? "+" : "-", Math.Abs(delta), FrameSize + delta);
            FrameSize += delta;
        }
    }

    void AllocateFrameBytes(int delta)
    {
        string op = (delta > 0) ? "DEX" : "INX";
        int count = Math.Abs(delta);

        for (int i = 0; i < count; i++)
        {
            EmitAsm(op);
        }

        AdjustFrameSize(delta);
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
        string name, fieldName, functionName;
        Expr left, right, subexpr, cond, functionExpr;
        Expr[] rest;
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
        else if (expr.Match(Tag.Index, out left, out right))
        {
            CType arrayType = TypeOf(left);
            if (!arrayType.IsArray && !arrayType.IsPointer) Error("an array or pointer is required");
            return arrayType.Subtype;
        }
        else if (expr.Match(Tag.Field, out left, out fieldName))
        {
            CType structType = TypeOf(left);
            if (!structType.IsStructOrUnion) Error("a struct or union is required");
            AggregateInfo info = GetAggregateInfo(structType.Name);
            foreach (FieldInfo field in info.Fields)
            {
                if (field.Name == fieldName) return field.Type;
            }
            Error("field does not exist: {0}", fieldName);
            return CType.Void;
        }
        else if (expr.Match(Tag.Conditional, out cond, out left, out right))
        {
            return FindCommonType(TypeOf(left), TypeOf(right));
        }
        else if (expr.Match(Tag.AddressOf, out subexpr))
        {
            return CType.MakePointer(TypeOf(subexpr));
        }
        else if (expr.Match(Tag.PreIncrement, out subexpr) ||
            expr.Match(Tag.PreDecrement, out subexpr) ||
            expr.Match(Tag.PostIncrement, out subexpr) ||
            expr.Match(Tag.PostDecrement, out subexpr))
        {
            return TypeOf(subexpr);
        }
        else if (expr.MatchAny(Tag.Call, out functionExpr, out rest) && functionExpr.Match(Tag.Name, out functionName))
        {
            CFunctionInfo functionInfo;
            if (!Functions.TryGetValue(functionName, out functionInfo))
            {
                Error("undefined function: {0}", functionName);
            }
            return functionInfo.ReturnType;
        }
        else if (BinaryOperatorsThatProduceIntegers.Contains(expr.GetTag()))
        {
            return CType.UInt16;
        }
        else
        {
            Program.UnhandledCase();
            return null;
        }
    }

    /// <summary>
    /// Binary operators that always return a uint16_t.
    /// </summary>
    static readonly string[] BinaryOperatorsThatProduceIntegers = new string[]
    {
        Tag.Multiply,
        Tag.Divide,
        Tag.Modulus,
        Tag.ShiftLeft,
        Tag.ShiftRight,
        Tag.Equal,
        Tag.NotEqual,
        Tag.BitwiseOr,
        Tag.BitwiseAnd,
        Tag.LogicalOr,
        Tag.LogicalAnd,
    };

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

    // TODO: Report a fatal error if this is hit.
    void NYI(Expr expr, string message = null)
    {
        if (message == null)
        {
            EmitComment("NYI: {0}", expr.Show());
        }
        else
        {
            EmitComment("NYI: {0}: {1}", message, expr.Show());
        }
    }

    void EmitVerboseComment(string format, params object[] args)
    {
        if (ShowVerboseComments)
        {
            Emit(Tag.Comment, string.Format(format, args));
        }
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
        if (TryFindSymbol(name, out sym)) return sym;
        Error("reference to undefined symbol: {0}", name);
        return null;
    }

    bool TryFindSymbol(string name, out Symbol found)
    {
        for (LexicalScope scope = CurrentScope; scope != null; scope = scope.Outer)
        {
            if (scope.Symbols.TryGetValue(name, out found))
            {
                return true;
            }
        }

        found = null;
        return false;
    }

    Symbol DeclareGlobal(MemoryRegion region, CType type, string name)
    {
        int size = SizeOf(type);

        // Reserve memory in the specified region.
        int address;
        if (region.Tag == MemoryRegionTag.ZeroPage) address = Allocate(ZeroPageRegion, size);
        else if (region.Tag == MemoryRegionTag.Oam) address = Allocate(OamRegion, size);
        else if (region.Tag == MemoryRegionTag.Ram) address = Allocate(RamRegion, size);
        else if (region.Tag == MemoryRegionTag.Fixed)
        {
            if (region.FixedAddress < 0 || region.FixedAddress > ushort.MaxValue) Program.Error("Invalid address.");
            address = region.FixedAddress;
        }
        else
        {
            Program.NYI();
            address = -1;
        }

        Emit(Tag.Variable, address, size, name);
        return DeclareSymbol(new Symbol(SymbolTag.Global, address, type, name));
    }

    int Allocate(AllocationRegion allocator, int size)
    {
        if (allocator.Next + size > allocator.Top) Program.Error("Not enough {0} to allocate global.", allocator.Name);
        int address = allocator.Next;
        allocator.Next += size;
        return address;
    }

    Symbol DeclareLocal(CType type, string name, bool isParameter)
    {
        // Parameters always reserve two bytes:
        int size = isParameter ? SizeOf(CType.UInt16) : SizeOf(type);
        if (size < 1) Program.Panic("locals must have nonzero size");
        if (isParameter)
        {
            AdjustFrameSize(+size);
        }
        else
        {
            EmitComment("reserve {0} byte(s) for local", size);
            AllocateFrameBytes(size);
        }
        return DeclareSymbol(new Symbol(SymbolTag.Local, FrameSize, type, name));
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

    void CallRuntimeFunction(string operation, CType type, int stackEffect)
    {
        string name = GetRuntimeFunctionName(operation, type);
        EmitAsm("JSR", new AsmOperand(name, AddressMode.Absolute));
        AdjustFrameSize(stackEffect);
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

class AllocationRegion
{
    public readonly string Name;
    public readonly int Bottom;
    public readonly int Top;
    public int Next;

    public AllocationRegion(string name, int bottom, int top)
    {
        Name = name;
        Bottom = bottom;
        Next = bottom;
        Top = top;
    }
}
