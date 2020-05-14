using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

class CodeGenerator
{
    List<Expr> Output = new List<Expr>();
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
    int FrameSize = 0;
    int NextLocalOffset;
    int NextLabelNumber = 0;

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
                    int fieldSize = SizeOf(decl, parsedFields[i].Type);
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
                if (Functions.ContainsKey(functionName)) Error(decl, "function is already defined: " + functionName);

                // Calculate the offset of each parameter:
                FieldInfo[] fields = new FieldInfo[parsedFields.Length];
                int offset = 0;
                for (int i = 0; i < parsedFields.Length; i++)
                {
                    fields[i] = new FieldInfo(parsedFields[i].Type, parsedFields[i].Name, offset);
                    int fieldSize = SizeOf(decl, parsedFields[i].Type);
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
                DeclareSymbol(decl, new Symbol(SymbolTag.Constant, number, type, name));
            }
            else if (decl.Match(Tag.Variable, out region, out type, out name))
            {
                DeclareGlobal(decl, region, type, name);
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
                        Error(
                            decl,
                            "declared size of array ({0}) is too small for the number of specified values ({1})",
                            type.Dimension, values.Length);
                    }
                }
                else if (values.Length != 1)
                {
                    Program.Panic(decl.Source, "non-array initializers must contain exactly one value");
                }

                // Convert the data to raw bytes:
                byte[] bytes = new byte[SizeOf(decl, type)];
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

                DeclareSymbol(decl, new Symbol(SymbolTag.Constant, 0, type, name));
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
                NextLabelNumber = 0;

                if (name == "reset")
                {
                    EmitComment("program setup");
                    // The stack pointer begins at the top of zero page.
                    EmitAsm("LDX", new AsmOperand(0, AddressMode.Immediate));
                }

                // Figure out the total frame size needed for parameters and locals:
                FrameSize = SizeOfLocals(body);
                foreach (FieldInfo field in fields)
                {
                    FrameSize += SizeOf(decl, field.Type);
                }
                NextLocalOffset = FrameSize;

                for (int i = 0; i < FrameSize; i++)
                {
                    EmitAsm("DEX");
                }

                // Declare parameters as local symbols:
                foreach (FieldInfo field in fields)
                {
                    DeclareLocal(decl, field.Type, field.Name);
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
        string name, mnemonic, fieldName;
        AsmOperand operand;

        EmitComment("SRC: {0}", expr.Show());

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
            DeclareLocal(expr, type, name);
        }
        else if (expr.MatchTag(Tag.Label))
        {
            // Pass through:
            Emit(expr);
        }
        else if (expr.Match(Tag.Asm, out mnemonic, out operand))
        {
            // If the operand refers to a constant or variable, replace it with the actual address.
            AsmOperand fixedOperand = operand;
            Symbol sym;
            if (operand.Base.HasValue && TryFindSymbol(operand.Base.Value, out sym))
            {
                int baseValue;
                AddressMode actualMode = operand.Mode;
                if (sym.Tag == SymbolTag.Constant || sym.Tag == SymbolTag.Global)
                {
                    baseValue = sym.Value;
                }
                else if (sym.Tag == SymbolTag.Local)
                {
                    // Use the offset of the local variable plus any offset specified by the assembly source code:
                    baseValue = OffsetOfLocal(sym) + operand.Offset;

                    // Convert the address mode to an appropriate stack-relative mode:
                    if (operand.Mode == AddressMode.Absolute) actualMode = AddressMode.ZeroPageX;
                    else if (operand.Mode == AddressMode.Indirect) actualMode = AddressMode.IndirectX;
                    else Error(expr, "invalid address mode for local variable");
                }
                else
                {
                    Program.UnhandledCase();
                    baseValue = 0;
                }

                string comment = sym.Name;
                if (operand.Offset != 0) comment += ("+" + operand.Offset);
                fixedOperand = operand.ReplaceBase(baseValue).WithMode(actualMode).WithComment(comment);
            }
            Emit(Tag.Asm, mnemonic, fixedOperand);
        }
        else if (expr.Match(Tag.For, out init, out test, out induct, out body))
        {
            AsmOperand top = MakeUniqueLabel("for");
            AsmOperand bottom = MakeUniqueLabel("for_break");

            BeginScope();
            Compile(init);
            EmitLabel(top);
            CompileJumpIf(false, test, bottom);
            Compile(body);
            Compile(induct);
            EmitAsm("JMP", top);
            EmitLabel(bottom);
            EndScope();
        }
        else if (expr.MatchAny(Tag.If, out parts))
        {
            AsmOperand endIf = MakeUniqueLabel("end_if");
            for (int i = 0; i < parts.Length; i += 2)
            {
                test = parts[i];
                body = parts[i + 1];
                AsmOperand nextClause = MakeUniqueLabel("next_clause");
                CompileJumpIf(false, test, nextClause);
                Compile(body);
                EmitAsm("JMP", endIf);
                EmitLabel(nextClause);
            }
            EmitLabel(endIf);
        }
        else if (expr.Match(Tag.Assign, out left, out right))
        {
            int leftSize = SizeOf(left);
            int rightSize = SizeOf(right);

            if (leftSize == 0) Error(left, "void expressions cannot be used in assignment");
            if (rightSize == 0) Error(right, "void expressions cannot be used in assignment");
            if (leftSize > 2) Error(left, "type is too large for assignment");
            if (rightSize > 2) Error(right, "type is too large for assignment");

            // Expressions involving values wider than one byte are more difficult to compile.
            bool wide = leftSize > 1 || rightSize > 1;

            Expr loadExpr, pointerExpr, arrayExpr, indexExpr;
            AsmOperand leftOperand, rightOperand, basePointer, indexOperand;
            WideOperand leftWideOperand, rightWideOperand, pointerWideOperand;

            if (!wide &&
                left.Match(Tag.Index, out arrayExpr, out indexExpr) &&
                TryGetPointerOperand(arrayExpr, out basePointer) &&
                TryGetOperand(indexExpr, out indexOperand) &&
                TryGetOperand(right, out rightOperand))
            {
                // Pattern:
                // array[i] = c;
                //
                // LDY index_subexpression
                // LDA right
                // STA (array),Y

                EmitAsm("LDA", rightOperand);
                EmitAsm("LDY", indexOperand);
                EmitAsm("STA", basePointer);
            }
            else if (wide && TryGetWideOperand(left, out leftWideOperand) && TryGetWideOperand(right, out rightWideOperand))
            {
                // Pattern:
                // a = b;
                // (for operands that can be addressed directly)

                if (leftWideOperand.Low.Mode == AddressMode.Immediate)
                {
                    Error(left, "an assignable expression is required");
                }

                EmitAsm("LDA", rightWideOperand.Low);
                EmitAsm("STA", leftWideOperand.Low);

                if (leftSize == 2)
                {
                    EmitAsm("LDA", rightWideOperand.High);
                    EmitAsm("STA", leftWideOperand.High);
                }
            }
            else if (wide &&
                TryGetWideOperand(left, out leftWideOperand) &&
                right.Match(Tag.Field, out loadExpr, out fieldName) &&
                loadExpr.Match(Tag.Load, out pointerExpr) &&
                TryGetWideOperand(pointerExpr, out pointerWideOperand) &&
                SizeOf(left) == 2 &&
                SizeOf(right) == 2)
            {
                // Pattern:
                // u8 *p = board->array;
                //
                // LDA board
                // CLC
                // ADC <offsetof(array)
                // STA p
                // LDA board+1
                // ADC >offsetof(array)
                // STA p+1

                if (leftWideOperand.Low.Mode == AddressMode.Immediate)
                {
                    Error(left, "an assignable expression is required");
                }

                FieldInfo field = GetFieldInfo(loadExpr, fieldName);

                if (!field.Type.IsArray)
                {
                    Program.Panic("only arrays are supported");
                }

                EmitAsm("LDA", pointerWideOperand.Low);
                EmitAsm("CLC");
                EmitAsm("ADC", new AsmOperand(LowByte(field.Offset), AddressMode.Immediate));
                EmitAsm("STA", leftWideOperand.Low);
                EmitAsm("LDA", pointerWideOperand.High);
                EmitAsm("ADC", new AsmOperand(HighByte(field.Offset), AddressMode.Immediate));
                EmitAsm("STA", leftWideOperand.High);
            }
            else
            {
                NYI(wide ? "WIDE" : "NARROW");
            }
        }
        else if ((expr.Match(Tag.PreIncrement, out subexpr) || expr.Match(Tag.PostIncrement, out subexpr)) &&
            TryGetOperand(subexpr, out operand))
        {
            EmitAsm("INC", operand);
        }
        else if ((expr.Match(Tag.PreDecrement, out subexpr) || expr.Match(Tag.PostDecrement, out subexpr)) &&
            TryGetOperand(subexpr, out operand) &&
            SizeOf(subexpr) == 1)
        {
            EmitAsm("DEC", operand);
        }
        else
        {
            NYI();
        }
    }

    void CompileJumpIf(bool condition, Expr expr, AsmOperand target)
    {
        AsmOperand operand, leftOperand, rightOperand;
        Expr left, right;

        if (TryGetOperand(expr, out operand))
        {
            if (operand.Mode == AddressMode.Immediate)
            {
                if ((operand.Offset != 0) == condition)
                {
                    EmitAsm("JMP", target);
                }
            }
            else
            {
                EmitAsm("LDA", operand);
                string opcode = condition ? "BNE" : "BEQ";
                EmitAsm(opcode, target);
            }
        }
        else if (expr.Match(Tag.LessThan, out left, out right) &&
            TryGetOperand(left, out leftOperand) &&
            TryGetOperand(right, out rightOperand))
        {
            // Pattern:
            // if (a < b) ...
            //
            // LDA b
            // CMP a
            // (carry is clear if CMP's operand is larger)
            // BCC/BCS target

            EmitAsm("LDA", leftOperand);
            EmitAsm("CMP", rightOperand);
            string opcode = condition ? "BCC" : "BCS";
            EmitAsm(opcode, target);
        }
        else if (expr.Match(Tag.GreaterThanOrEqual, out left, out right))
        {
            CompileJumpIf(!condition, Expr.Make(Tag.LessThan, left, right), target);
        }
        else
        {
            NYI("jump if " + condition);
        }
    }

    /// <summary>
    /// If this expression can be represented by a single operand, produce it.
    /// </summary>
    bool TryGetOperand(Expr expr, out AsmOperand operand)
    {
        int size = SizeOf(expr);

        int number;
        string name;
        if (size != 1)
        {
            operand = null;
            return false;
        }
        else if (expr.Match(Tag.Integer, out number))
        {
            if (number > 255) Program.Panic("immediate is too large for one byte");
            operand = new AsmOperand(number, AddressMode.Immediate).WithComment("'literal'");
            return true;
        }
        else if (expr.Match(Tag.Name, out name))
        {
            Symbol sym = FindSymbol(expr, name);
            if (sym.Tag == SymbolTag.Constant)
            {
                operand = new AsmOperand(LowByte(sym.Value), AddressMode.Immediate).WithComment("#" + sym.Name);
            }
            else if (sym.Tag == SymbolTag.Global)
            {
                operand = new AsmOperand(sym.Value, AddressMode.Absolute).WithComment(sym.Name);
            }
            else if (sym.Tag == SymbolTag.Local)
            {
                operand = new AsmOperand(OffsetOfLocal(sym), AddressMode.ZeroPageX).WithComment(sym.Name);
            }
            else
            {
                Program.UnhandledCase();
                operand = null;
            }
            return true;
        }
        else
        {
            operand = null;
            return false;
        }
    }

    /// <summary>
    /// If this expression can be represented by a pair of operands, produce it.
    /// </summary>
    bool TryGetWideOperand(Expr expr, out WideOperand operand)
    {
        int size = SizeOf(expr);
        if (size < 1) Program.Panic("expression must have size of at least 1");

        operand = new WideOperand();
        int number;
        string name;
        if (expr.Match(Tag.Integer, out number))
        {
            operand.Low = new AsmOperand(LowByte(number), AddressMode.Immediate).WithComment("<'literal'");
            operand.High = new AsmOperand(HighByte(number), AddressMode.Immediate).WithComment(">'literal'");
            return true;
        }
        else if (expr.Match(Tag.Name, out name))
        {
            Symbol sym = FindSymbol(expr, name);

            if (sym.Tag == SymbolTag.Constant)
            {
                operand.Low = new AsmOperand(LowByte(sym.Value), AddressMode.Immediate).WithComment("#<" + sym.Name);
                operand.High = new AsmOperand(HighByte(sym.Value), AddressMode.Immediate).WithComment("#>" + sym.Name);
            }
            else if (sym.Tag == SymbolTag.Global)
            {
                operand.Low = new AsmOperand(sym.Value, AddressMode.Absolute).WithComment(sym.Name);
                operand.High = new AsmOperand(sym.Value + 1, AddressMode.Absolute).WithComment(sym.Name + "+1");
            }
            else if (sym.Tag == SymbolTag.Local)
            {
                operand.Low = new AsmOperand(OffsetOfLocal(sym), AddressMode.ZeroPageX).WithComment(sym.Name);
                operand.High = new AsmOperand(OffsetOfLocal(sym) + 1, AddressMode.ZeroPageX).WithComment(sym.Name + "+1");
            }
            else
            {
                Program.UnhandledCase();
            }

            if (size < 2)
            {
                operand.High = new AsmOperand(0, AddressMode.Immediate).WithComment("...zero-extended");
            }

            return true;
        }
        else
        {
            operand = null;
            return false;
        }
    }

    bool TryGetPointerOperand(Expr expr, out AsmOperand baseAddress)
    {
        WideOperand operand;
        if (TryGetWideOperand(expr, out operand) && operand.Low.Mode == AddressMode.Absolute && operand.Low.Offset < 0x100)
        {
            baseAddress = operand.Low.WithMode(AddressMode.IndirectY).WithComment("({0}),Y", operand.Low.Comment);
            return true;
        }

        baseAddress = null;
        return false;
    }

    int SizeOfLocals(Expr expr)
    {
        CType type;
        string name;
        Expr[] body;

        if (expr.MatchAny(Tag.Sequence, out body) ||
            expr.MatchAny(Tag.If, out body) ||
            expr.MatchAny(Tag.For, out body))
        {
            return body.Select(SizeOfLocals).Sum();
        }
        else if (expr.Match(Tag.Variable, out type, out name))
        {
            return SizeOf(expr, type);
        }
        else
        {
            return 0;
        }
    }

    void ReturnFromFunction()
    {
        for (int i = 0; i < FrameSize; i++)
        {
            EmitAsm("INX");
        }
        EmitAsm("RTS");
    }

    int OffsetOfLocal(Symbol sym)
    {
        if (sym.Tag != SymbolTag.Local) Program.Panic("a local symbol is required");
        return sym.Value;
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
        CType type = TypeOfWithoutDecay(expr);
        // In most contexts, arrays are treated as a pointer to their first element.
        if (type.IsArray) return CType.MakePointer(type.Subtype);
        return type;
    }

    CType TypeOfWithoutDecay(Expr expr)
    {
        string name, fieldName, functionName, op;
        int number;
        Expr left, right, subexpr, cond, functionExpr;
        Expr[] rest;

        if (expr.Match(Tag.Integer, out number))
        {
            return (number < 256) ? CType.UInt8 : CType.UInt16;
        }
        else if (expr.Match(Tag.Name, out name))
        {
            return FindSymbol(expr, name).Type;
        }
        else if (expr.Match(Tag.Load, out subexpr))
        {
            return DereferencePointerType(subexpr, TypeOf(subexpr));
        }
        else if (expr.Match(Tag.Add, out left, out right) ||
            expr.Match(Tag.Subtract, out left, out right))
        {
            return FindCommonType(TypeOf(left), TypeOf(right));
        }
        else if (expr.Match(Tag.Index, out left, out right))
        {
            CType arrayType = TypeOf(left);
            if (!arrayType.IsArray && !arrayType.IsPointer) Error(left, "an array or pointer is required");
            return arrayType.Subtype;
        }
        else if (expr.Match(Tag.Field, out left, out fieldName))
        {
            return GetFieldInfo(left, fieldName).Type;
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
                Error(functionExpr, "undefined function: {0}", functionName);
            }
            return functionInfo.ReturnType;
        }
        else if (expr.MatchAnyTag(out op, out subexpr) && UnaryOperatorsThatAlwaysProduceIntegers.Contains(op))
        {
            return TypeOf(subexpr);
        }
        else if (expr.MatchAnyTag(out op, out left, out right) && BinaryOperatorsThatAlwaysProduceIntegers.Contains(op))
        {
            return FindCommonType(TypeOf(left), TypeOf(right));
        }
        else
        {
            Program.UnhandledCase();
            return null;
        }
    }

    /// <summary>
    /// Unary operators that always return an integer.
    /// </summary>
    static readonly string[] UnaryOperatorsThatAlwaysProduceIntegers = new string[]
    {
        Tag.BitwiseNot,
        Tag.LogicalNot,
    };

    /// <summary>
    /// Binary operators that always return an integer.
    /// </summary>
    static readonly string[] BinaryOperatorsThatAlwaysProduceIntegers = new string[]
    {
        Tag.Multiply,
        Tag.Divide,
        Tag.Modulus,
        Tag.ShiftLeft,
        Tag.ShiftRight,
        Tag.Equal,
        Tag.NotEqual,
        Tag.LessThan,
        Tag.LessThanOrEqual,
        Tag.GreaterThan,
        Tag.GreaterThanOrEqual,
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
        Output.Add(e);
    }

    void EmitAsm(string mnemonic) => Emit(Expr.MakeAsm(mnemonic));

    void EmitAsm(string mnemonic, AsmOperand operand) => Emit(Expr.MakeAsm(mnemonic, operand));

    void EmitLabel(AsmOperand operand)
    {
        if (operand.Mode != AddressMode.Absolute) Program.Panic("labels declarations must use absolute address mode");
        Emit(Tag.Label, operand.Base.Value);
    }

    void EmitComment(string format, params object[] args)
    {
        Emit(Tag.Comment, string.Format(format, args));
    }

    // TODO: Report a fatal error if this is hit in release builds.
    void NYI(string message = null)
    {
        if (message == null)
        {
            EmitComment("NYI");
        }
        else
        {
            EmitComment("NYI: {0}", message);
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

    Symbol FindSymbol(Expr origin, string name)
    {
        Symbol sym;
        if (TryFindSymbol(name, out sym)) return sym;
        Error(origin, "reference to undefined symbol: {0}", name);
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

    Symbol DeclareGlobal(Expr origin, MemoryRegion region, CType type, string name)
    {
        int size = SizeOf(origin, type);

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

        return DeclareSymbol(origin, new Symbol(SymbolTag.Global, address, type, name));
    }

    int Allocate(AllocationRegion allocator, int size)
    {
        if (allocator.Next + size > allocator.Top) Program.Error("Not enough {0} to allocate global.", allocator.Name);
        int address = allocator.Next;
        allocator.Next += size;
        return address;
    }

    Symbol DeclareLocal(Expr origin, CType type, string name)
    {
        int size = SizeOf(origin, type);
        NextLocalOffset -= size;
        if (NextLocalOffset < 0) Program.Panic("call frame is too small");
        return DeclareSymbol(origin, new Symbol(SymbolTag.Local, NextLocalOffset, type, name));
    }

    Symbol DeclareSymbol(Expr origin, Symbol r)
    {
        // It is an error to define two things with the same name in the same scope.
        if (CurrentScope.Symbols.ContainsKey(r.Name))
        {
            Error(origin, "symbols cannot be redefined: {0}", r.Name);
        }

        CurrentScope.Symbols.Add(r.Name, r);
        EmitComment("symbol {0} is {1}", r.Name, r);
        return r;
    }

    AsmOperand MakeUniqueLabel(string prefix)
    {
        string name = string.Format("${0}_{1}", prefix, NextLabelNumber++);
        return new AsmOperand(name, AddressMode.Absolute);
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

    CType DereferencePointerType(Expr origin, CType pointer)
    {
        if (!pointer.IsPointer) Error(origin, "a pointer type is required");
        return pointer.Subtype;
    }

    AggregateInfo GetAggregateInfo(Expr structExpr)
    {
        CType type = TypeOf(structExpr);
        if (!type.IsStructOrUnion) Error(structExpr, "a struct or union type is required");
        return GetAggregateInfo(structExpr, type.Name);
    }

    AggregateInfo GetAggregateInfo(Expr origin, string name)
    {
        AggregateInfo info;
        if (!AggregateTypes.TryGetValue(name, out info)) Error(origin, "struct or union not defined: {0}", name);
        return info;
    }

    FieldInfo GetFieldInfo(Expr structExpr, string fieldName)
    {
        AggregateInfo info = GetAggregateInfo(structExpr);
        foreach (FieldInfo field in info.Fields)
        {
            if (field.Name == fieldName) return field;
        }
        Error(structExpr, "invalid field name: " + fieldName);
        return null;
    }

    static string AggregateLayoutToString(AggregateLayout layout)
    {
        if (layout == AggregateLayout.Struct) return "struct";
        else if (layout == AggregateLayout.Union) return "union";

        Program.UnhandledCase();
        return null;
    }

    int SizeOf(Expr expr)
    {
        return SizeOf(expr, TypeOf(expr));
    }

    int SizeOf(Expr origin, CType type)
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
            return GetAggregateInfo(origin, type.Name).TotalSize;
        }
        else if (type.IsArray)
        {
            return type.Dimension * SizeOf(origin, type.Subtype);
        }

        Program.NYI();
        return 1;
    }

    [DebuggerStepThrough]
    void Warning(Expr origin, string format, params object[] args)
    {
        Program.Warning(origin.Source, format, args);
    }

    [DebuggerStepThrough]
    void Error(Expr origin, string format, params object[] args)
    {
        Program.Error(origin.Source, format, args);
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

class WideOperand
{
    public AsmOperand Low, High;
}
