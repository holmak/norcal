using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

class CodeGenerator
{
    Stack<OutputTransaction> OutputStack = new Stack<OutputTransaction>();
    OutputTransaction Output => OutputStack.Peek();
    Dictionary<string, CFunctionInfo> Functions = new Dictionary<string, CFunctionInfo>();
    Dictionary<string, AggregateInfo> AggregateTypes = new Dictionary<string, AggregateInfo>();
    Dictionary<string, int[]> LookupTables = new Dictionary<string, int[]>();

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
        return converter.Output.Lines;
    }

    void CompileProgram(Expr program)
    {
        OutputStack.Push(new OutputTransaction());
        CurrentScope = new LexicalScope(null);

        Expr[] declarations;
        if (!program.MatchAny(Tag.Sequence, out declarations))
        {
            Program.Panic("The top level of the syntax tree must be a sequence.");
        }

        string name, functionName;
        CType type, returnType;
        MemoryRegion region;
        int[] values;
        Expr value, body;

        // Pass: Process constants and struct/union declarations.
        foreach (Expr decl in declarations)
        {
            FieldInfo[] parsedFields;
            if (decl.Match(Tag.Constant, out type, out name, out value))
            {
                // TODO: Make sure the value fits in the specified type.
                int number = CalculateConstantExpression(value);
                DeclareSymbol(decl, new Symbol(SymbolTag.Constant, number, type, name));
            }
            else if (decl.Match(Tag.Struct, out name, out parsedFields) || decl.Match(Tag.Union, out name, out parsedFields))
            {
                bool union = decl.MatchTag(Tag.Union);

                // Calculate the offset of each field:
                FieldInfo[] fields = new FieldInfo[parsedFields.Length];
                int offset = 0;
                int maxSize = 1;
                for (int i = 0; i < parsedFields.Length; i++)
                {
                    CType fieldType = CalculateConstantArrayDimensions(parsedFields[i].Type);
                    fields[i] = new FieldInfo(fieldType, parsedFields[i].Name, offset);
                    int fieldSize = SizeOf(decl, fieldType);
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
                    CType fieldType = CalculateConstantArrayDimensions(parsedFields[i].Type);
                    fields[i] = new FieldInfo(fieldType, parsedFields[i].Name, offset);
                    int fieldSize = SizeOf(decl, fieldType);
                    offset += fieldSize;
                }

                CFunctionInfo function = new CFunctionInfo
                {
                    Parameters = fields,
                    ReturnType = returnType,
                };
                Functions.Add(functionName, function);
            }
            else if (decl.Match(Tag.Variable, out region, out type, out name))
            {
                DeclareGlobal(decl, region, CalculateConstantArrayDimensions(type), name);
            }
            else if (decl.Match(Tag.ReadonlyData, out type, out name, out values))
            {
                DeclareReadonlyData(decl, type, name, values);
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

                CompileStatement(body);
                ReturnFromFunction();
                EndScope();
            }
        }

        // Pass: Emit all necessary lookup tables.
        foreach (var entry in LookupTables)
        {
            int[] data = entry.Value;
            DeclareReadonlyData(null, CType.MakeArray(CType.UInt8, data.Length), entry.Key, data);
        }

        // Put the interrupt vector table at the end of ROM:
        Emit(Tag.SkipTo, 0xFFFA);
        Emit(Tag.Word, "nmi");
        Emit(Tag.Word, "reset");
        Emit(Tag.Word, "brk");

        if (Output.SpeculationError)
        {
            Program.Panic("root transaction error: {0}", Output.AbortReason);
        }

        Console.WriteLine("Memory usage:");
        foreach (AllocationRegion allocator in new[] { ZeroPageRegion, OamRegion, RamRegion })
        {
            Console.WriteLine("    {0}: {1} bytes free", allocator.Name, allocator.Top - allocator.Next);
        }
    }

    void CompileStatement(Expr expr)
    {
        Expr subexpr, left, right;
        Expr init, test, induct, body;
        Expr[] block, parts;
        CType type;
        string name, mnemonic, fieldName, op;
        AsmOperand operand;

        // Don't print "block" expressions; the subexpressions will be handled individually.
        // Also don't print "empty" expressions.
        if ( !expr.MatchTag(Tag.Sequence) && !expr.MatchTag(Tag.If) && !expr.MatchTag(Tag.For) && !expr.Match(Tag.Empty))
        {
            EmitComment("");
            EmitComment("{0}", ToSourceCode(expr));
        }

        if (expr.Match(Tag.Empty))
        {
            // Nothing!
            return;
        }

        if (expr.MatchAny(Tag.Sequence, out block))
        {
            foreach (Expr stmt in block)
            {
                EmitVerboseComment("STATEMENT: " + stmt.Show());
                CompileStatement(stmt);
            }
            return;
        }

        if (expr.Match(Tag.Variable, out type, out name))
        {
            DeclareLocal(expr, type, name);
            return;
        }

        if (expr.MatchTag(Tag.Label))
        {
            // Pass through:
            Emit(expr);
            return;
        }

        if (expr.Match(Tag.Asm, out mnemonic, out operand))
        {
            // If the operand refers to a constant or variable, replace it with the actual address.
            // (References to readonly data must remain as symbols.)
            AsmOperand fixedOperand = operand;
            Symbol sym;
            if (operand.Base.HasValue &&
                TryFindSymbol(operand.Base.Value, out sym) &&
                sym.Tag != SymbolTag.ReadonlyData)
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
            return;
        }

        if (expr.Match(Tag.For, out init, out test, out induct, out body))
        {
            AsmOperand top = MakeUniqueLabel("for");
            AsmOperand bottom = MakeUniqueLabel("for_break");

            BeginScope();
            CompileStatement(init);
            EmitLabel(top);
            CompileJumpIf(false, test, bottom);
            CompileStatement(body);
            CompileStatement(induct);
            EmitAsm("JMP", top);
            EmitLabel(bottom);
            EndScope();
            return;
        }

        if (expr.MatchAny(Tag.If, out parts))
        {
            AsmOperand endIf = MakeUniqueLabel("end_if");
            for (int i = 0; i < parts.Length; i += 2)
            {
                test = parts[i];
                body = parts[i + 1];
                AsmOperand nextClause = MakeUniqueLabel("next_clause");
                CompileJumpIf(false, test, nextClause);
                CompileStatement(body);
                EmitAsm("JMP", endIf);
                EmitLabel(nextClause);
            }
            EmitLabel(endIf);
            return;
        }

        if (expr.Match(Tag.Assign, out left, out right))
        {
            int leftSize, rightSize;
            bool wide;
            CheckBinaryOperandWidth(left, right, out leftSize, out rightSize, out wide);

            Expr loadExpr, pointerExpr, arrayExpr, indexExpr, structExpr;
            AsmOperand leftOperand, rightOperand, baseAddress, basePointer;
            WideOperand leftWideOperand, rightWideOperand, pointerWideOperand;

            if (!wide && TryGetOperand(left, out leftOperand))
            {
                // Pattern:
                // left = right;
                //
                // LDA right
                // STA left

                Speculate();
                CompileIntoA(right);
                EmitAsm("STA", leftOperand);
                ReleaseA();
                if (Commit()) return;
            }

            if (!wide &&
                left.Match(Tag.Load, out pointerExpr) &&
                TryGetWideOperand(pointerExpr, out pointerWideOperand) &&
                pointerWideOperand.Low.Mode == AddressMode.ZeroPageX)
            {
                // Pattern:
                // *p = right;  (where p is a local)
                //
                // LDA right
                // STA (p,X)

                Speculate();
                CompileIntoA(right);
                EmitAsm("STA", pointerWideOperand.Low.WithMode(AddressMode.IndirectX));
                ReleaseA();
                if (Commit()) return;
            }

            if (!wide && left.Match(Tag.Index, out arrayExpr, out indexExpr))
            {
                // With an indexed expression, there are two subexpressions to compile:
                // the index and the right hand side.
                // Typically you want to compile the more complicated expression first so that
                // you have more registers to work with; therefore, try compiling in both orders and
                // hope that one works.

                if (TryGetConstantBaseAddress(arrayExpr, out baseAddress))
                {
                    // Pattern:
                    // array[index] = right;
                    //
                    // LDY index
                    // LDA right
                    // STA array,Y

                    Speculate();
                    CompileIntoY(indexExpr);
                    CompileIntoA(right);
                    EmitAsm("STA", baseAddress.WithMode(AddressMode.AbsoluteY));
                    ReleaseA();
                    ReleaseY();
                    if (Commit()) return;

                    Speculate();
                    CompileIntoA(right);
                    CompileIntoY(indexExpr);
                    EmitAsm("STA", baseAddress.WithMode(AddressMode.AbsoluteY));
                    ReleaseA();
                    ReleaseY();
                    if (Commit()) return;
                }

                if (TryGetPointerOperand(arrayExpr, out basePointer))
                {
                    // Pattern:
                    // array[index] = right;
                    //
                    // LDY index
                    // LDA right
                    // STA (array),Y

                    Speculate();
                    CompileIntoY(indexExpr);
                    CompileIntoA(right);
                    EmitAsm("STA", basePointer);
                    ReleaseA();
                    ReleaseY();
                    if (Commit()) return;

                    Speculate();
                    CompileIntoA(right);
                    CompileIntoY(indexExpr);
                    EmitAsm("STA", basePointer);
                    ReleaseA();
                    ReleaseY();
                    if (Commit()) return;
                }
            }

            if (!wide &&
                left.Match(Tag.Field, out structExpr, out fieldName) &&
                structExpr.Match(Tag.Load, out pointerExpr) &&
                TryGetPointerOperand(pointerExpr, out basePointer))
            {
                // Pattern:
                // record->field = right;
                //
                // LDY #offsetof(field)
                // LDA right
                // STA (record),Y

                FieldInfo field = GetFieldInfo(structExpr, fieldName);

                Speculate();
                if (field.Offset > 255) Abort("field offset is too large");
                ReserveY();
                EmitAsm("LDY", new AsmOperand(field.Offset, AddressMode.Immediate).WithComment("offset of .{0}", fieldName));
                CompileIntoA(right);
                EmitAsm("STA", basePointer);
                ReleaseA();
                ReleaseY();
                if (Commit()) return;
            }

            if (wide && TryGetWideOperand(left, out leftWideOperand) && TryGetWideOperand(right, out rightWideOperand))
            {
                // Pattern:
                // a = b;
                // (for operands that can be addressed directly)

                if (leftWideOperand.Low.Mode == AddressMode.Immediate)
                {
                    Error(left, "an assignable expression is required");
                }

                ReserveA();
                EmitAsm("LDA", rightWideOperand.Low);
                EmitAsm("STA", leftWideOperand.Low);
                ReleaseA();

                if (leftSize == 2)
                {
                    ReserveA();
                    EmitAsm("LDA", rightWideOperand.High);
                    EmitAsm("STA", leftWideOperand.High);
                    ReleaseA();
                }

                return;
            }

            if (wide &&
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

                ReserveA();
                EmitAsm("LDA", pointerWideOperand.Low);
                EmitAsm("CLC");
                EmitAsm("ADC", new AsmOperand(LowByte(field.Offset), AddressMode.Immediate));
                EmitAsm("STA", leftWideOperand.Low);
                EmitAsm("LDA", pointerWideOperand.High);
                EmitAsm("ADC", new AsmOperand(HighByte(field.Offset), AddressMode.Immediate));
                EmitAsm("STA", leftWideOperand.High);
                ReleaseA();
                return;
            }

            NYI(wide ? "WIDE" : "NARROW");
            return;
        }

        if (expr.Match(Tag.AssignModify, out op, out left, out right))
        {
            int leftSize, rightSize;
            bool wide;
            CheckBinaryOperandWidth(left, right, out leftSize, out rightSize, out wide);

            AsmOperand leftOperand, rightOperand;

            if (!wide && TryGetOperand(left, out leftOperand) && TryGetOperand(right, out rightOperand))
            {
                // Pattern:
                // left @= right;
                //
                // LDA left
                // OP@ right
                // STA left

                Speculate();
                CompileIntoA(left);
                if (op == Tag.Add)
                {
                    EmitAsm("CLC");
                    EmitAsm("ADC", rightOperand);
                }
                else if (op == Tag.Subtract)
                {
                    EmitAsm("SEC");
                    EmitAsm("SBC", rightOperand);
                }
                else
                {
                    Abort("unhandled binary operation");
                }
                EmitAsm("STA", leftOperand);
                ReleaseA();
                if (Commit()) return;
            }

        }

        if (expr.MatchTag(Tag.Call))
        {
            Speculate();
            CompileCall(expr);
            ReleaseA();
            if (Commit()) return;
        }

        if ((expr.Match(Tag.PreIncrement, out subexpr) || expr.Match(Tag.PostIncrement, out subexpr)) &&
            TryGetOperand(subexpr, out operand))
        {
            EmitAsm("INC", operand);
            return;
        }

        if ((expr.Match(Tag.PreDecrement, out subexpr) || expr.Match(Tag.PostDecrement, out subexpr)) &&
            TryGetOperand(subexpr, out operand) &&
            SizeOf(subexpr) == 1)
        {
            EmitAsm("DEC", operand);
            return;
        }

        if (expr.Match(Tag.Return))
        {
            ReturnFromFunction();
            return;
        }

        if (expr.Match(Tag.Return, out subexpr))
        {
            Speculate();
            CompileIntoA(subexpr);
            ReturnFromFunction();
            ReleaseA();
            if (Commit()) return;
        }

        NYI("unhandled expression");
    }

    void CheckBinaryOperandWidth(Expr left, Expr right, out int leftSize, out int rightSize, out bool wide)
    {
        leftSize = SizeOf(left);
        rightSize = SizeOf(right);

        if (leftSize == 0) Error(left, "void expressions cannot be used in assignment");
        if (rightSize == 0) Error(right, "void expressions cannot be used in assignment");
        if (leftSize > 2) Error(left, "type is too large for assignment");
        if (rightSize > 2) Error(right, "type is too large for assignment");

        // Expressions involving values wider than one byte are more difficult to compile.
        wide = leftSize > 1 || rightSize > 1;
    }

    void CompileJumpIf(bool condition, Expr expr, AsmOperand target)
    {
        EmitComment("");
        EmitComment("jump if {0}: {1}", condition.ToString().ToLower(), ToSourceCode(expr));

        AsmOperand operand, leftOperand, rightOperand;
        Expr left, right;
        int number;

        // Jump (or not) unconditionally when the condition is a constant:
        if (TryGetConstant(expr, out number))
        {
            if ((number != 0) == condition)
            {
                EmitAsm("JMP", target);
            }
            return;
        }

        // Jump if nonzero:
        {
            Speculate();
            CompileIntoA(expr);
            string opcode = condition ? "BNE" : "BEQ";
            EmitAsm(opcode, target);
            ReleaseA();
            if (Commit()) return;
        }

        // Jump if less than:
        if (expr.Match(Tag.LessThan, out left, out right) &&
            TryGetOperand(right, out rightOperand))
        {
            // Pattern:
            // if (a < b) ...
            //
            // LDA b
            // CMP a
            // (carry is *clear* if a < b)
            // BCC/BCS target

            Speculate();
            CompileIntoA(left);
            EmitAsm("CMP", rightOperand);
            string opcode = condition ? "BCC" : "BCS";
            EmitAsm(opcode, target);
            ReleaseA();
            if (Commit()) return;
        }

        // (a <= b) === !(b < a)
        if (expr.Match(Tag.LessThanOrEqual, out left, out right))
        {
            CompileJumpIf(condition, Expr.Make(Tag.LessThan, right, left), target);
            return;
        }

        // (a > b) === (b < a)
        if (expr.Match(Tag.GreaterThan, out left, out right))
        {
            CompileJumpIf(condition, Expr.Make(Tag.LessThan, right, left), target);
            return;
        }

        // (a >= b) === !(a < b)
        if (expr.Match(Tag.GreaterThanOrEqual, out left, out right))
        {
            CompileJumpIf(!condition, Expr.Make(Tag.LessThan, left, right), target);
            return;
        }

        // Jump if (a && b):
        if (!condition && expr.Match(Tag.LogicalAnd, out left, out right))
        {
            CompileJumpIf(false, left, target);
            CompileJumpIf(false, right, target);
            return;
        }

        // (a || b) === (!a && !b)

        // Wide comparison:
        // aa < bb === if (ah == bh) then (al < bl); else (ah < bh)
        // aa < bb === (ah < bh) || ((ah == bh) && (al < bl)))

        NYI("unhandled jump expression");
    }

    /// <summary>
    /// Return true if the expression could be calculated and loaded into A.
    /// </summary>
    void CompileIntoA(Expr expr)
    {
        if (SizeOf(expr) != 1) Abort("too large for A");

        Expr left, right, subexpr, structExpr, pointerExpr, indexExpr;
        AsmOperand operand, leftOperand, rightOperand, baseAddress, basePointer;
        int number;
        string fieldName;

        // Simple value:
        if (TryGetOperand(expr, out operand))
        {
            ReserveA();
            EmitAsm("LDA", operand);
            return;
        }

        // Index direct or indirect:
        if (expr.Match(Tag.Index, out pointerExpr, out indexExpr))
        {
            if (TryGetConstantBaseAddress(pointerExpr, out baseAddress))
            {
                CompileIntoY(indexExpr);
                ReserveA();
                EmitAsm("LDA", baseAddress.WithMode(AddressMode.AbsoluteY));
                ReleaseY();
                return;
            }

            if (TryGetPointerOperand(pointerExpr, out basePointer))
            {
                CompileIntoY(indexExpr);
                ReserveA();
                EmitAsm("LDA", basePointer);
                ReleaseY();
                return;
            }
        }

        // Field via pointer to struct:
        if (expr.Match(Tag.Field, out structExpr, out fieldName) &&
            structExpr.Match(Tag.Load, out pointerExpr) &&
            TryGetPointerOperand(pointerExpr, out basePointer))
        {
            FieldInfo field = GetFieldInfo(structExpr, fieldName);
            if (field.Offset > 255) Abort("field offset is too large");
            ReserveY();
            EmitAsm("LDY", new AsmOperand(field.Offset, AddressMode.Immediate).WithComment("offset of .{0}", fieldName));
            ReserveA();
            EmitAsm("LDA", basePointer);
            ReleaseY();
            return;
        }

        // Addition:
        if (expr.Match(Tag.Add, out left, out right))
        {
            // Try both orders, since we can only handle one complex operand.

            if (TryGetOperand(right, out rightOperand))
            {
                CompileIntoA(left);
                EmitAsm("CLC");
                EmitAsm("ADC", rightOperand);
                return;
            }

            if (TryGetOperand(left, out leftOperand))
            {
                CompileIntoA(right);
                EmitAsm("CLC");
                EmitAsm("ADC", leftOperand);
                return;
            }
        }

        // Subtraction:
        if (expr.Match(Tag.Subtract, out left, out right) &&
            TryGetOperand(right, out rightOperand))
        {
            CompileIntoA(left);
            EmitAsm("SEC");
            EmitAsm("SBC", rightOperand);
            return;
        }

        // Bitwise NOT:
        if (expr.Match(Tag.BitwiseNot, out subexpr))
        {
            CompileIntoA(subexpr);
            EmitAsm("EOR", new AsmOperand(0xFF, AddressMode.Immediate));
            return;
        }

        // Bitwise AND:
        if (expr.Match(Tag.BitwiseAnd, out left, out right) &&
            TryGetOperand(right, out rightOperand))
        {
            CompileIntoA(left);
            EmitAsm("AND", rightOperand);
            return;
        }

        // Bitwise OR:
        if (expr.Match(Tag.BitwiseOr, out left, out right) &&
            TryGetOperand(right, out rightOperand))
        {
            CompileIntoA(left);
            EmitAsm("ORA", rightOperand);
            return;
        }

        // Bitwise XOR:
        if (expr.Match(Tag.BitwiseXor, out left, out right) &&
            TryGetOperand(right, out rightOperand))
        {
            CompileIntoA(left);
            EmitAsm("EOR", rightOperand);
            return;
        }

        // Multiplication by a constant:
        if (expr.Match(Tag.Multiply, out left, out right) &&
            TryGetOperand(left, out leftOperand) &&
            TryGetConstant(right, out number))
        {
            ReserveA();
            EmitAsm("LDA", new AsmOperand(0, AddressMode.Immediate));
            EmitAsm("CLC");
            for (int i = 0; i < number; i++)
            {
                EmitAsm("ADC", leftOperand);
            }
            return;
        }

        // Multiplication by a power of two:
        int power;
        if (expr.Match(Tag.Multiply, out left, out right) &&
            TryGetConstant(right, out number) &&
            TryGetPowerOfTwo(number, out power))
        {
            CompileIntoA(left);
            for (int i = 0; i < power; i++)
            {
                EmitAsm("ASL");
            }
            return;
        }

        // Division by a constant:
        if (expr.Match(Tag.Divide, out left, out right) &&
            TryGetConstant(right, out number))
        {
            // By a small power of two:
            if (TryGetPowerOfTwo(number, out power) && power < 4)
            {
                CompileIntoA(left);
                for (int i = 0; i < power; i++)
                {
                    EmitAsm("LSR");
                }
                return;
            }

            // By any number, via lookup table:
            CompileIntoY(left);
            ReserveA();
            EmitAsm("LDA", GenerateDivisionTable(number).WithMode(AddressMode.AbsoluteY));
            ReleaseY();
            return;
        }

        // Call:
        if (expr.MatchTag(Tag.Call))
        {
            CompileCall(expr);
            return;
        }

        Abort("too complex for A");
    }

    /// <summary>
    /// Return true if the expression could be calculated and loaded into Y.
    /// </summary>
    void CompileIntoY(Expr expr)
    {
        if (SizeOf(expr) != 1) Abort("too large for Y");

        Expr left, right;
        AsmOperand operand;
        int number;

        // Simple value:
        if (TryGetOperand(expr, out operand))
        {
            ReserveY();
            EmitAsm("LDY", operand);
            return;
        }

        // Subtract one:
        if (expr.Match(Tag.Subtract, out left, out right) &&
            right.Match(Tag.Integer, out number) &&
            number == 1)
        {
            CompileIntoY(left);
            EmitAsm("DEY");
            return;
        }

        // See if the value can be calculated in A:
        CompileIntoA(expr);
        ReserveY();
        EmitAsm("TAY");
        ReleaseA();
    }

    int CalculateConstantExpression(Expr expr)
    {
        int number;
        if (expr.Match(Tag.Integer, out number))
        {
            return number;
        }

        string name;
        if (expr.Match(Tag.Name, out name))
        {
            Symbol sym = FindSymbol(expr, name);
            if (sym.Tag == SymbolTag.Constant)
            {
                return sym.Value;
            }
        }

        Expr left, right;
        if (expr.Match(Tag.Multiply, out left, out right))
        {
            return CalculateConstantExpression(left) * CalculateConstantExpression(right);
        }

        Error(expr, "a constant expression is required");
        return 0;
    }

    /// <summary>
    /// The result will be stored in A.
    /// </summary>
    void CompileCall(Expr expr)
    {
        Expr functionExpr;
        Expr[] args;
        string function;

        if (expr.MatchAny(Tag.Call, out functionExpr, out args) &&
            functionExpr.Match(Tag.Name, out function))
        {
            CFunctionInfo info;
            if (!Functions.TryGetValue(function, out info)) Error(functionExpr, "function not defined: {0}", function);

            if (args.Length != info.Parameters.Length)
            {
                Error(expr, "wrong number of arguments in function call");
            }

            // Push arguments to negative stack offsets:
            int offset = 256;
            for (int i = 0; i < args.Length; i++)
            {
                FieldInfo param = info.Parameters[i];
                Expr arg = args[i];
                EmitComment("arg {0}: {1}", i, ToSourceCode(arg));
                int paramSize = SizeOf(arg, param.Type);
                offset -= paramSize;
                if (paramSize == 1)
                {
                    CompileIntoA(arg);
                    EmitAsm("STA", new AsmOperand(offset, AddressMode.ZeroPageX));
                    ReleaseA();
                }
                else if (paramSize == 2)
                {
                    WideOperand w;
                    if (TryGetWideOperand(arg, out w))
                    {
                        ReserveA();
                        EmitAsm("LDA", w.Low);
                        EmitAsm("STA", new AsmOperand(offset, AddressMode.ZeroPageX));
                        EmitAsm("LDA", w.High);
                        EmitAsm("STA", new AsmOperand(offset + 1, AddressMode.ZeroPageX));
                        ReleaseA();
                    }
                    else
                    {
                        // TODO: Handle more complex wide arguments.
                        Abort("wide argument is too complex");
                    }
                }
                else
                {
                    Abort("argument is too large");
                }
            }

            // The return value will be stored in A.
            ReserveA();
            EmitAsm("JSR", new AsmOperand(function, AddressMode.Absolute));
            return;
        }

        Abort("call is too complex");
    }

    /// <summary>
    /// If this expression can be represented by a single operand, produce it.
    /// </summary>
    bool TryGetOperand(Expr expr, out AsmOperand operand)
    {
        int size = SizeOf(expr);

        if (size != 1)
        {
            operand = null;
            return false;
        }

        int number;
        if (expr.Match(Tag.Integer, out number))
        {
            if (number > 255) Program.Panic("immediate is too large for one byte");
            operand = new AsmOperand(number, AddressMode.Immediate).WithComment("'literal'");
            return true;
        }

        string name;
        if (expr.Match(Tag.Name, out name))
        {
            Symbol sym = FindSymbol(expr, name);
            if (sym.Tag == SymbolTag.Constant)
            {
                operand = new AsmOperand(LowByte(sym.Value), AddressMode.Immediate).WithComment("#" + sym.Name);
            }
            else if (sym.Tag == SymbolTag.ReadonlyData)
            {
                // Readonly data is always represented by a pointer, which is too large for this purpose.
                Program.Panic("this case should never be reached");
                operand = null;
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

        operand = null;
        return false;
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
                operand.Low = new AsmOperand(LowByte(sym.Value), AddressMode.Immediate).WithComment("#<" + name);
                operand.High = new AsmOperand(HighByte(sym.Value), AddressMode.Immediate).WithComment("#>" + name);
            }
            else if (sym.Tag == SymbolTag.ReadonlyData)
            {
                // No comment is needed for these, since the operand is already a symbol.
                operand.Low = new AsmOperand(name, ImmediateModifier.LowByte);
                operand.High = new AsmOperand(name, ImmediateModifier.HighByte);
            }
            else if (sym.Tag == SymbolTag.Global)
            {
                operand.Low = new AsmOperand(sym.Value, AddressMode.Absolute).WithComment(name);
                operand.High = new AsmOperand(sym.Value + 1, AddressMode.Absolute).WithComment(name + "+1");
            }
            else if (sym.Tag == SymbolTag.Local)
            {
                operand.Low = new AsmOperand(OffsetOfLocal(sym), AddressMode.ZeroPageX).WithComment(name);
                operand.High = new AsmOperand(OffsetOfLocal(sym) + 1, AddressMode.ZeroPageX).WithComment(name + "+1");
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

    /// <summary>
    /// True only if the expression is a compile-time constant base address.
    /// </summary>
    bool TryGetConstantBaseAddress(Expr expr, out AsmOperand baseAddress)
    {
        // Globally allocated arrays have a constant base address:
        CType originalType = TypeOfWithoutDecay(expr);
        WideOperand wideOperand;
        if (TryGetWideOperand(expr, out wideOperand) && originalType.IsArray)
        {
            baseAddress = wideOperand.Low;
            return true;
        }

        baseAddress = null;
        return false;
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

    /// <summary>
    /// True only if the expression is a compile-time constant.
    /// </summary>
    bool TryGetConstant(Expr expr, out int number)
    {
        AsmOperand operand;
        if (TryGetOperand(expr, out operand) && operand.Mode == AddressMode.Immediate)
        {
            number = operand.Offset;
            return true;
        }

        number = 0;
        return false;
    }

    static bool TryGetPowerOfTwo(int number, out int power)
    {
        power = 0;
        if (number <= 0) return false;
        while (true)
        {
            if ((number & 1) == 1)
            {
                // If we've reached the lowest one-bit, and it is the only bit set, then this is a power of two.
                return (number == 1);
            }
            number >>= 1;
            power += 1;
        }
    }

    AsmOperand GenerateDivisionTable(int divisor)
    {
        int[] table = new int[256];
        for (int i = 0; i < 256; i++)
        {
            table[i] = i / divisor;
        }
        string name = "_lookup_divide_by_" + divisor;
        LookupTables[name] = table;
        return new AsmOperand(name, AddressMode.Absolute);
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
        int[] data;
        CType type;
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
        else if (expr.Match(Tag.ReadonlyData, out type, out name, out data))
        {
            return type;
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

    void Speculate()
    {
        OutputStack.Push(new OutputTransaction());
    }

    void Abort(string reason)
    {
        if (OutputStack.Count <= 1) Program.Panic("cannot abort transaction; no speculative output in progress");
        Output.SpeculationError = true;
        Output.AbortReason = reason;
    }

    bool Commit()
    {
        if (Output.ReservedA || Output.ReservedY) Program.Panic("registers were not released");
        bool accept = !Output.SpeculationError;
        OutputTransaction latest = OutputStack.Pop();
        if (OutputStack.Count == 0) Program.Panic("output stack underflowed");
        if (accept) Output.Lines.AddRange(latest.Lines);
        //if (!accept) EmitComment("ABORT: " + latest.AbortReason);
        return accept;
    }

    void ReserveA()
    {
        if (OutputStack.Any(x => x.ReservedA)) Abort("register A is already in use");
        Output.ReservedA = true;
    }

    void ReserveY()
    {
        if (OutputStack.Any(x => x.ReservedY)) Abort("register Y is already in use");
        Output.ReservedY = true;
    }

    void ReleaseA()
    {
        // Don't report errors if a speculation error has occurred, since reserve/release pairs will be unbalanced.
        if (!Output.SpeculationError && !Output.ReservedA) Program.Panic("register A is not reserved in the current transaction");
        Output.ReservedA = false;
    }

    void ReleaseY()
    {
        if (!Output.SpeculationError && !Output.ReservedY) Program.Panic("register Y is not reserved in the current transaction");
        Output.ReservedY = false;
    }

    void Emit(params object[] args)
    {
        Emit(Expr.Make(args));
    }

    void Emit(Expr e)
    {
        Output.Lines.Add(e);
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
    void NYI(string message)
    {
        EmitComment("NYI: {0}", message);
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

        type = CalculateConstantArrayDimensions(type);
        return DeclareSymbol(origin, new Symbol(SymbolTag.Global, address, type, name));
    }

    void DeclareReadonlyData(Expr origin, CType type, string name, int[] values)
    {
        if (type.IsArray)
        {
            type = CalculateConstantArrayDimensions(type);

            // If the array size is unspecified, automatically make it match the number of provided values.
            if (type.Dimension == 0)
            {
                type = CType.MakeArray(type.Subtype, values.Length);
            }

            if (values.Length > type.Dimension)
            {
                Error(
                    origin,
                    "declared size of array ({0}) is too small for the number of specified values ({1})",
                    type.Dimension, values.Length);
            }
        }
        else if (values.Length != 1)
        {
            Program.Panic(origin.Source, "non-array initializers must contain exactly one value");
        }

        // Convert the data to raw bytes:
        byte[] bytes = new byte[SizeOf(origin, type)];
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

        DeclareSymbol(origin, new Symbol(SymbolTag.ReadonlyData, 0, type, name));
        Emit(Tag.ReadonlyData, name, bytes);
    }

    /// <summary>
    /// Convert array dimension expressions to constant integers.
    /// </summary>
    CType CalculateConstantArrayDimensions(CType type)
    {
        if (type.Tag == CTypeTag.ArrayWithDimensionExpression)
        {
            int dimension;
            if (type.DimensionExpression.Match(Tag.Empty))
            {
                // If no dimension is specified, it is implied by the assigned elements.
                dimension = 0;
            }
            else
            {
                dimension = CalculateConstantExpression(type.DimensionExpression);
            }
            type = CType.MakeArray(type.Subtype, dimension);
        }
        return type;
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
        if (r.Type.Tag == CTypeTag.ArrayWithDimensionExpression)
        {
            Program.Panic("array dimensions must be calculated before this point");
        }

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

    /// <summary>
    /// Convert an AST expression back into C source code.
    /// </summary>
    static string ToSourceCode(Expr expr)
    {
        if (expr.Match(Tag.Empty)) return "// empty";

        int number;
        if (expr.Match(Tag.Integer, out number))
        {
            if (number < 256) return number.ToString();
            else return string.Format("0x{0:X}", number);
        }

        string name;
        if (expr.Match(Tag.Name, out name))
        {
            return name;
        }

        string mnemonic;
        AsmOperand operand;
        if (expr.Match(Tag.Asm, out mnemonic, out operand))
        {
            return string.Format("__asm {0} {1}", mnemonic, operand.Show());
        }

        if (expr.MatchTag(Tag.ReadonlyData))
        {
            return "<readonly_data>";
        }

        Expr subexpr, left, right;
        string tag, op;

        if (expr.MatchAnyTag(out tag, out subexpr) && PrefixOperators.TryGetValue(tag, out op))
        {
            return string.Format("{0}({1})", op, ToSourceCode(subexpr));
        }

        if (expr.MatchAnyTag(out tag, out subexpr) && PostfixOperators.TryGetValue(tag, out op))
        {
            return string.Format("({1}){0}", op, ToSourceCode(subexpr));
        }

        if (expr.MatchAnyTag(out tag, out left, out right) && BinaryOperators.TryGetValue(tag, out op))
        {
            return string.Format("({1}) {0} ({2})", op, ToSourceCode(left), ToSourceCode(right));
        }

        if (expr.Match(Tag.AssignModify, out tag, out left, out right) && BinaryOperators.TryGetValue(tag, out op))
        {
            return string.Format("({1}) {0}= ({2})", op, ToSourceCode(left), ToSourceCode(right));
        }

        Expr test;
        if (expr.Match(Tag.Conditional, out test, out left, out right))
        {
            return string.Format("({0}) ? ({1}) : ({2})", ToSourceCode(test), ToSourceCode(left), ToSourceCode(right));
        }

        Expr function;
        Expr[] args;
        if (expr.MatchAny(Tag.Call, out function, out args))
        {
            return string.Format("{0}({1})", ToSourceCode(function), string.Join(", ", args.Select(ToSourceCode)));
        }

        Expr arrayExpr, indexExpr;
        if (expr.Match(Tag.Index, out arrayExpr, out indexExpr))
        {
            return string.Format("({0})[{1}]", ToSourceCode(arrayExpr), ToSourceCode(indexExpr));
        }

        Expr pointerExpr, structExpr;
        string fieldName;
        if (expr.Match(Tag.Field, out structExpr, out fieldName) && structExpr.Match(Tag.Load, out pointerExpr))
        {
            return string.Format("({0})->{1}", ToSourceCode(pointerExpr), fieldName);
        }

        if (expr.Match(Tag.Field, out structExpr, out fieldName))
        {
            return string.Format("({0}).{1}", ToSourceCode(structExpr), fieldName);
        }

        if (expr.Match(Tag.Return))
        {
            return "return;";
        }

        if (expr.Match(Tag.Return, out subexpr))
        {
            return string.Format("return {0};", ToSourceCode(subexpr));
        }

        CType type;
        if (expr.Match(Tag.Variable, out type, out name))
        {
            return string.Format("{0} {1};", type.Show(), name);
        }

        if (expr.Match(Tag.Break)) return "break;";

        if (expr.Match(Tag.Continue)) return "continue;";

        if (expr.Match(Tag.Label, out name)) return name + ":";

        if (expr.Match(Tag.Jump, out name)) return "goto " + name + ";";

        return "???";
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

    static readonly Dictionary<string, string> PrefixOperators = new Dictionary<string, string>
    {
        { Tag.BitwiseNot, "~" },
        { Tag.LogicalNot, "!" },
        { Tag.PreIncrement, "++" },
        { Tag.PreDecrement, "--" },
        { Tag.Load, "*" },
        { Tag.AddressOf, "&" },
    };

    static readonly Dictionary<string, string> PostfixOperators = new Dictionary<string, string>
    {
        { Tag.PostIncrement, "++" },
        { Tag.PostDecrement, "--" },
    };

    static readonly Dictionary<string, string> BinaryOperators = new Dictionary<string, string>
    {
        { Tag.Assign, "=" },
        { Tag.Add, "+" },
        { Tag.Subtract, "-" },
        { Tag.Multiply, "*" },
        { Tag.Divide, "/" },
        { Tag.Modulus, "%" },
        { Tag.Equal, "==" },
        { Tag.NotEqual, "!=" },
        { Tag.LessThan, "<" },
        { Tag.GreaterThan, ">" },
        { Tag.LessThanOrEqual, "<=" },
        { Tag.GreaterThanOrEqual, ">=" },
        { Tag.BitwiseAnd, "&" },
        { Tag.BitwiseOr, "|" },
        { Tag.BitwiseXor, "^" },
        { Tag.LogicalAnd, "&&" },
        { Tag.LogicalOr, "||" },
        { Tag.ShiftLeft, "<<" },
        { Tag.ShiftRight, ">>" },
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
    ReadonlyData,
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

class OutputTransaction
{
    public List<Expr> Lines = new List<Expr>();
    public bool SpeculationError;
    public string AbortReason = "";
    public bool ReservedA, ReservedY;
}
