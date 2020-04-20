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
    Operand TempPointer;
    Dictionary<string, CFunctionInfo> Functions = new Dictionary<string, CFunctionInfo>();
    Dictionary<string, AggregateInfo> AggregateTypes = new Dictionary<string, AggregateInfo>();
    string NameOfCurrentFunction = null;
    CType ReturnTypeOfCurrentFunction = null;
    int FrameSize = 0;
    LexicalScope CurrentScope;
    LoopScope Loop;

    public static List<Expr> Compile(Expr program)
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
                DeclareSymbol(new Operand(AddressMode.Absolute, number, type, name));
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

                DeclareSymbol(new Operand(AddressMode.Absolute, 0, type, name));
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

                NameOfCurrentFunction = name;
                ReturnTypeOfCurrentFunction = returnType;
                FrameSize = 0;

                foreach (FieldInfo field in fields)
                {
                    DeclareLocal(field.Type, field.Name);
                }

                Expr[] statements;
                if (!body.MatchAny(Tag.Sequence, out statements))
                {
                    Program.Panic("The body of a function must be a sequence.");
                }

                foreach (Expr statement in statements)
                {
                    CompileExpression(statement, Operand.Discard);
                }

                // TODO: Emit the stack cleanup code.
                EmitAsm("RTS");

                EndScope();
            }
        }

        // Put the interrupt vector table at the end of ROM:
        Emit(Tag.SkipTo, 0xFFFA);
        Emit(Tag.Word, "nmi");
        Emit(Tag.Word, "reset");
        Emit(Tag.Word, "brk");
    }

    // TODO: Figure out how to thread continuation info through the compiler.
    void CompileExpression(Expr expr, Operand dest)
    {
        CompileExpression(expr, dest, Continuation.Fallthrough, Continuation.Fallthrough);
    }

    void CompileExpression(Expr expr, Operand dest, Continuation whenTrue, Continuation whenFalse)
    {
        CType type;
        int number;
        string name, op;
        Expr left, right, subexpr;
        Expr[] block;
        if (expr.Match(Tag.Integer, out number))
        {
            // TODO: Convert the type appropriately.

            int size = SizeOf(dest.Type);
            if (size > 2) Program.Panic("value is too large");

            EmitAsm("LDA", new AsmOperand(LowByte(number), AddressMode.Immediate));

            // TODO

        }
        else if (expr.Match(Tag.Name, out name))
        {
            Operand sym = FindSymbol(name);

            // TODO: Convert type if necessary.

            int size = SizeOf(dest.Type);
            if (size > 2) Program.Panic("value is too large");

            // TODO
        }
        else if (expr.Match(Tag.Assign, out left, out right))
        {
            // TODO: Check types.

            Operand address = CompileAddressOf(left);
            Operand value = CompileExpressionToOperand(right);
            int size = SizeOf(value.Type);

            EmitAsm("LDA", LowByte(address));
            EmitAsm("STA", LowByte(TempPointer));
            EmitAsm("LDA", HighByte(address));
            EmitAsm("STA", HighByte(TempPointer));
            EmitAsm("LDY", new AsmOperand(0, AddressMode.Immediate));
            EmitAsm("LDA", LowByte(value));
            EmitAsm("STA", new AsmOperand(TempPointer.Offset, AddressMode.IndirectY));

            if (size == 2)
            {
                EmitAsm("INY");
                EmitAsm("LDA", HighByte(value));
                EmitAsm("STA", new AsmOperand(TempPointer.Offset, AddressMode.IndirectY));
            }

            if (size > 2) Error("cannot handle data larger than two bytes");
        }
        else if (expr.MatchAny(Tag.Sequence, out block))
        {
            foreach (Expr stmt in block)
            {
                CompileExpression(stmt, Operand.Discard);
            }
        }
        else if (expr.Match(Tag.Variable, out type, out name))
        {
            DeclareLocal(type, name);
        }
        else if (expr.Match(Tag.AssignModify, out op, out left, out right))
        {
            Emit(Tag.Comment, "NYI: assign-modify");
        }
        else if (expr.Match(Tag.Return, out subexpr))
        {
            Emit(Tag.Comment, "NYI: return");
        }
        else if (expr.MatchTag(Tag.Asm) || expr.MatchTag(Tag.Label))
        {
            // Pass assembly through unchanged:
            Emit(expr);
        }
        else if (expr.Match(Tag.Add, out left, out right))
        {
            Operand a = CompileExpressionToOperand(left);
            Operand b = CompileExpressionToOperand(right);
        }
        else
        {
            Emit(Tag.Comment, "NYI: expression: " + expr.Show());
        }
    }

    Operand CompileExpressionToOperand(Expr expr)
    {
        int number;
        string name;
        if (expr.Match(Tag.Integer, out number))
        {
            return new Operand(AddressMode.Immediate, number, number < 256 ? CType.UInt8 : CType.UInt16);
        }
        else if (expr.Match(Tag.Name, out name))
        {
            return FindSymbol(name);
        }
        else
        {
            Operand temp = DeclareTemporary(TypeOf(expr));
            CompileExpression(expr, temp);
            return temp;
        }
    }

    Operand CompileAddressOf(Expr expr)
    {
        string name;
        Expr subexpr;
        if (expr.Match(Tag.Name, out name))
        {
            Operand sym = FindSymbol(name);
            return new Operand(AddressMode.Immediate, sym.Offset, CType.MakePointer(sym.Type));
        }
        else if (expr.Match(Tag.Load, out subexpr))
        {
            return CompileAddressOf(subexpr);
        }
        else
        {
            Program.NYI();
            return null;
        }
    }

    AsmOperand LowByte(Operand r)
    {
        if (r.Tag == AddressMode.Immediate)
        {
            return new AsmOperand(LowByte(r.Offset), AddressMode.Immediate);
        }
        else if (r.Tag == AddressMode.Absolute)
        {
            return new AsmOperand(r.Offset, AddressMode.Absolute);
        }
        else if (r.Tag == AddressMode.ZeroPageX)
        {
            return new AsmOperand(0, AddressMode.Absolute);
        }
        else
        {
            Program.UnhandledCase();
            return null;
        }
    }

    AsmOperand HighByte(Operand r)
    {
        if (r.Tag == AddressMode.Immediate)
        {
            return new AsmOperand(HighByte(r.Offset), AddressMode.Immediate);
        }
        else if (r.Tag == AddressMode.Absolute)
        {
            return new AsmOperand(r.Offset + 1, AddressMode.Absolute);
        }
        else if (r.Tag == AddressMode.ZeroPageX)
        {
            return new AsmOperand(1, AddressMode.ZeroPageX);
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
        if (expr.Match(Tag.Name, out name))
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

    //        else if (Next().Match(Tag.PushVariableAddress, out name))
    //        {
    //            ConsumeInput(1);

    //            Symbol sym;
    //            if (!Symbols.TryGetValue(name, out sym))
    //            {
    //                Error("reference to undefined symbol: {0}", name);
    //            }
    //            CType valueType = sym.Type;

    //            if (sym.Tag == SymbolTag.Variable)
    //            {
    //                Push(OperandInfo.MakeVariableAddress(name, CType.MakePointer(valueType)));
    //            }
    //            else if (sym.Tag == SymbolTag.Constant)
    //            {
    //                Push(OperandInfo.MakeImmediate(sym.Value, sym.Type));

    //                // Ignore the subsequent load op:
    //                if (Next().Match(Tag.Load))
    //                {
    //                    ConsumeInput(1);
    //                }
    //                else
    //                {
    //                    // Variable references should always be followed by a load.
    //                    Program.UnhandledCase();
    //                }
    //            }
    //            else
    //            {
    //                Program.UnhandledCase();
    //            }
    //        }
    //        else if (Next().Match(Tag.DropFinal))
    //        {
    //            ConsumeInput(1);

    //            if (Stack.Count != 1) Program.Panic("the virtual stack should contain exactly one operand");
    //            Stack.Clear();
    //            Emit(Expr.Make(Tag.Comment, "drop final"));
    //        }
    //        else if (Next().Match(Tag.Drop))
    //        {
    //            ConsumeInput(1);

    //            Drop(1);
    //        }
    //        else if (Next().Match(Tag.Materialize))
    //        {
    //            ConsumeInput(1);

    //            // Put the top operand in the accumulator and update the stack to reflect its new location.
    //            OperandReference top = Peek(0);
    //            LoadAccumulator(top);
    //            Replace(top, OperandInfo.MakeRegister(OperandRegister.Accumulator, top.Type));
    //        }
    //        else if (Next().Match(Tag.Cast, out newType))
    //        {
    //            ConsumeInput(1);

    //            OperandReference r = Peek(0);
    //            if (SizeOf(r.Type) != SizeOf(newType))
    //            {
    //                // TODO: Perform any necessary type conversions.
    //                Program.Panic("a type conversion is required");
    //            }

    //            Replace(r, r.WithType(newType));
    //        }
    //        else if (Next().Match(Tag.Jump, out target))
    //        {
    //            ConsumeInput(1);

    //            EmitAsm("JMP", new AsmOperand(target, AddressMode.Absolute));
    //        }
    //        else if (Next().Match(Tag.JumpIfTrue, out target))
    //        {
    //            ConsumeInput(1);

    //            OperandReference cond = Peek(0);

    //            int size = SizeOf(cond.Type);

    //            Spill(cond);
    //            UnloadAccumulator();
    //            if (size >= 1) EmitAsm("LDA", cond.LowByte());
    //            if (size >= 2) EmitAsm("ORA", cond.HighByte());
    //            if (size > 2) Program.Panic("value is too large");
    //            EmitAsm("BEQ", new AsmOperand(3, AddressMode.Relative));
    //            EmitAsm("JMP", new AsmOperand(target, AddressMode.Absolute));

    //            Drop(1);
    //        }
    //        else if (Next().Match(Tag.JumpIfFalse, out target))
    //        {
    //            ConsumeInput(1);

    //            OperandReference cond = Peek(0);

    //            int size = SizeOf(cond.Type);

    //            Spill(cond);
    //            UnloadAccumulator();
    //            if (size >= 1) EmitAsm("LDA", cond.LowByte());
    //            if (size >= 2) EmitAsm("ORA", cond.HighByte());
    //            if (size > 2) Program.Panic("value is too large");
    //            EmitAsm("BNE", new AsmOperand(3, AddressMode.Relative));
    //            EmitAsm("JMP", new AsmOperand(target, AddressMode.Absolute));

    //            Drop(1);
    //        }
    //        else if (Next().Match(Tag.AddressOf))
    //        {
    //            ConsumeInput(1);

    //            Error("it is not possible to take the address of this expression");
    //        }
    //        else if (Next().Match(Tag.Store))
    //        {
    //            ConsumeInput(1);

    //            // Get operands:
    //            OperandReference value = Peek(0);
    //            OperandReference address = Peek(1);

    //            // Check types:
    //            CType typeToStore = DereferencePointerType(address.Type);
    //            ConvertType(Conversion.Implicit, value, typeToStore);
    //            int size = SizeOf(typeToStore);

    //            // Generate code:

    //            // First, copy the address to zero page:
    //            if (address.Tag == OperandTag.Immediate)
    //            {
    //                LoadAccumulator(value);
    //                if (size >= 1) Emit(Expr.MakeAsm("STA", new AsmOperand(address.Value, AddressMode.Absolute)));
    //                if (size >= 2) Emit(Expr.MakeAsm("STX", new AsmOperand(address.Value + 1, AddressMode.Absolute)));
    //                if (size > 2) Program.Panic("values larger than two bytes cannot be stored via accumulator");
    //            }
    //            else if (address.Tag == OperandTag.VariableAddress)
    //            {
    //                LoadAccumulator(value);
    //                if (size >= 1) Emit(Expr.MakeAsm("STA", new AsmOperand(address.Name, AddressMode.Absolute)));
    //                if (size >= 2) Emit(Expr.MakeAsm("STX", new AsmOperand(address.Name, 1, AddressMode.Absolute)));
    //                if (size > 2) Program.Panic("values larger than two bytes cannot be stored via accumulator");
    //            }
    //            else if (address.Tag == OperandTag.Variable || address.Tag == OperandTag.Register)
    //            {
    //                Store(address, TempPointer);

    //                // Now copy the value:
    //                LoadAccumulator(value);

    //                if (size >= 1)
    //                {
    //                    Emit(Expr.MakeAsm("LDY", new AsmOperand(0, AddressMode.Immediate)));
    //                    Emit(Expr.MakeAsm("STA", new AsmOperand(TempPointer, AddressMode.IndirectY)));
    //                }

    //                if (size >= 2)
    //                {
    //                    EmitAsm("INY");
    //                    // The store operation must leave the stored value in the accumulator, so save and restore A:
    //                    EmitAsm("PHA");
    //                    EmitAsm("TXA");
    //                    Emit(Expr.MakeAsm("STA", new AsmOperand(TempPointer, AddressMode.IndirectY)));
    //                    EmitAsm("PLA");
    //                }

    //                if (size > 2) Program.Panic("values larger than two bytes cannot be stored via accumulator");
    //            }
    //            else
    //            {
    //                Program.UnhandledCase();
    //            }

    //            // The store operation leaves the value in the accumulator; this matches the behavior of C assignment.
    //            Drop(2);
    //            PushAccumulator(typeToStore);
    //        }
    //        else if (Next(0).Match(Tag.Load) && Next(1).Match(Tag.AddressOf))
    //        {
    //            // An "address of" cancels out a preceding "load".
    //            ConsumeInput(2);
    //        }
    //        else if (Next().Match(Tag.Load) || Next().Match(Tag.LoadNondestructive))
    //        {
    //            bool drop = !Next().Match(Tag.LoadNondestructive);
    //            ConsumeInput(1);

    //            // Get operands:
    //            OperandReference address = Peek(0);

    //            // Check types:
    //            CType loadedType = DereferencePointerType(address.Type);

    //            if (loadedType.IsArray)
    //            {
    //                // The result of loading an array is a pointer to the first element of the array.
    //                // (This is the automatic "decay" of arrays to pointers.)
    //                ConvertType(Conversion.Explicit, address, CType.MakePointer(loadedType.Subtype));
    //            }
    //            else if (address.Tag == OperandTag.Immediate)
    //            {
    //                Program.Panic("the load operation should never be applied to an immediate");
    //            }
    //            else if (address.Tag == OperandTag.Variable || address.IsAccumulator)
    //            {
    //                int size = SizeOf(loadedType);

    //                // Copy the pointer to zero page so that it can be used:
    //                Store(address, TempPointer);

    //                UnloadAccumulator();
    //                if (size == 1)
    //                {
    //                    Emit(Expr.MakeAsm("LDY", new AsmOperand(0, AddressMode.Immediate)));
    //                    Emit(Expr.MakeAsm("LDA", new AsmOperand(TempPointer, AddressMode.IndirectY)));
    //                }
    //                else if (size == 2)
    //                {
    //                    Emit(Expr.MakeAsm("LDY", new AsmOperand(1, AddressMode.Immediate)));
    //                    Emit(Expr.MakeAsm("LDA", new AsmOperand(TempPointer, AddressMode.IndirectY)));
    //                    EmitAsm("TAX");
    //                    EmitAsm("DEY");
    //                    Emit(Expr.MakeAsm("LDA", new AsmOperand(TempPointer, AddressMode.IndirectY)));
    //                }
    //                else
    //                {
    //                    Program.Panic("values larger than two bytes cannot be loaded via accumulator");
    //                }

    //                if (drop) Drop(1);
    //                PushAccumulator(loadedType);
    //            }
    //            else if (address.Tag == OperandTag.VariableAddress)
    //            {
    //                OperandInfo result = OperandInfo.MakeVariable(address.Name, loadedType);
    //                if (drop) Drop(1);
    //                Push(result);
    //            }
    //            else
    //            {
    //                Program.NYI();
    //            }
    //        }
    //        else if (Next().Match(Tag.Field, out name))
    //        {
    //            ConsumeInput(1);

    //            // Get operands:
    //            OperandReference structAddress = Peek(0);

    //            // Check types:
    //            CType aggregateType = DereferencePointerType(structAddress.Type);
    //            if (!aggregateType.IsStructOrUnion)
    //            {
    //                Error("expression must be a struct or union");
    //            }
    //            CAggregateInfo aggregateInfo = GetAggregateInfo(aggregateType.Name);

    //            CField fieldInfo = aggregateInfo.Fields.FirstOrDefault(x => x.Name == name);
    //            if (fieldInfo == null)
    //            {
    //                Error("struct type '{0}' does not contain a field named '{1}'", aggregateType.Name, name);
    //            }

    //            // Replace with simpler stack code:
    //            Input.InsertRange(0, new[]
    //            {
    //                Expr.Make(Tag.Cast, CType.UInt8Ptr),
    //                Expr.Make(Tag.PushImmediate, fieldInfo.Offset),
    //                Expr.Make(Tag.Add),
    //                Expr.Make(Tag.Cast, CType.MakePointer(fieldInfo.Type)),
    //            });
    //        }
    //        else if (Next().Match(Tag.PreIncrement) || Next().Match(Tag.PreDecrement))
    //        {
    //            bool increment = Next().Match(Tag.PreIncrement);
    //            ConsumeInput(1);

    //            OperandReference address = Peek(0);
    //            CType valueType = DereferencePointerType(address.Type);
    //            int size = SizeOf(valueType);

    //            // Pointer arithmetic must be scaled by element size:
    //            int amount = 1;
    //            if (valueType.IsPointer)
    //            {
    //                amount = SizeOf(DereferencePointerType(valueType));
    //            }

    //            if (address.Tag == OperandTag.VariableAddress && amount == 1 && size == 1)
    //            {
    //                EmitAsm(increment ? "INC" : "DEC", new AsmOperand(address.Name, AddressMode.Absolute));
    //                UnloadAccumulator();
    //                EmitAsm("LDA", new AsmOperand(address.Name, AddressMode.Absolute));

    //                Drop(1);
    //                PushAccumulator(valueType);
    //            }
    //            else
    //            {
    //                CType effectiveType = CType.UInt8;
    //                if (size == 2) effectiveType = CType.UInt16;
    //                if (size > 2) Program.Panic("value is too large");

    //                ConvertType(Conversion.Explicit, address, CType.MakePointer(effectiveType));
    //                Push(OperandInfo.MakeImmediate(amount, effectiveType));
    //                EmitCall(GetRuntimeFunctionName(increment ? "preinc" : "predec", effectiveType), 2);
    //                ConvertType(Conversion.Explicit, Peek(0), valueType);
    //            }
    //        }
    //        else if (Next().Match(Tag.PostIncrement) || Next().Match(Tag.PostDecrement))
    //        {
    //            bool increment = Next().Match(Tag.PostIncrement);
    //            ConsumeInput(1);

    //            OperandReference address = Peek(0);
    //            CType valueType = DereferencePointerType(address.Type);
    //            int size = SizeOf(valueType);

    //            // Pointer arithmetic must be scaled by element size:
    //            int amount = 1;
    //            if (valueType.IsPointer)
    //            {
    //                amount = SizeOf(DereferencePointerType(valueType));
    //            }

    //            if (address.Tag == OperandTag.VariableAddress && amount == 1 && size == 1)
    //            {
    //                UnloadAccumulator();
    //                EmitAsm("LDA", new AsmOperand(address.Name, AddressMode.Absolute));
    //                EmitAsm(increment ? "INC" : "DEC", new AsmOperand(address.Name, AddressMode.Absolute));

    //                Drop(1);
    //                PushAccumulator(valueType);
    //            }
    //            else
    //            {
    //                CType effectiveType = CType.UInt8;
    //                if (size == 2) effectiveType = CType.UInt16;
    //                if (size > 2) Program.Panic("value is too large");

    //                ConvertType(Conversion.Explicit, address, CType.MakePointer(effectiveType));
    //                Push(OperandInfo.MakeImmediate(amount, effectiveType));
    //                EmitCall(GetRuntimeFunctionName(increment ? "postinc" : "postdec", effectiveType), 2);
    //                ConvertType(Conversion.Explicit, Peek(0), valueType);
    //            }
    //        }
    //        else if (Next().Match(Tag.Add))
    //        {
    //            ConsumeInput(1);

    //            // Get operands:
    //            OperandReference right = Peek(0);
    //            OperandReference left = Peek(1);

    //            // Check types:
    //            CType resultType;
    //            // TODO: Find a better way to scale the index by the element size.
    //            int repetitions = 1;
    //            if (left.Type.IsPointer && right.Type.IsPointer)
    //            {
    //                Error("pointers cannot be added together");
    //            }
    //            if (left.Type.IsPointer && right.Type.IsInteger)
    //            {
    //                resultType = left.Type;
    //                repetitions = SizeOf(DereferencePointerType(resultType));
    //                ConvertType(Conversion.Implicit, right, CType.UInt16);
    //            }
    //            else if (left.Type.IsInteger && right.Type.IsPointer)
    //            {
    //                // Swap the roles of the operands:
    //                OperandReference temp = left;
    //                left = right;
    //                right = temp;

    //                resultType = left.Type;
    //                repetitions = SizeOf(DereferencePointerType(resultType));
    //                ConvertType(Conversion.Implicit, right, CType.UInt16);
    //            }
    //            else
    //            {
    //                resultType = FindCommonType(left.Type, right.Type);
    //                ConvertType(Conversion.Implicit, right, resultType);
    //                ConvertType(Conversion.Implicit, left, resultType);
    //            }

    //            // Generate code:
    //            int size = SizeOf(resultType);
    //            LoadAccumulator(left);
    //            for (int i = 0; i < repetitions; i++)
    //            {
    //                if (size >= 1)
    //                {
    //                    EmitAsm("CLC");
    //                    EmitAsm("ADC", right.LowByte());
    //                }

    //                if (size >= 2)
    //                {
    //                    EmitAsm("TAY");
    //                    EmitAsm("TXA");
    //                    EmitAsm("ADC", right.HighByte());
    //                    EmitAsm("TAX");
    //                    EmitAsm("TYA");
    //                }

    //                if (size > 2) Program.Panic("value is too large");
    //            }

    //            Drop(2);
    //            PushAccumulator(resultType);
    //        }
    //        else if (Next().Match(Tag.Subtract))
    //        {
    //            ConsumeInput(1);

    //            // Get operands:
    //            OperandReference right = Peek(0);
    //            OperandReference left = Peek(1);

    //            // Check types:
    //            CType resultType;
    //            // TODO: Find a better way to scale the index by the element size.
    //            int repetitions = 1;
    //            if (left.Type.IsPointer && right.Type.IsPointer)
    //            {
    //                resultType = CType.UInt16;
    //                Program.NYI();
    //            }
    //            if (left.Type.IsPointer && right.Type.IsInteger)
    //            {
    //                resultType = left.Type;
    //                repetitions = SizeOf(DereferencePointerType(resultType));
    //                ConvertType(Conversion.Implicit, right, CType.UInt16);
    //            }
    //            else if (left.Type.IsInteger && right.Type.IsPointer)
    //            {
    //                Error("cannot subtract a pointer from an integer");
    //                resultType = left.Type;
    //            }
    //            else
    //            {
    //                resultType = FindCommonType(left.Type, right.Type);
    //                ConvertType(Conversion.Implicit, right, resultType);
    //                ConvertType(Conversion.Implicit, left, resultType);
    //            }

    //            // Generate code:
    //            int size = SizeOf(resultType);
    //            LoadAccumulator(left);

    //            if (size >= 1)
    //            {
    //                EmitAsm("SEC");
    //                EmitAsm("SBC", right.LowByte());
    //            }

    //            if (size >= 2)
    //            {
    //                EmitAsm("TAY");
    //                EmitAsm("TXA");
    //                EmitAsm("SBC", right.HighByte());
    //                EmitAsm("TAX");
    //                EmitAsm("TYA");
    //            }

    //            if (size > 2) Program.Panic("value is too large");

    //            Drop(2);
    //            PushAccumulator(resultType);
    //        }
    //        else if (Next().Match(out name) && UnaryRuntimeOperators.TryGetValue(name, out functionName))
    //        {
    //            ConsumeInput(1);

    //            // Get operand:
    //            CType argType = Peek(0).Type;

    //            // Check types and generate code:
    //            if (argType.IsInteger)
    //            {
    //                EmitCall(GetRuntimeFunctionName(functionName, argType), 1);
    //            }
    //            else if (argType.IsPointer)
    //            {
    //                Program.NYI();
    //            }
    //            else
    //            {
    //                Error("unary operator cannot be used with type '{0}'", argType.Show());
    //            }
    //        }
    //        else if (Next().Match(out name) && BinaryRuntimeOperators.TryGetValue(name, out functionName))
    //        {
    //            ConsumeInput(1);

    //            bool pointersAllowed = BinaryOperatorsThatAllowPointers.Contains(name);

    //            // Get operands:
    //            OperandReference right = Peek(0);
    //            OperandReference left = Peek(1);

    //            // Check types:
    //            CType resultType = CType.Void;
    //            if (left.Type.IsInteger && right.Type.IsInteger)
    //            {
    //                // All of these operators work on integers.
    //                resultType = FindCommonType(left.Type, right.Type);
    //                ConvertType(Conversion.Implicit, right, resultType);
    //                ConvertType(Conversion.Implicit, left, resultType);
    //            }
    //            else if (left.Type.IsPointer || right.Type.IsPointer)
    //            {
    //                if (!pointersAllowed)
    //                {
    //                    Error("operation does not support pointers");
    //                }
    //                else if (left.Type != right.Type)
    //                {
    //                    Error("these pointer operands must have the same type");
    //                }
    //                else
    //                {
    //                    // Treat the pointer operands as unsigned integers of the same size.
    //                    resultType = CType.UInt16;
    //                    ConvertType(Conversion.Explicit, left, resultType);
    //                    ConvertType(Conversion.Explicit, right, resultType);
    //                }
    //            }
    //            else
    //            {
    //                // TODO: Make this error message more specific.
    //                Error("operation does not allow these types");
    //            }

    //            // Generate code:
    //            EmitCall(GetRuntimeFunctionName(functionName, resultType), 2);
    //        }
    //        else if (Next().Match(Tag.Return))
    //        {
    //            ConsumeInput(1);

    //            OperandReference top = Peek(0);

    //            ConvertType(Conversion.Implicit, top, ReturnTypeOfCurrentFunction);
    //            LoadAccumulator(top);
    //            Drop(1);
    //            EmitAsm("RTS");
    //        }
    //        else if (Next().Match(Tag.ReturnVoid))
    //        {
    //            ConsumeInput(1);

    //            EmitAsm("RTS");
    //        }
    //        else if (Next().Match(Tag.Call, out functionName, out argCount))
    //        {
    //            ConsumeInput(1);

    //            // This is a general-purpose call.
    //            EmitCall(functionName, argCount);
    //        }
    //        else if (Next().MatchTag(Tag.Asm) || Next().MatchTag(Tag.Label) || Next().MatchTag(Tag.Comment))
    //        {
    //            Expr op = Next();
    //            ConsumeInput(1);

    //            // Pass certain instructions through unchanged.
    //            Emit(op);
    //        }
    //        else
    //        {
    //            Program.UnhandledCase();
    //        }
    //    }

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

    void BeginScope()
    {
        CurrentScope = new LexicalScope(CurrentScope);
    }

    void EndScope()
    {
        CurrentScope = CurrentScope.Outer;
    }

    Operand FindSymbol(string name)
    {
        Operand sym;

        for (LexicalScope scope = CurrentScope; scope != null; scope = scope.Outer)
        {
            if (scope.Locals.TryGetValue(name, out sym))
            {
                return sym;
            }
        }

        Error("reference to undefined symbol: {0}", name);
        return null;
    }

    Operand DeclareGlobal(MemoryRegion region, CType type, string name)
    {
        // TODO: Allocate an address from the appropriate region.

        return DeclareSymbol(new Operand(AddressMode.Absolute, 0x80, type, name));
    }

    Operand DeclareLocal(CType type, string name)
    {
        int size = SizeOf(type);
        int offset = FrameSize;
        FrameSize += size;
        return DeclareSymbol(new Operand(AddressMode.ZeroPageX, offset, type, name));
    }

    Operand DeclareTemporary(CType type)
    {
        int i = 0;
        while (true)
        {
            string name = string.Format("$temp{0}", i);
            if (!CurrentScope.Locals.ContainsKey(name))
            {
                return DeclareLocal(type, name);
            }
            i += 1;
        }
    }

    Operand DeclareSymbol(Operand r)
    {
        // It is an error to define two things with the same name in the same scope.
        if (CurrentScope.Locals.ContainsKey(r.Name))
        {
            Error("symbols cannot be redefined: {0}", r.Name);
        }

        CurrentScope.Locals.Add(r.Name, r);
        Emit(Tag.Comment, string.Format("symbol: {0} @ {1}", r.Name, r));
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

class Operand
{
    public readonly AddressMode Tag;
    public readonly int Offset;
    public readonly CType Type;
    public readonly string Name;

    public Operand(AddressMode tag, int offset, CType type)
        : this(tag, offset, type, "<unnamed>")
    {
    }

    public Operand(AddressMode tag, int offset, CType type, string name)
    {
        Tag = tag;
        Offset = offset;
        Type = type;
        Name = name;
    }

    public static readonly Operand Discard = null;

    public override string ToString()
    {
        return string.Format("Location({0}, {1}, {2}, {3})", Tag, Offset, Type.Show(), Name);
    }
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
    public readonly Dictionary<string, Operand> Locals = new Dictionary<string, Operand>();

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
