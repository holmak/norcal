using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

class CodeGenerator
{
    List<Expr> Input;
    List<Expr> Output = new List<Expr>();
    Dictionary<string, CFunctionInfo> Functions = new Dictionary<string, CFunctionInfo>();
    Dictionary<string, CStructInfo> Structs = new Dictionary<string, CStructInfo>();
    Dictionary<string, Symbol> Symbols = new Dictionary<string, Symbol>();
    List<OperandInfo> Stack = new List<OperandInfo>();
    long StackEpoch = 100;
    string NameOfCurrentFunction = null;
    CType ReturnTypeOfCurrentFunction = null;
    int NextTemporary = 0;

    static readonly string TempPointer = "$ptr";

    /// <summary>
    /// Convert stack machine code to 6502 assembly code.
    /// </summary>
    public static List<Expr> Convert(IReadOnlyList<Expr> input)
    {
        CodeGenerator converter = new CodeGenerator();
        converter.Input = input.ToList();
        converter.Run();
        return converter.Output;
    }

    void Run()
    {
        // Process struct declarations first so we know how much space to allocate for variables.
        foreach (Expr op in Input)
        {
            string name;
            FieldInfo[] fields;
            if (op.Match(Tag.Struct, out name, out fields))
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

                Structs.Add(name, new CStructInfo
                {
                    TotalSize = offset,
                    Fields = structFields,
                });
            }
        }

        // Process all other declarations.
        foreach (Expr op in Input)
        {
            string name, functionName;
            MemoryRegion region;
            CType type, returnType;
            FieldInfo[] fields;
            int number;
            int[] values;
            if (op.Match(Tag.Function, out returnType, out functionName, out fields))
            {
                if (Functions.ContainsKey(functionName)) Error("function is already defined: " + functionName);

                // Allocate a global variable for each parameter:
                foreach (FieldInfo field in fields)
                {
                    string qualifiedName = string.Format("{0}{1}{2}", functionName, Program.NamespaceSeparator, field.Name);
                    DeclareSymbol(SymbolTag.Variable, qualifiedName, field.Type, 0);
                    Emit(Tag.Variable, field.Region, SizeOf(field.Type), qualifiedName);
                }

                CFunctionInfo function = new CFunctionInfo
                {
                    Parameters = fields.Select(x => new CParameter(x.Type, x.Name)).ToArray(),
                    ReturnType = returnType,
                };
                Functions.Add(functionName, function);
            }
            else if (op.Match(Tag.Constant, out type, out name, out number))
            {
                // TODO: Make sure the value fits in the specified type.
                DeclareSymbol(SymbolTag.Constant, name, type, number);
                Emit(Tag.Constant, name, number);
            }
            else if (op.Match(Tag.Variable, out region, out type, out name))
            {
                if (IsVariableGlobal(name))
                {
                    DeclareVariable(region, type, name);
                }
            }
            else if (op.Match(Tag.ReadonlyData, out type, out name, out values))
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
                        Error("Declared size of array ({0}) is too small for the number of specified values ({1}).",
                            type.Dimension, values.Length);
                    }
                }
                else if (values.Length != 1)
                {
                    Program.Panic("Non-array initializers must contain exactly one value.");
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

                DeclareSymbol(SymbolTag.Variable, name, type, 0);
                Emit(Tag.ReadonlyData, name, bytes);
            }
        }

        Emit(Tag.Variable, MemoryRegion.ZeroPage, SizeOf(CType.UInt8Ptr), TempPointer);

        while (Input.Count > 0)
        {
            string name, functionName, target;
            MemoryRegion region;
            CType type, newType, returnType;
            FieldInfo[] fields;
            int number, argCount;
            if (Next().Match(Tag.NoOperation))
            {
                ConsumeInput(1);

                // This does nothing.
            }
            else if (Next().Match(Tag.Function, out returnType, out functionName, out fields))
            {
                ConsumeInput(1);

                Emit(Tag.Function, functionName);
                NameOfCurrentFunction = functionName;
                ReturnTypeOfCurrentFunction = returnType;
            }
            else if (Next().MatchTag(Tag.Constant) || Next().MatchTag(Tag.ReadonlyData) || Next().MatchTag(Tag.Struct))
            {
                ConsumeInput(1);

                // All the necessary work was handled in the declaration pre-passes.
            }
            else if (Next().Match(Tag.Variable, out region, out type, out name))
            {
                ConsumeInput(1);

                if (!IsVariableGlobal(name))
                {
                    DeclareVariable(region, type, name);
                }
            }
            else if (Next().Match(Tag.PushImmediate, out number))
            {
                ConsumeInput(1);

                Push(OperandInfo.MakeImmediate(number, (number <= byte.MaxValue) ? CType.UInt8 : CType.UInt16));
            }
            else if (Next().Match(Tag.PushVariableAddress, out name))
            {
                ConsumeInput(1);

                Symbol sym;
                if (!Symbols.TryGetValue(name, out sym))
                {
                    Error("reference to undefined symbol: {0}", name);
                }
                CType valueType = sym.Type;

                if (valueType.IsArray)
                {
                    // Arrays are treated like a pointer to their first element:
                    valueType = valueType.Subtype;

                    // Arrays can only be used as rvalues.
                    // (In VSM terms, this means that every array operand must be "loaded".)
                    if (Next().Match(Tag.Load))
                    {
                        ConsumeInput(1);
                    }
                    else
                    {
                        Error("arrays cannot be used as the target of an assignment");
                    }
                }

                Push(OperandInfo.MakeVariableAddress(name, CType.MakePointer(valueType)));
            }
            else if (Next().Match(Tag.DropFinal))
            {
                ConsumeInput(1);

                if (Stack.Count != 1) Program.Panic("the virtual stack should contain exactly one operand");
                Stack.Clear();
                Emit(Expr.Make(Tag.Comment, "drop final"));
            }
            else if (Next().Match(Tag.Drop))
            {
                ConsumeInput(1);

                Drop(1);
            }
            else if (Next().Match(Tag.Materialize))
            {
                ConsumeInput(1);

                // Put the top operand in the accumulator and update the stack to reflect its new location.
                OperandReference top = Peek(0);
                LoadAccumulator(top);
                Replace(top, OperandInfo.MakeRegister(OperandRegister.Accumulator, top.Type));
            }
            else if (Next().Match(Tag.Cast, out newType))
            {
                ConsumeInput(1);

                OperandReference r = Peek(0);
                if (SizeOf(r.Type) != SizeOf(newType))
                {
                    // TODO: Perform any necessary type conversions.
                    Program.Panic("a type conversion is required");
                }

                Replace(r, r.WithType(newType));
            }
            else if (Next().Match(Tag.Jump, out target))
            {
                ConsumeInput(1);

                EmitAsm("JMP", new AsmOperand(target, AddressMode.Absolute));
            }
            else if (Next().Match(Tag.JumpIfTrue, out target))
            {
                ConsumeInput(1);

                OperandReference cond = Peek(0);

                int size = SizeOf(cond.Type);

                Spill(cond);
                UnloadAccumulator();
                if (size >= 1) EmitAsm("LDA", cond.LowByte());
                if (size >= 2) EmitAsm("ORA", cond.HighByte());
                if (size > 2) Program.Panic("value is too large");
                EmitAsm("BEQ", new AsmOperand(3, AddressMode.Relative));
                EmitAsm("JMP", new AsmOperand(target, AddressMode.Absolute));

                Drop(1);
            }
            else if (Next().Match(Tag.JumpIfFalse, out target))
            {
                ConsumeInput(1);

                OperandReference cond = Peek(0);

                int size = SizeOf(cond.Type);

                Spill(cond);
                UnloadAccumulator();
                if (size >= 1) EmitAsm("LDA", cond.LowByte());
                if (size >= 2) EmitAsm("ORA", cond.HighByte());
                if (size > 2) Program.Panic("value is too large");
                EmitAsm("BNE", new AsmOperand(3, AddressMode.Relative));
                EmitAsm("JMP", new AsmOperand(target, AddressMode.Absolute));

                Drop(1);
            }
            else if (Next().Match(Tag.AddressOf))
            {
                ConsumeInput(1);

                Error("it is not possible to take the address of this expression");
            }
            else if (Next().Match(Tag.Store))
            {
                ConsumeInput(1);

                // Get operands:
                OperandReference value = Peek(0);
                OperandReference address = Peek(1);

                // Check types:
                CType typeToStore = DereferencePointerType(address.Type);
                ConvertType(Conversion.Implicit, value, typeToStore);
                int size = SizeOf(typeToStore);

                // Generate code:

                // First, copy the address to zero page:
                if (address.Tag == OperandTag.Immediate)
                {
                    LoadAccumulator(value);
                    if (size >= 1) Emit(Expr.MakeAsm("STA", new AsmOperand(address.Value, AddressMode.Absolute)));
                    if (size >= 2) Emit(Expr.MakeAsm("STX", new AsmOperand(address.Value + 1, AddressMode.Absolute)));
                    if (size > 2) Program.Panic("values larger than two bytes cannot be stored via accumulator");
                }
                else if (address.Tag == OperandTag.VariableAddress)
                {
                    LoadAccumulator(value);
                    if (size >= 1) Emit(Expr.MakeAsm("STA", new AsmOperand(address.Name, AddressMode.Absolute)));
                    if (size >= 2) Emit(Expr.MakeAsm("STX", new AsmOperand(address.Name, 1, AddressMode.Absolute)));
                    if (size > 2) Program.Panic("values larger than two bytes cannot be stored via accumulator");
                }
                else if (address.Tag == OperandTag.Variable || address.Tag == OperandTag.Register)
                {
                    Store(address, TempPointer);

                    // Now copy the value:
                    LoadAccumulator(value);

                    if (size >= 1)
                    {
                        Emit(Expr.MakeAsm("LDY", new AsmOperand(0, AddressMode.Immediate)));
                        Emit(Expr.MakeAsm("STA", new AsmOperand(TempPointer, AddressMode.IndirectY)));
                    }

                    if (size >= 2)
                    {
                        EmitAsm("INY");
                        // The store operation must leave the stored value in the accumulator, so save and restore A:
                        EmitAsm("PHA");
                        EmitAsm("TXA");
                        Emit(Expr.MakeAsm("STA", new AsmOperand(TempPointer, AddressMode.IndirectY)));
                        EmitAsm("PLA");
                    }

                    if (size > 2) Program.Panic("values larger than two bytes cannot be stored via accumulator");
                }
                else
                {
                    Program.UnhandledCase();
                }

                // The store operation leaves the value in the accumulator; this matches the behavior of C assignment.
                Drop(2);
                PushAccumulator(typeToStore);
            }
            else if (Next(0).Match(Tag.Load) && Next(1).Match(Tag.AddressOf))
            {
                // An "address of" cancels out a preceding "load".
                ConsumeInput(2);
            }
            else if (Next().Match(Tag.Load) || Next().Match(Tag.LoadNondestructive))
            {
                bool drop = !Next().Match(Tag.LoadNondestructive);
                ConsumeInput(1);

                // Get operands:
                OperandReference address = Peek(0);

                // Check types:
                CType loadedType = DereferencePointerType(address.Type);
                int size = SizeOf(loadedType);

                if (address.Tag == OperandTag.Immediate)
                {
                    Program.Panic("the load operation should never be applied to an immediate");
                }
                else if (address.Tag == OperandTag.Variable || address.IsAccumulator)
                {
                    // Copy the pointer to zero page so that it can be used:
                    Store(address, TempPointer);

                    UnloadAccumulator();
                    if (size == 1)
                    {
                        Emit(Expr.MakeAsm("LDY", new AsmOperand(0, AddressMode.Immediate)));
                        Emit(Expr.MakeAsm("LDA", new AsmOperand(TempPointer, AddressMode.IndirectY)));
                    }
                    else if (size == 2)
                    {
                        Emit(Expr.MakeAsm("LDY", new AsmOperand(1, AddressMode.Immediate)));
                        Emit(Expr.MakeAsm("LDA", new AsmOperand(TempPointer, AddressMode.IndirectY)));
                        EmitAsm("TAX");
                        EmitAsm("DEY");
                        Emit(Expr.MakeAsm("LDA", new AsmOperand(TempPointer, AddressMode.IndirectY)));
                    }
                    else
                    {
                        Program.Panic("values larger than two bytes cannot be loaded via accumulator");
                    }

                    if (drop) Drop(1);
                    PushAccumulator(loadedType);
                }
                else if (address.Tag == OperandTag.VariableAddress)
                {
                    OperandInfo result = OperandInfo.MakeVariable(address.Name, loadedType);
                    if (drop) Drop(1);
                    Push(result);
                }
                else
                {
                    Program.NYI();
                }
            }
            else if (Next().Match(Tag.Field, out name))
            {
                ConsumeInput(1);

                // Get operands:
                OperandReference structAddress = Peek(0);

                // Check types:
                CType structType = DereferencePointerType(structAddress.Type);
                if (!structType.IsStruct)
                {
                    Error("expression must be a struct");
                }
                CStructInfo structInfo = GetStructInfo(structType.Name);

                CField fieldInfo = structInfo.Fields.FirstOrDefault(x => x.Name == name);
                if (fieldInfo == null)
                {
                    Error("struct type '{0}' does not contain a field named '{1}'", structType.Name, name);
                }

                // Replace with simpler stack code:
                Input.InsertRange(0, new[]
                {
                    Expr.Make(Tag.Cast, CType.UInt8Ptr),
                    Expr.Make(Tag.PushImmediate, fieldInfo.Offset),
                    Expr.Make(Tag.Add),
                    Expr.Make(Tag.Cast, CType.MakePointer(fieldInfo.Type)),
                });
            }
            else if (Next().Match(Tag.PreIncrement) || Next().Match(Tag.PreDecrement))
            {
                bool increment = Next().Match(Tag.PreIncrement);
                ConsumeInput(1);

                OperandReference address = Peek(0);
                CType valueType = DereferencePointerType(address.Type);
                int size = SizeOf(valueType);

                // Pointer arithmetic must be scaled by element size:
                int amount = 1;
                if (valueType.IsPointer)
                {
                    amount = SizeOf(DereferencePointerType(valueType));
                }

                if (address.Tag == OperandTag.VariableAddress && amount == 1 && size == 1)
                {
                    EmitAsm(increment ? "INC" : "DEC", new AsmOperand(address.Name, AddressMode.Absolute));
                    UnloadAccumulator();
                    EmitAsm("LDA", new AsmOperand(address.Name, AddressMode.Absolute));

                    Drop(1);
                    PushAccumulator(valueType);
                }
                else
                {
                    CType effectiveType = CType.UInt8;
                    if (size == 2) effectiveType = CType.UInt16;
                    if (size > 2) Program.Panic("value is too large");

                    ConvertType(Conversion.Explicit, address, CType.MakePointer(effectiveType));
                    Push(OperandInfo.MakeImmediate(amount, effectiveType));
                    EmitCall(GetRuntimeFunctionName(increment ? "preinc" : "predec", effectiveType), 2);
                    ConvertType(Conversion.Explicit, Peek(0), valueType);
                }
            }
            else if (Next().Match(Tag.PostIncrement) || Next().Match(Tag.PostDecrement))
            {
                bool increment = Next().Match(Tag.PostIncrement);
                ConsumeInput(1);

                OperandReference address = Peek(0);
                CType valueType = DereferencePointerType(address.Type);
                int size = SizeOf(valueType);

                // Pointer arithmetic must be scaled by element size:
                int amount = 1;
                if (valueType.IsPointer)
                {
                    amount = SizeOf(DereferencePointerType(valueType));
                }

                if (address.Tag == OperandTag.VariableAddress && amount == 1 && size == 1)
                {
                    UnloadAccumulator();
                    EmitAsm("LDA", new AsmOperand(address.Name, AddressMode.Absolute));
                    EmitAsm(increment ? "INC" : "DEC", new AsmOperand(address.Name, AddressMode.Absolute));

                    Drop(1);
                    PushAccumulator(valueType);
                }
                else
                {
                    CType effectiveType = CType.UInt8;
                    if (size == 2) effectiveType = CType.UInt16;
                    if (size > 2) Program.Panic("value is too large");

                    ConvertType(Conversion.Explicit, address, CType.MakePointer(effectiveType));
                    Push(OperandInfo.MakeImmediate(amount, effectiveType));
                    EmitCall(GetRuntimeFunctionName(increment ? "postinc" : "postdec", effectiveType), 2);
                    ConvertType(Conversion.Explicit, Peek(0), valueType);
                }
            }
            else if (Next().Match(Tag.Add))
            {
                ConsumeInput(1);

                // Get operands:
                OperandReference right = Peek(0);
                OperandReference left = Peek(1);

                // Check types:
                CType resultType;
                // TODO: Find a better way to scale the index by the element size.
                int repetitions = 1;
                if (left.Type.IsPointer && right.Type.IsPointer)
                {
                    Error("pointers cannot be added together");
                }
                if (left.Type.IsPointer && right.Type.IsInteger)
                {
                    resultType = left.Type;
                    repetitions = SizeOf(DereferencePointerType(resultType));
                    ConvertType(Conversion.Implicit, right, CType.UInt16);
                }
                else if (left.Type.IsInteger && right.Type.IsPointer)
                {
                    // Swap the roles of the operands:
                    OperandReference temp = left;
                    left = right;
                    right = temp;

                    resultType = left.Type;
                    repetitions = SizeOf(DereferencePointerType(resultType));
                    ConvertType(Conversion.Implicit, right, CType.UInt16);
                }
                else
                {
                    resultType = FindCommonType(left.Type, right.Type);
                    ConvertType(Conversion.Implicit, right, resultType);
                    ConvertType(Conversion.Implicit, left, resultType);
                }

                // Generate code:
                int size = SizeOf(resultType);
                LoadAccumulator(left);
                for (int i = 0; i < repetitions; i++)
                {
                    if (size >= 1)
                    {
                        EmitAsm("CLC");
                        EmitAsm("ADC", right.LowByte());
                    }

                    if (size >= 2)
                    {
                        EmitAsm("TAY");
                        EmitAsm("TXA");
                        EmitAsm("ADC", right.HighByte());
                        EmitAsm("TAX");
                        EmitAsm("TYA");
                    }

                    if (size > 2) Program.Panic("value is too large");
                }

                Drop(2);
                PushAccumulator(resultType);
            }
            else if (Next().Match(Tag.Subtract))
            {
                ConsumeInput(1);

                // Get operands:
                OperandReference right = Peek(0);
                OperandReference left = Peek(1);

                // Check types:
                CType resultType;
                // TODO: Find a better way to scale the index by the element size.
                int repetitions = 1;
                if (left.Type.IsPointer && right.Type.IsPointer)
                {
                    resultType = CType.UInt16;
                    Program.NYI();
                }
                if (left.Type.IsPointer && right.Type.IsInteger)
                {
                    resultType = left.Type;
                    repetitions = SizeOf(DereferencePointerType(resultType));
                    ConvertType(Conversion.Implicit, right, CType.UInt16);
                }
                else if (left.Type.IsInteger && right.Type.IsPointer)
                {
                    Error("cannot subtract a pointer from an integer");
                    resultType = left.Type;
                }
                else
                {
                    resultType = FindCommonType(left.Type, right.Type);
                    ConvertType(Conversion.Implicit, right, resultType);
                    ConvertType(Conversion.Implicit, left, resultType);
                }

                // Generate code:
                int size = SizeOf(resultType);
                LoadAccumulator(left);

                if (size >= 1)
                {
                    EmitAsm("SEC");
                    EmitAsm("SBC", right.LowByte());
                }

                if (size >= 2)
                {
                    EmitAsm("TAY");
                    EmitAsm("TXA");
                    EmitAsm("SBC", right.HighByte());
                    EmitAsm("TAX");
                    EmitAsm("TYA");
                }

                if (size > 2) Program.Panic("value is too large");

                Drop(2);
                PushAccumulator(resultType);
            }
            else if (Next().Match(out name) && UnaryRuntimeOperators.TryGetValue(name, out functionName))
            {
                ConsumeInput(1);

                // Get operand:
                CType argType = Peek(0).Type;

                // Check types and generate code:
                if (argType.IsInteger)
                {
                    EmitCall(GetRuntimeFunctionName(functionName, argType), 1);
                }
                else if (argType.IsPointer)
                {
                    Program.NYI();
                }
                else
                {
                    Error("unary operator cannot be used with type '{0}'", argType.Show());
                }
            }
            else if (Next().Match(out name) && BinaryRuntimeOperators.TryGetValue(name, out functionName))
            {
                ConsumeInput(1);

                bool pointersAllowed = BinaryOperatorsThatAllowPointers.Contains(name);

                // Get operands:
                OperandReference right = Peek(0);
                OperandReference left = Peek(1);

                // Check types:
                CType resultType = CType.Void;
                if (left.Type.IsInteger && right.Type.IsInteger)
                {
                    // All of these operators work on integers.
                    resultType = FindCommonType(left.Type, right.Type);
                    ConvertType(Conversion.Implicit, right, resultType);
                    ConvertType(Conversion.Implicit, left, resultType);
                }
                else if (left.Type.IsPointer || right.Type.IsPointer)
                {
                    if (!pointersAllowed)
                    {
                        Error("operation does not support pointers");
                    }
                    else if (left.Type != right.Type)
                    {
                        Error("these pointer operands must have the same type");
                    }
                    else
                    {
                        // Treat the pointer operands as unsigned integers of the same size.
                        resultType = CType.UInt16;
                        ConvertType(Conversion.Explicit, left, resultType);
                        ConvertType(Conversion.Explicit, right, resultType);
                    }
                }
                else
                {
                    // TODO: Make this error message more specific.
                    Error("operation does not allow these types");
                }

                // Generate code:
                EmitCall(GetRuntimeFunctionName(functionName, resultType), 2);
            }
            else if (Next().Match(Tag.Return))
            {
                ConsumeInput(1);

                OperandReference top = Peek(0);

                ConvertType(Conversion.Implicit, top, ReturnTypeOfCurrentFunction);
                LoadAccumulator(top);
                Drop(1);
                EmitAsm("RTS");
            }
            else if (Next().Match(Tag.ReturnImplicitly))
            {
                ConsumeInput(1);

                // TODO: Return statement analysis will make this fallback code unnecessary for non-void functions.
                // Functions that return non-void should never reach this point.
                EmitAsm("RTS");
            }
            else if (Next().Match(Tag.Call, out functionName, out argCount))
            {
                ConsumeInput(1);

                // This is a general-purpose call.
                EmitCall(functionName, argCount);
            }
            else if (Next().MatchTag(Tag.Asm) || Next().MatchTag(Tag.Label) || Next().MatchTag(Tag.Comment))
            {
                Expr op = Next();
                ConsumeInput(1);

                // Pass certain instructions through unchanged.
                Emit(op);
            }
            else
            {
                Program.UnhandledCase();
            }
        }

        // Put the interrupt vector table at the end of ROM:
        Emit(Tag.SkipTo, 0xFFFA);
        Emit(Tag.Word, "nmi");
        Emit(Tag.Word, "reset");
        Emit(Tag.Word, "brk");
    }

    Expr Next() => Next(0);

    /// <summary>
    /// Return the Nth item in the input queue.
    /// </summary>
    Expr Next(int offset)
    {
        return (offset < Input.Count) ? Input[offset] : Expr.Make(Tag.NoOperation);
    }

    void ConsumeInput(int count)
    {
        count = Math.Min(count, Input.Count);
        Input.RemoveRange(0, count);
    }

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

    void DeclareVariable(MemoryRegion region, CType type, string name)
    {
        DeclareSymbol(SymbolTag.Variable, name, type, 0);
        // Replace the type information with just a size:
        Emit(Tag.Variable, region, SizeOf(type), name);
    }

    void DeclareSymbol(SymbolTag tag, string name, CType type, int value)
    {
        // It is an error to define two things with the same name in the same scope.
        if (Symbols.ContainsKey(name))
        {
            Error("symbols cannot be redefined: {0}", name);
        }

        Symbols.Add(name, new Symbol
        {
            Tag = tag,
            Name = name,
            Type = type,
            Value = value,
        });
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

    void ConvertType(Conversion kind, OperandReference r, CType desiredType)
    {
        CType originalType = r.Type;

        if (originalType != desiredType)
        {
            Emit(Tag.Comment, string.Format("convert type from '{0}' to '{1}':", originalType.Show(), desiredType.Show()));
        }

        bool narrowed = false;

        if (originalType == desiredType)
        {
            // NOP
        }
        else if (originalType == CType.UInt8 && desiredType == CType.UInt16)
        {
            if (r.Tag == OperandTag.Immediate)
            {
                // The type and value of immediates can be changed directly:
                Replace(r, OperandInfo.MakeImmediate(r.Value, desiredType));
            }
            else
            {
                // Zero-extend the value:
                LoadAccumulator(r);
                EmitAsm("LDX", new AsmOperand(0, AddressMode.Immediate));
                Replace(r, OperandInfo.MakeRegister(OperandRegister.Accumulator, desiredType));
            }
        }
        else if (originalType == CType.UInt16 && desiredType == CType.UInt8)
        {
            if (r.Tag == OperandTag.Immediate)
            {
                narrowed = r.Value > 0xFF;
                Replace(r, OperandInfo.MakeImmediate(r.Value & 0xFF, desiredType));
            }
            else
            {
                narrowed = true;
                // The actual type doesn't need to be modified; just change the operand's type to exclude the high byte.
                Replace(r, r.WithType(desiredType));
            }
        }
        else if (kind == Conversion.Explicit && SizeOf(originalType) == SizeOf(desiredType))
        {
            Replace(r, r.WithType(desiredType));
        }
        else
        {
            Error("expected type '{1}' does not match actual type '{0}'", originalType.Show(), desiredType.Show());
        }

        // The value was truncated, so display a warning.
        if (narrowed)
        {
            Warning("implicit conversion from '{0}' to '{1}' may lose information", originalType.Show(), desiredType.Show());
        }
    }

    CType DereferencePointerType(CType pointer)
    {
        if (!pointer.IsPointer) Error("a pointer type is required");
        return pointer.Subtype;
    }

    CStructInfo GetStructInfo(string name)
    {
        CStructInfo info;
        if (!Structs.TryGetValue(name, out info)) Error("struct not defined: {0}", name);
        return info;
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
        else if (type.IsStruct)
        {
            CStructInfo info = GetStructInfo(type.Name);
            return info.TotalSize;
        }
        else if (type.IsArray)
        {
            return type.Dimension * SizeOf(type.Subtype);
        }

        Program.NYI();
        return 1;
    }

    string DeclareTemporary(CType type)
    {
        string name = NameOfCurrentFunction + ":$" + NextTemporary;
        NextTemporary += 1;
        DeclareSymbol(SymbolTag.Variable, name, type, 0);
        Emit(Expr.Make(Tag.Variable, MemoryRegion.Ram, SizeOf(type), name));
        return name;
    }

    void Push(OperandInfo r)
    {
        Stack.Add(r);
        StackWasResized();
    }

    void PushAccumulator(CType type)
    {
        Push(OperandInfo.MakeRegister(OperandRegister.Accumulator, type));
    }

    void Drop(int count)
    {
        Stack.RemoveRange(Stack.Count - count, count);
        StackWasResized();
    }

    OperandReference Peek(int offset)
    {
        if (offset < 0 || offset >= Stack.Count) throw new IndexOutOfRangeException("Operand offset is out of range.");
        return new OperandReference(this, Stack.Count - 1 - offset, StackEpoch);
    }

    public OperandInfo GetOperandInfo(OperandReference r) => GetOperandInfo(r.Index, r.StackEpoch);

    public OperandInfo GetOperandInfo(int index, long epoch)
    {
        CheckStackEpoch(epoch);
        return Stack[index];
    }

    void Replace(OperandReference original, OperandInfo replacement)
    {
        CheckStackEpoch(original.StackEpoch);
        Stack[original.Index] = replacement;
    }

    void StackWasResized()
    {
        // Invalidate any existing OperandReferences.
        StackEpoch += 1;
    }

    void CheckStackEpoch(long submission)
    {
        if (submission != StackEpoch) Program.Panic("This operand reference is out of date; the stack has changed.");
    }

    void Spill(OperandReference r)
    {
        if (r.Tag == OperandTag.Register)
        {
            string temp = DeclareTemporary(r.Type);
            Store(r, temp);
            Replace(r, OperandInfo.MakeVariable(temp, r.Type));
        }
    }

    /// <summary>
    /// Spill all registers so that they can be used for something else.
    /// </summary>
    void UnloadAccumulator()
    {
        for (int i = 0; i < Stack.Count; i++)
        {
            OperandReference r = new OperandReference(this, i, StackEpoch);
            Spill(r);
        }
    }

    void LoadAccumulator(OperandReference source)
    {
        CheckStackEpoch(source.StackEpoch);

        // If any other operands are in registers, spill them to memory and remember their new location.
        for (int i = 0; i < Stack.Count; i++)
        {
            if (i != source.Index)
            {
                OperandReference r = new OperandReference(this, i, StackEpoch);
                Spill(r);
            }
        }

        if (source.Tag == OperandTag.Register)
        {
            if (source.Register == OperandRegister.Accumulator)
            {
                // NOP
            }
            else
            {
                Program.NYI();
            }
        }
        else
        {
            int size = SizeOf(source.Type);
            if (size >= 1) EmitAsm("LDA", source.LowByte());
            if (size >= 2) EmitAsm("LDX", source.HighByte());
            if (size > 2) Program.Panic("value is too large");
        }
    }

    /// <summary>
    /// Store an operand on the stack into memory.
    /// </summary>
    void Store(OperandReference source, string destination)
    {
        LoadAccumulator(source);
        int size = SizeOf(source.Type);
        if (size >= 1) EmitAsm("STA", new AsmOperand(destination, 0, AddressMode.Absolute));
        if (size >= 2) EmitAsm("STX", new AsmOperand(destination, 1, AddressMode.Absolute));
        if (size > 2) Program.Panic("value is too large");
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
            OperandReference arg = Peek(i);
            CParameter param = function.Parameters[function.Parameters.Length - 1 - i];
            ConvertType(Conversion.Implicit, arg, param.Type);
            string paramName = functionName + Program.NamespaceSeparator + param.Name;
            Store(arg, paramName);
        }

        EmitAsm("JSR", new AsmOperand(functionName, AddressMode.Absolute));

        // Use the return value:
        Drop(argCount);
        PushAccumulator(function.ReturnType);
    }

    static bool IsVariableGlobal(string name)
    {
        return !name.Contains(Program.NamespaceSeparator);
    }

    static void Warning(string format, params object[] args)
    {
        Program.Warning("warning: " + format, args);
    }

    static void Error(string format, params object[] args)
    {
        Program.Error("error: " + format, args);
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

    [DebuggerDisplay("{Tag} {Name} = 0x{Value,h} ({Type.Show(),nq})")]
    class Symbol
    {
        public SymbolTag Tag;
        public string Name;
        public CType Type;
        public int Value;
    }

    enum SymbolTag
    {
        Constant,
        Variable,
    }
}

class OperandReference
{
    private readonly CodeGenerator Generator;
    public readonly int Index;
    public readonly long StackEpoch;

    public OperandReference(CodeGenerator generator, int index, long epoch)
    {
        Generator = generator;
        Index = index;
        StackEpoch = epoch;
    }

    public override string ToString() => Get().Show();

    OperandInfo Get() => Generator.GetOperandInfo(Index, StackEpoch);

    public OperandTag Tag => Get().Tag;
    public int Value => Get().Value;
    public string Name => Get().Name;
    public OperandRegister Register => Get().Register;
    public CType Type => Get().Type;

    public bool IsAccumulator => Tag == OperandTag.Register && Register == OperandRegister.Accumulator;
    public OperandInfo WithType(CType newType) => new OperandInfo(Tag, Value, Name, Register, newType);

    public AsmOperand LowByte()
    {
        if (Tag == OperandTag.Immediate) return new AsmOperand(Value & 0xFF, AddressMode.Immediate);
        else if (Tag == OperandTag.Variable) return new AsmOperand(Name, AddressMode.Absolute);
        else if (Tag == OperandTag.VariableAddress) return new AsmOperand(Name, ImmediateModifier.LowByte);
        else
        {
            Program.Panic("not possible with register operands");
            return null;
        }
    }

    public AsmOperand HighByte()
    {
        if (Tag == OperandTag.Immediate) return new AsmOperand((Value >> 8) & 0xFF, AddressMode.Immediate);
        else if (Tag == OperandTag.Variable) return new AsmOperand(Name, 1, AddressMode.Absolute);
        else if (Tag == OperandTag.VariableAddress) return new AsmOperand(Name, ImmediateModifier.HighByte);
        else
        {
            Program.Panic("not possible with register operands");
            return null;
        }
    }
}

[DebuggerDisplay("{Show(),nq}")]
class OperandInfo
{
    public readonly OperandTag Tag;
    public readonly int Value;
    public readonly string Name;
    public readonly OperandRegister Register;
    public readonly CType Type;

    public OperandInfo(OperandTag tag, int value, string name, OperandRegister register, CType type)
    {
        Tag = tag;
        Value = value;
        Name = name;
        Register = register;
        Type = type;
    }

    public static OperandInfo MakeImmediate(int value, CType type) => new OperandInfo(OperandTag.Immediate, value, null, OperandRegister.Invalid, type);
    public static OperandInfo MakeVariable(string name, CType type) => new OperandInfo(OperandTag.Variable, 0, name, OperandRegister.Invalid, type);
    public static OperandInfo MakeVariableAddress(string name, CType type) => new OperandInfo(OperandTag.VariableAddress, 0, name, OperandRegister.Invalid, type);
    public static OperandInfo MakeRegister(OperandRegister register, CType type) => new OperandInfo(OperandTag.Register, 0, null, register, type);

    public string Show()
    {
        if (Tag == OperandTag.Immediate) return "#" + Value;
        else if (Tag == OperandTag.Variable) return Name;
        else if (Tag == OperandTag.VariableAddress) return "&" + Name;
        else if (Tag == OperandTag.Register)
        {
            if (Register == OperandRegister.Accumulator) return "A";
            else if (Register == OperandRegister.FlagZero) return "ZF";
            else if (Register == OperandRegister.FlagNotZero) return "!ZF";
            else if (Register == OperandRegister.FlagCarry) return "CF";
            else if (Register == OperandRegister.FlagNotCarry) return "!CF";
        }

        Program.NYI();
        return null;
    }
}

enum OperandTag
{
    Immediate,
    Variable,
    VariableAddress,
    Register,
}

enum OperandRegister
{
    Invalid = 0,
    Accumulator,
    FlagZero,
    FlagNotZero,
    FlagCarry,
    FlagNotCarry,
}

enum Conversion
{
    Implicit,
    Explicit,
}
