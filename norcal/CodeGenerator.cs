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
    List<Operand> Stack = new List<Operand>();
    string NameOfCurrentFunction = null;
    int NextTemporary = 0;

    static readonly Operand TempPointer = Operand.MakeVariable("$ptr", CType.UInt8Ptr);

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
                if (Functions.ContainsKey(functionName)) Program.Error("function is already defined: " + functionName);

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
                        Program.Error("Declared size of array ({0}) is too small for the number of specified values ({1}).",
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

        DeclareVariable(MemoryRegion.ZeroPage, TempPointer.Type, TempPointer.Name);

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

                Push(Operand.MakeImmediate(number, (number <= byte.MaxValue) ? CType.UInt8 : CType.UInt16));
            }
            else if (Next().Match(Tag.PushVariableAddress, out name))
            {
                ConsumeInput(1);

                Symbol sym;
                if (!Symbols.TryGetValue(name, out sym))
                {
                    Program.Error("reference to undefined symbol: {0}", name);
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
                        Program.Error("arrays cannot be used as the target of an assignment");
                    }
                }

                Push(Operand.MakeVariableAddress(name, CType.MakePointer(valueType)));
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

                Pop();
            }
            else if (Next().Match(Tag.Duplicate))
            {
                ConsumeInput(1);

                Push(Peek(0));
            }
            else if (Next().Match(Tag.Swap))
            {
                ConsumeInput(1);

                Operand a = Pop();
                Operand b = Pop();
                Push(a);
                Push(b);
            }
            else if (Next().Match(Tag.Over))
            {
                ConsumeInput(1);

                Push(Peek(1));
            }
            else if (Next().Match(Tag.Materialize))
            {
                ConsumeInput(1);

                // Put the top operand in the accumulator.
                Operand top = Pop();
                SpillAll();
                EmitLoadAccumulator(top);
                PushAccumulator(top.Type);
            }
            else if (Next().Match(Tag.Cast, out newType))
            {
                ConsumeInput(1);

                Operand r = Pop();
                if (SizeOf(r.Type) != SizeOf(newType))
                {
                    // TODO: Perform any necessary type conversions.
                    Program.Panic("a type conversion is required");
                }
                Push(r.WithType(newType));
            }
            else if (Next().Match(Tag.Jump, out target))
            {
                ConsumeInput(1);

                EmitAsm("JMP", new AsmOperand(target, AddressMode.Absolute));
            }
            else if (Next().Match(Tag.JumpIfTrue, out target))
            {
                ConsumeInput(1);

                SpillAll();
                Operand cond = Pop();

                int size = SizeOf(cond.Type);

                if (cond.Tag == OperandTag.Register)
                {
                    Program.NYI();
                }
                else
                {
                    if (size >= 1) EmitAsm("LDA", cond.LowByte());
                    if (size >= 2) EmitAsm("ORA", cond.HighByte());
                    if (size > 2) Program.Panic("value is too large");
                    EmitAsm("BEQ", new AsmOperand(3, AddressMode.Relative));
                    EmitAsm("JMP", new AsmOperand(target, AddressMode.Absolute));
                }
            }
            else if (Next().Match(Tag.JumpIfFalse, out target))
            {
                ConsumeInput(1);

                SpillAll();
                Operand cond = Pop();

                int size = SizeOf(cond.Type);

                if (cond.Tag == OperandTag.Register)
                {
                    Program.NYI();
                }
                else
                {
                    if (size >= 1) EmitAsm("LDA", cond.LowByte());
                    if (size >= 2) EmitAsm("ORA", cond.HighByte());
                    if (size > 2) Program.Panic("value is too large");
                    EmitAsm("BNE", new AsmOperand(3, AddressMode.Relative));
                    EmitAsm("JMP", new AsmOperand(target, AddressMode.Absolute));
                }
            }
            else if (Next().Match(Tag.AddressOf))
            {
                ConsumeInput(1);

                Program.Error("it is not possible to take the address of this expression");
            }
            else if (Next().Match(Tag.Store))
            {
                ConsumeInput(1);

                // Get operands:
                Operand value = Pop();
                Operand address = Pop();

                // Check types:
                if (!TryToMatchLeftType(DereferencePointerType(address.Type), ref value))
                {
                    Program.Error("types in assignment don't match");
                }
                int size = SizeOf(value.Type);

                // Rearrange stack:
                SpillAll();
                address = Spill(address);

                // Generate code:

                // First, copy the address to zero page:
                if (address.Tag == OperandTag.Immediate)
                {
                    EmitLoadAccumulator(value);
                    if (size >= 1) Emit(Expr.MakeAsm("STA", new AsmOperand(address.Value, AddressMode.Absolute)));
                    if (size >= 2) Emit(Expr.MakeAsm("STX", new AsmOperand(address.Value + 1, AddressMode.Absolute)));
                    if (size > 2) Program.Panic("values larger than two bytes cannot be stored via accumulator");
                }
                else if (address.Tag == OperandTag.VariableAddress)
                {
                    EmitLoadAccumulator(value);
                    if (size >= 1) Emit(Expr.MakeAsm("STA", new AsmOperand(address.Name, AddressMode.Absolute)));
                    if (size >= 2) Emit(Expr.MakeAsm("STX", new AsmOperand(address.Name, 1, AddressMode.Absolute)));
                    if (size > 2) Program.Panic("values larger than two bytes cannot be stored via accumulator");
                }
                else if (address.Tag == OperandTag.Variable)
                {
                    value = Spill(value);
                    EmitLoadAccumulator(address);
                    EmitStoreAccumulator(TempPointer);

                    // Now copy the value:
                    EmitLoadAccumulator(value);

                    if (size >= 1)
                    {
                        Emit(Expr.MakeAsm("LDY", new AsmOperand(0, AddressMode.Immediate)));
                        Emit(Expr.MakeAsm("STA", new AsmOperand(TempPointer.Name, AddressMode.IndirectY)));
                    }

                    if (size >= 2)
                    {
                        EmitAsm("INY");
                        // The store operation must leave the stored value in the accumulator, so save and restore A:
                        EmitAsm("PHA");
                        EmitAsm("TXA");
                        Emit(Expr.MakeAsm("STA", new AsmOperand(TempPointer.Name, AddressMode.IndirectY)));
                        EmitAsm("PLA");
                    }

                    if (size > 2) Program.Panic("values larger than two bytes cannot be stored via accumulator");
                }
                else
                {
                    Program.UnhandledCase();
                }

                // The store operation leaves the value in the accumulator; this matches the behavior of C assignment.
                PushAccumulator(value.Type);
            }
            else if (Next(0).Match(Tag.Load) && Next(1).Match(Tag.AddressOf))
            {
                // An "address of" cancels out a preceding "load".
                ConsumeInput(2);
            }
            else if (Next().Match(Tag.Load))
            {
                ConsumeInput(1);

                // Get operands:
                Operand address = Pop();

                // Check types:
                CType loadedType = DereferencePointerType(address.Type);
                int size = SizeOf(loadedType);

                if (address.Tag == OperandTag.Immediate)
                {
                    Program.Panic("the load operation should never be applied to an immediate");
                }
                else if (address.Tag == OperandTag.Variable || address.IsAccumulator)
                {
                    // Rearrange stack:
                    SpillAll();
                    if (!address.IsAccumulator) address = Spill(address);

                    // Copy the pointer to zero page so that it can be used:
                    EmitLoadAccumulator(address);
                    EmitStoreAccumulator(TempPointer);

                    if (size == 1)
                    {
                        Emit(Expr.MakeAsm("LDY", new AsmOperand(0, AddressMode.Immediate)));
                        Emit(Expr.MakeAsm("LDA", new AsmOperand(TempPointer.Name, AddressMode.IndirectY)));
                    }
                    else if (size == 2)
                    {
                        Emit(Expr.MakeAsm("LDY", new AsmOperand(1, AddressMode.Immediate)));
                        Emit(Expr.MakeAsm("LDA", new AsmOperand(TempPointer.Name, AddressMode.IndirectY)));
                        EmitAsm("TAX");
                        EmitAsm("DEY");
                        Emit(Expr.MakeAsm("LDA", new AsmOperand(TempPointer.Name, AddressMode.IndirectY)));
                    }
                    else
                    {
                        Program.Panic("values larger than two bytes cannot be loaded via accumulator");
                    }

                    PushAccumulator(loadedType);
                }
                else if (address.Tag == OperandTag.VariableAddress)
                {
                    Push(Operand.MakeVariable(address.Name, loadedType));
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
                Operand structAddress = Peek(0);

                // Check types:
                CType structType = DereferencePointerType(structAddress.Type);
                if (!structType.IsStruct)
                {
                    Program.Error("expression must be a struct");
                }
                CStructInfo structInfo = GetStructInfo(structType.Name);

                CField fieldInfo = structInfo.Fields.FirstOrDefault(x => x.Name == name);
                if (fieldInfo == null)
                {
                    Program.Error("struct type '{0}' does not contain a field named '{1}'", structType.Name, name);
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
            else if (Next().Match(Tag.Add))
            {
                ConsumeInput(1);

                // Get operands:
                Operand right = Pop();
                Operand left = Pop();

                int repetitions;

                // Check types:
                if (left.Type.IsPointer)
                {
                    if (!TryToMatchLeftType(CType.UInt16, ref right)) Program.Error("types don't match");

                    int elementSize = SizeOf(DereferencePointerType(left.Type));
                    // TODO: Find a better way to scale the index by the element size.
                    repetitions = elementSize;
                }
                else
                {
                    if (!TryToMatchTypes(ref left, ref right)) Program.Error("types don't match");
                    repetitions = 1;
                }
                int size = SizeOf(left.Type);

                // Rearrange the stack:
                SpillAll();
                right = Spill(right);
                EmitLoadAccumulator(left);

                // Generate code:
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

                PushAccumulator(left.Type);
            }
            else if (Next().Match(Tag.Subtract))
            {
                ConsumeInput(1);

                // Get operands:
                Operand right = Pop();
                Operand left = Pop();

                // Check types:
                if (!TryToMatchTypes(ref left, ref right)) Program.Error("types don't match");
                int size = SizeOf(left.Type);

                // Rearrange the stack:
                SpillAll();
                right = Spill(right);
                EmitLoadAccumulator(left);

                // Generate code:
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

                PushAccumulator(left.Type);
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
                    Program.Error("unary operator cannot be used with type '{0}'", argType.Show());
                }
            }
            else if (Next().Match(out name) && BinaryRuntimeOperators.TryGetValue(name, out functionName))
            {
                ConsumeInput(1);

                // Get operands:
                Operand right = Peek(0);
                Operand left = Peek(1);

                // Check types:
                if (!left.Type.IsInteger || !right.Type.IsInteger) Program.Error("arithmetic requires integers");
                if (!TryToMatchTypes(ref left, ref right)) Program.Error("types don't match");

                // Generate code:
                EmitCall(GetRuntimeFunctionName(functionName, left.Type), 2);
            }
            else if (Next().Match(Tag.Return))
            {
                ConsumeInput(1);

                Operand result = Pop();
                EmitLoadAccumulator(result);
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
            Program.Error("symbols cannot be redefined: {0}", name);
        }

        Symbols.Add(name, new Symbol
        {
            Tag = tag,
            Name = name,
            Type = type,
            Value = value,
        });
    }

    /// <summary>
    /// Return true if the operand types match.
    /// This function is necessary because the type of immediates has some flexibility.
    /// </summary>
    static bool TryToMatchTypes(ref Operand left, ref Operand right)
    {
        if (left.Type == right.Type)
        {
            return true;
        }
        else if (left.Tag == OperandTag.Immediate || right.Tag == OperandTag.Immediate)
        {
            // Try promoting the immediate values to a larger type:
            if (left.Tag == OperandTag.Immediate && left.Type == CType.UInt8) left = left.WithType(CType.UInt16);
            if (right.Tag == OperandTag.Immediate && right.Type == CType.UInt8) right = right.WithType(CType.UInt16);
            return left.Type == right.Type;
        }
        else
        {
            return false;
        }
    }

    /// <summary>
    /// Return true if the operand types match.
    /// This function is necessary because the type of immediates has some flexibility.
    /// </summary>
    static bool TryToMatchLeftType(CType leftType, ref Operand right)
    {
        if (leftType == right.Type)
        {
            return true;
        }
        else if (right.Tag == OperandTag.Immediate)
        {
            // Try promoting the immediate value to a larger type:
            if (right.Type == CType.UInt8) right = right.WithType(CType.UInt16);
            return leftType == right.Type;
        }
        else
        {
            return false;
        }
    }

    CType DereferencePointerType(CType pointer)
    {
        if (!pointer.IsPointer) Program.Error("a pointer type is required");
        return pointer.Subtype;
    }

    CStructInfo GetStructInfo(string name)
    {
        CStructInfo info;
        if (!Structs.TryGetValue(name, out info)) Program.Error("struct not defined: {0}", name);
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

    void SpillAll()
    {
        Stack = Stack.Select(Spill).ToList();
    }

    /// <summary>
    /// Returns the corresponding new operand.
    /// </summary>
    Operand Spill(Operand r)
    {
        if (r.Tag == OperandTag.Register)
        {
            if (r.Register == OperandRegister.Accumulator)
            {
                string temp = DeclareTemporary(r.Type);
                int size = SizeOf(r.Type);
                if (size >= 1) EmitAsm("STA", new AsmOperand(temp, AddressMode.Absolute));
                if (size >= 2) EmitAsm("STX", new AsmOperand(temp, 1, AddressMode.Absolute));
                if (size > 2) Program.Panic("value is too large");
                return Operand.MakeVariable(temp, r.Type);
            }
            else
            {
                Program.NYI();
                return null;
            }
        }
        else
        {
            return r;
        }
    }

    string DeclareTemporary(CType type)
    {
        string name = NameOfCurrentFunction + ":$" + NextTemporary;
        NextTemporary += 1;
        DeclareSymbol(SymbolTag.Variable, name, type, 0);
        Emit(Expr.Make(Tag.Variable, MemoryRegion.Ram, SizeOf(type), name));
        return name;
    }

    void AssertRegistersFree()
    {
        if (Stack.Any(x => x.Tag == OperandTag.Register))
        {
            Program.Panic("An accumulator or flag operand was overwritten; it should have been saved.");
        }
    }

    void PushAccumulator(CType type)
    {
        AssertRegistersFree();
        Push(Operand.MakeRegister(OperandRegister.Accumulator, type));
    }

    void Push(Operand r)
    {
        Stack.Add(r);
    }

    Operand Pop()
    {
        Operand r = Stack[Stack.Count - 1];
        Stack.RemoveAt(Stack.Count - 1);
        return r;
    }

    Operand Peek(int offset)
    {
        return Stack[Stack.Count - 1 - offset];
    }

    void EmitLoadAccumulator(Operand r)
    {
        AssertRegistersFree();

        int size = SizeOf(r.Type);

        if (r.Tag == OperandTag.Register)
        {
            if (r.Register == OperandRegister.Accumulator)
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
            if (size >= 1) EmitAsm("LDA", r.LowByte());
            if (size >= 2) EmitAsm("LDX", r.HighByte());
            if (size > 2) Program.Panic("value is too large");
        }
    }

    void EmitStoreAccumulator(Operand r)
    {
        int size = SizeOf(r.Type);
        if (size >= 1) EmitAsm("STA", r.LowByte());
        if (size >= 2) EmitAsm("STX", r.HighByte());
        if (size > 2) Program.Panic("value is too large");
    }

    void EmitCall(string functionName, int argCount)
    {
        // Copy the arguments into the function's call frame:
        SpillAll();
        CFunctionInfo function;
        if (!Functions.TryGetValue(functionName, out function))
        {
            Program.Error("function not implemented: {0}", functionName);
        }

        if (argCount != function.Parameters.Length)
        {
            Program.Error("wrong number of arguments in call to '{0}': {1} required; {2} specified",
                functionName, function.Parameters.Length, argCount);
        }

        for (int i = function.Parameters.Length - 1; i >= 0; i--)
        {
            Operand arg = Pop();

            // Check types:
            CParameter param = function.Parameters[i];
            if (!TryToMatchLeftType(param.Type, ref arg))
            {
                Program.Error("in call to function '{0}', argument '{1}' has the wrong type", functionName, param.Name);
            }

            Operand paramOperand = Operand.MakeVariable(
                functionName + Program.NamespaceSeparator + param.Name,
                param.Type);
            EmitLoadAccumulator(arg);
            EmitStoreAccumulator(paramOperand);
        }

        EmitAsm("JSR", new AsmOperand(functionName, AddressMode.Absolute));

        // Use the return value:
        PushAccumulator(function.ReturnType);
    }

    static bool IsVariableGlobal(string name)
    {
        return !name.Contains(Program.NamespaceSeparator);
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

[DebuggerDisplay("{Show(),nq}")]
class Operand
{
    public readonly OperandTag Tag;
    public readonly int Value;
    public readonly string Name;
    public readonly OperandRegister Register;
    public readonly CType Type;

    public Operand(OperandTag tag, int value, string name, OperandRegister register, CType type)
    {
        Tag = tag;
        Value = value;
        Name = name;
        Register = register;
        Type = type;
    }

    public static Operand MakeImmediate(int value, CType type) => new Operand(OperandTag.Immediate, value, null, OperandRegister.Invalid, type);
    public static Operand MakeVariable(string name, CType type) => new Operand(OperandTag.Variable, 0, name, OperandRegister.Invalid, type);
    public static Operand MakeVariableAddress(string name, CType type) => new Operand(OperandTag.VariableAddress, 0, name, OperandRegister.Invalid, type);
    public static Operand MakeRegister(OperandRegister register, CType type) => new Operand(OperandTag.Register, 0, null, register, type);

    public bool IsAccumulator => Tag == OperandTag.Register && Register == OperandRegister.Accumulator;
    public Operand WithType(CType newType) => new Operand(Tag, Value, Name, Register, newType);

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
