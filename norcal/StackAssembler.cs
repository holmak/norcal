using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

class StackAssembler
{
    List<Expr> Output = new List<Expr>();
    Dictionary<string, CFunctionInfo> Functions = new Dictionary<string, CFunctionInfo>();
    Dictionary<string, CStructInfo> Structs = new Dictionary<string, CStructInfo>();
    Dictionary<string, Symbol> Symbols = new Dictionary<string, Symbol>();
    List<Operand> Stack = new List<Operand>();
    string NameOfCurrentFunction = null;
    int NextTemporary = 0;

    /// <summary>
    /// Convert stack machine code to 6502 assembly code.
    /// </summary>
    public static List<Expr> Convert(List<Expr> input)
    {
        StackAssembler converter = new StackAssembler();
        converter.Run(input);
        return converter.Output;
    }

    void Run(List<Expr> input)
    {
        foreach (Expr op in input)
        {
            string name, functionName, target;
            MemoryRegion region;
            CType type, returnType;
            FieldInfo[] fields;
            int number;
            if (op.Match(Tag.Function, out returnType, out functionName, out fields))
            {
                if (Functions.ContainsKey(functionName)) Program.Error("function is already defined: " + functionName);

                Emit(Tag.Function, functionName);
                NameOfCurrentFunction = functionName;

                // Allocate a global variable for each parameter:
                foreach (FieldInfo field in fields)
                {
                    string qualifiedName = string.Format("{0}{1}{2}", functionName, Compiler.NamespaceSeparator, field.Name);
                    DeclareSymbol(SymbolTag.Variable, qualifiedName, field.Type, 0);
                    Emit(Tag.Variable, field.Region, SizeOf(field.Type), qualifiedName);
                }

                CFunctionInfo function = new CFunctionInfo
                {
                    ParameterTypes = fields.Select(x => x.Type).ToArray(),
                    ReturnType = returnType,
                };
                Functions.Add(functionName, function);
            }
            else if (op.Match(Tag.Constant, out type, out name, out number))
            {
                // TODO: Make sure the value fits in the specified type.
                DeclareSymbol(SymbolTag.Constant, name, type, number);
            }
            else if (op.Match(Tag.Variable, out region, out type, out name))
            {
                DeclareSymbol(SymbolTag.Variable, name, type, 0);
                // Replace the type information with just a size:
                Emit(Tag.Variable, region, SizeOf(type), name);
            }
            else if (op.Match(Tag.Struct, out name, out fields))
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
            else if (op.Match(Tag.PushImmediate, out number))
            {
                PushImmediate(number);
            }
            else if (op.Match(Tag.PushVariable, out name))
            {
                PushVariable(name, Symbols[name].Type);
            }
            else if (op.Match(Tag.Jump, out target))
            {
                Emit(Expr.MakeAsm("JMP", new AsmOperand(target, AddressMode.Absolute)));
            }
            else if (op.Match(Tag.JumpIfTrue, out target))
            {
                SpillAll();
                Operand cond = Pop();

                if (cond.Tag == OperandTag.Variable)
                {
                    Emit(Expr.MakeAsm("LDA", new AsmOperand(cond.Name, AddressMode.Absolute)));
                    Emit(Expr.MakeAsm("ORA", new AsmOperand(cond.Name, 1, AddressMode.Absolute)));
                    Emit(Expr.MakeAsm("BNE", new AsmOperand(target, AddressMode.Absolute)));
                }
                else
                {
                    Program.NYI();
                }
            }
            else if (op.Match(Tag.JumpIfFalse, out target))
            {
                SpillAll();
                Operand cond = Pop();

                if (cond.Tag == OperandTag.Variable)
                {
                    Emit(Expr.MakeAsm("LDA", new AsmOperand(cond.Name, AddressMode.Absolute)));
                    Emit(Expr.MakeAsm("ORA", new AsmOperand(cond.Name, 1, AddressMode.Absolute)));
                    Emit(Expr.MakeAsm("BEQ", new AsmOperand(target, AddressMode.Absolute)));
                }
                else
                {
                    Program.NYI();
                }
            }
            else if (op.Match(Tag.AddressOf))
            {
                SpillAll();
                Operand r = Pop();
                if (r.Tag == OperandTag.Variable)
                {
                    PushVariableAddress(r.Name, r.Type);
                }
                else
                {
                    Program.NYI();
                }
            }
            else if (op.Match(Tag.Store))
            {
                // Get operands:
                Operand value = Pop();
                Operand dest = Pop();

                // Check types:
                if (!TryToMakeTypesEqual(ref dest, ref value)) Program.Error("types in assignment don't match");
                int size = SizeOf(value.Type);

                // Rearrange stack:
                SpillAll();
                dest = Spill(dest);
                EmitLoadAccumulator(value);

                // Generate code:
                if (dest.Tag == OperandTag.Variable)
                {
                    if (size >= 1) Emit(Expr.MakeAsm("STA", new AsmOperand(dest.Name, AddressMode.Absolute)));
                    if (size >= 2) Emit(Expr.MakeAsm("STX", new AsmOperand(dest.Name, 1, AddressMode.Absolute)));
                    if (size > 2) Program.Panic("value is too large");
                    PushAccumulator(value.Type);
                }
                else
                {
                    Program.NYI();
                }
            }
            else if (op.Match(Tag.Load))
            {
                SpillAll();
                Operand address = Pop();

                CType loadedType = DereferencePointerType(address.Type);
                int size = SizeOf(loadedType);

                if (address.Tag == OperandTag.Variable)
                {
                    if (size >= 1) Emit(Expr.MakeAsm("LDA", new AsmOperand(address.Name, AddressMode.Absolute)));
                    if (size >= 2) Emit(Expr.MakeAsm("LDX", new AsmOperand(address.Name, 1, AddressMode.Absolute)));
                    if (size > 2) Program.Panic("values larger than two bytes cannot be loaded");
                    PushAccumulator(loadedType);
                }
                else
                {
                    Program.NYI();
                }
            }
            else if (op.Match(Tag.Add))
            {
                // Get operands:
                Operand right = Pop();
                Operand left = Pop();

                // Check types:
                if (!TryToMakeTypesEqual(ref left, ref right)) Program.Error("types don't match");
                int size = SizeOf(left.Type);

                // Rearrange the stack:
                SpillAll();
                right = Spill(right);
                EmitLoadAccumulator(left);

                // Generate code:
                if (right.Tag == OperandTag.Immediate)
                {
                    if (size >= 1)
                    {
                        Emit(Expr.MakeAsm("CLC"));
                        Emit(Expr.MakeAsm("ADC", new AsmOperand(LowByte(right.Value), AddressMode.Immediate)));
                    }

                    if (size >= 2)
                    {
                        Emit(Expr.MakeAsm("TAY"));
                        Emit(Expr.MakeAsm("TXA"));
                        Emit(Expr.MakeAsm("ADC", new AsmOperand(HighByte(right.Value), AddressMode.Immediate)));
                        Emit(Expr.MakeAsm("TAX"));
                        Emit(Expr.MakeAsm("TYA"));
                    }

                    if (size > 2) Program.Panic("value is too large");

                    PushAccumulator(left.Type);
                }
                else if (right.Tag == OperandTag.Variable)
                {
                    if (size >= 1)
                    {
                        Emit(Expr.MakeAsm("CLC"));
                        Emit(Expr.MakeAsm("ADC", new AsmOperand(right.Name, AddressMode.Absolute)));
                    }

                    if (size >= 2)
                    {
                        Emit(Expr.MakeAsm("TAY"));
                        Emit(Expr.MakeAsm("TXA"));
                        Emit(Expr.MakeAsm("ADC", new AsmOperand(right.Name, 1, AddressMode.Absolute)));
                        Emit(Expr.MakeAsm("TAX"));
                        Emit(Expr.MakeAsm("TYA"));
                    }

                    if (size > 2) Program.Panic("value is too large");

                    PushAccumulator(left.Type);
                }
                else
                {
                    Program.NYI();
                }
            }
            else if (op.Match(Tag.Subtract))
            {
                // Get operands:
                Operand right = Pop();
                Operand left = Pop();

                // Check types:
                if (!TryToMakeTypesEqual(ref left, ref right)) Program.Error("types don't match");
                int size = SizeOf(left.Type);

                // Rearrange the stack:
                SpillAll();
                right = Spill(right);
                EmitLoadAccumulator(left);

                // Generate code:
                if (right.Tag == OperandTag.Immediate)
                {
                    if (size >= 1)
                    {
                        Emit(Expr.MakeAsm("SEC"));
                        Emit(Expr.MakeAsm("SBC", new AsmOperand(LowByte(right.Value), AddressMode.Immediate)));
                    }

                    if (size >= 2)
                    {
                        Emit(Expr.MakeAsm("TAY"));
                        Emit(Expr.MakeAsm("TXA"));
                        Emit(Expr.MakeAsm("SBC", new AsmOperand(HighByte(right.Value), AddressMode.Immediate)));
                        Emit(Expr.MakeAsm("TAX"));
                        Emit(Expr.MakeAsm("TYA"));
                    }

                    if (size > 2) Program.Panic("value is too large");

                    PushAccumulator(left.Type);
                }
                else if (right.Tag == OperandTag.Variable)
                {
                    if (size >= 1)
                    {
                        Emit(Expr.MakeAsm("SEC"));
                        Emit(Expr.MakeAsm("SBC", new AsmOperand(right.Name, AddressMode.Absolute)));
                    }

                    if (size >= 2)
                    {
                        Emit(Expr.MakeAsm("TAY"));
                        Emit(Expr.MakeAsm("TXA"));
                        Emit(Expr.MakeAsm("SBC", new AsmOperand(right.Name, 1, AddressMode.Absolute)));
                        Emit(Expr.MakeAsm("TAX"));
                        Emit(Expr.MakeAsm("TYA"));
                    }

                    if (size > 2) Program.Panic("value is too large");

                    PushAccumulator(left.Type);
                }
                else
                {
                    Program.NYI();
                }
            }
            else if (op.Match(Tag.Drop))
            {
                if (Stack.Count != 1) Program.Panic("the virtual stack should contain exactly one operand");
                Stack.Clear();
            }
            else if (op.Match(Tag.Return))
            {
                SpillAll();
                Operand result = Pop();
                EmitLoadAccumulator(result);
                Emit(Expr.MakeAsm("RTS"));
            }
            else if (op.Match(out functionName))
            {
                // This is a general-purpose call.
                // TODO: Copy the arguments into the function's call frame.
                Emit(Expr.MakeAsm("JSR", new AsmOperand(functionName, AddressMode.Absolute)));
            }
            else
            {
                // Pass anything else through unchanged.
                Emit(op);
            }
        }

        // Put the interrupt vector table at the end of ROM:
        Emit(Tag.SkipTo, 0xFFFA);
        Emit(Tag.Word, "nmi");
        Emit(Tag.Word, "reset");
        Emit(Tag.Word, "brk");
    }

    void Emit(params object[] args)
    {
        Emit(Expr.Make(args));
    }

    void Emit(Expr e)
    {
        Output.Add(e);
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
    /// Return true if the types match, and produce the type that they share.
    /// This function is necessary because the type of immediates has some flexibility.
    /// </summary>
    static bool TryToMakeTypesEqual(ref Operand left, ref Operand right)
    {
        if (left.Type == right.Type)
        {
            return true;
        }
        else if (left.Tag == OperandTag.Immediate || right.Tag == OperandTag.Immediate)
        {
            // Try promoting the immediate values to a larger type:
            if (left.Tag == OperandTag.Immediate && left.Type == CType.UInt8) left.Type = CType.UInt16;
            if (right.Tag == OperandTag.Immediate && right.Type == CType.UInt8) right.Type = CType.UInt16;
            return left.Type == right.Type;
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
        if (r.Tag == OperandTag.Accumulator)
        {
            string temp = DeclareTemporary(r.Type);
            Emit(Expr.MakeAsm("STA", new AsmOperand(temp, AddressMode.Absolute)));
            Emit(Expr.MakeAsm("STX", new AsmOperand(temp, 1, AddressMode.Absolute)));
            return new Operand
            {
                Tag = OperandTag.Variable,
                Name = temp,
                Type = r.Type,
            };
        }
        else if (r.IsFlag())
        {
            Program.NYI();
            return null;
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

    void PushAccumulator(CType type)
    {
        if (Stack.Any(x => x.Tag == OperandTag.Accumulator || x.IsFlag()))
        {
            Program.Panic("An accumulator or flag operand was overwritten; it should have been saved.");
        }

        Stack.Add(new Operand
        {
            Tag = OperandTag.Accumulator,
            Type = type,
        });
    }

    void PushImmediate(int n)
    {
        Stack.Add(new Operand
        {
            Tag = OperandTag.Immediate,
            Value = n,
            Type = (n <= byte.MaxValue) ? CType.UInt8 : CType.UInt16,
        });
    }

    void PushVariable(string name, CType type)
    {
        Symbol sym;
        if (!Symbols.TryGetValue(name, out sym)) Program.Error("variable not defined: {0}", name);

        Stack.Add(new Operand
        {
            Tag = OperandTag.Variable,
            Name = name,
            Type = type,
        });
    }

    void PushVariableAddress(string name, CType type)
    {
        Symbol sym;
        if (!Symbols.TryGetValue(name, out sym)) Program.Error("variable not defined: {0}", name);

        Stack.Add(new Operand
        {
            Tag = OperandTag.VariableAddress,
            Name = name,
            Type = CType.MakePointer(type),
        });
    }

    Operand Pop()
    {
        Operand r = Stack[Stack.Count - 1];
        Stack.RemoveAt(Stack.Count - 1);
        return r;
    }

    void EmitLoadAccumulator(Operand r)
    {
        int size = SizeOf(r.Type);

        if (r.Tag == OperandTag.Accumulator)
        {
            // NOP
        }
        else if (r.Tag == OperandTag.Immediate)
        {
            if (size >= 1) Emit(Expr.MakeAsm("LDA", new AsmOperand(LowByte(r.Value), AddressMode.Immediate)));
            if (size >= 2) Emit(Expr.MakeAsm("LDX", new AsmOperand(HighByte(r.Value), AddressMode.Immediate)));
            if (size > 2) Program.Panic("value is too large");
        }
        else if (r.Tag == OperandTag.Variable)
        {
            if (size >= 1) Emit(Expr.MakeAsm("LDA", new AsmOperand(r.Name, AddressMode.Absolute)));
            if (size >= 2) Emit(Expr.MakeAsm("LDX", new AsmOperand(r.Name, 1, AddressMode.Absolute)));
            if (size > 2) Program.Panic("value is too large");
        }
        else
        {
            Program.NYI();
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
    public OperandTag Tag;
    public int Value;
    public string Name;
    public CType Type;

    public string Show()
    {
        if (Tag == OperandTag.Accumulator) return "A";
        else if (Tag == OperandTag.Immediate) return "#" + Value;
        else if (Tag == OperandTag.Variable) return Name;
        else if (Tag == OperandTag.VariableAddress) return "&" + Name;
        else if (Tag == OperandTag.FlagZero) return "ZF";
        else if (Tag == OperandTag.FlagNotZero) return "!ZF";
        else if (Tag == OperandTag.FlagCarry) return "CF";
        else if (Tag == OperandTag.FlagNotCarry) return "!CF";
        else return "???";
    }

    public bool IsFlag()
    {
        return Tag == OperandTag.FlagZero || Tag == OperandTag.FlagNotZero ||
            Tag == OperandTag.FlagCarry || Tag == OperandTag.FlagNotCarry;
    }
}

enum OperandTag
{
    Accumulator,
    Immediate,
    Variable,
    VariableAddress,
    FlagZero,
    FlagNotZero,
    FlagCarry,
    FlagNotCarry,
}
