using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

class StackAssembler
{
    List<Expr> Output = new List<Expr>();
    Dictionary<string, CFunctionInfo> Functions = new Dictionary<string, CFunctionInfo>();
    Dictionary<string, CStructInfo> Structs = new Dictionary<string, CStructInfo>();
    int ZeroPageNext = ZeroPageStart;
    int RamNext = RamStart;
    LexicalScope Scope = new LexicalScope();
    List<Operand> Stack = new List<Operand>();

    static readonly int ZeroPageStart = 0x000;
    static readonly int ZeroPageEnd = 0x100;
    static readonly int RamStart = 0x300;
    static readonly int RamEnd = 0x800;

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
        // HACK: If you don't define an interrupt handler, it will target address zero.
        // TODO: What should the compiler do if an interrupt handler isn't defined? Is it an error?
        Emit(Asm.Label, "nmi");
        Emit(Asm.Label, "reset");
        Emit(Asm.Label, "brk");

        foreach (Expr op in input)
        {
            string name, functionName;
            MemoryRegion region;
            CType type, returnType;
            FieldInfo[] fields;
            int number;
            if (op.Match(Stk.Function, out returnType, out functionName, out fields))
            {
                if (Functions.ContainsKey(functionName)) Program.Error("function is already defined: " + functionName);

                // Allocate space for each parameter and store it in the symbol table.
                int[] addresses = new int[fields.Length];
                for (int i = 0; i < fields.Length; i++)
                {
                    addresses[i] = AllocGlobal(fields[i].Region, SizeOf(fields[i].Type));
                }

                CFunctionInfo function = new CFunctionInfo
                {
                    ParameterTypes = fields.Select(x => x.Type).ToArray(),
                    ParameterAddresses = addresses,
                    ReturnType = returnType,
                };
                Functions.Add(functionName, function);

                // Get the call frame address (and type information) from the function's type entry.
                CType[] paramTypes = function.ParameterTypes;
                int[] paramAddresses = function.ParameterAddresses;

                // Define each of the function's parameters as a local variable:
                Scope = Scope.Push();
                foreach (FieldInfo f in fields)
                {
                    DefineSymbol(SymbolTag.Variable, f.Name, f.Type, f.Address);
                }
            }
            else if (op.Match(Stk.Constant, out type, out name, out number))
            {
                // TODO: Make sure the value fits in the specified type.
                DefineSymbol(SymbolTag.Constant, name, type, number);
            }
            else if (op.Match(Stk.Variable, out region, out type, out name))
            {
                int address = AllocGlobal(region, SizeOf(type));
                DefineSymbol(SymbolTag.Variable, name, type, address);
            }
            else if (op.Match(Stk.Struct, out name, out fields))
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
            else if (op.Match(Stk.PushImmediate, out number))
            {
                PushImmediate(number);
            }
            else if (op.Match(Stk.PushVariable, out name))
            {
                PushVariable(name);
            }
            else if (op.Match(Stk.Call, out functionName))
            {
                if (functionName == Tag.StoreGeneric)
                {
                    Operand value = Pop();
                    Operand address = Pop();
                    EmitOp("LDA", value);
                    EmitOp("STA", address);
                }
                else if (functionName == Tag.AddGeneric)
                {
                    Operand right = Pop();
                    Operand left = Pop();
                    Emit("CLC");
                    EmitOp("LDA", left);
                    EmitOp("ADC", right);
                    PushAccumulator();
                }
                else if (functionName == Tag.SubtractGeneric)
                {
                    Operand right = Pop();
                    Operand left = Pop();
                    Emit("SEC");
                    EmitOp("LDA", left);
                    EmitOp("SBC", right);
                    PushAccumulator();
                }
                else
                {
                    // This is a general-purpose call.
                    // TODO: Copy the arguments into the function's call frame.
                    Emit("JSR", functionName);
                }
            }
            else if (op.Match(Stk.Return))
            {
                // TODO: "Return" must load top-of-stack into the accumulator, then RTS.
                Emit("RTS");
            }
        }

        // Put the interrupt vector table at the end of ROM:
        Emit(Asm.SkipTo, 0xFFFA);
        Emit(Asm.Word, "nmi");
        Emit(Asm.Word, "reset");
        Emit(Asm.Word, "brk");
    }

    void Emit(params object[] args)
    {
        Output.Add(Expr.Make(args));
    }

    void DefineSymbol(SymbolTag tag, string name, CType type, int value)
    {
        // It is an error to define two things with the same name in the same scope.
        if (Scope.Symbols.Any(x => x.Name == name))
        {
            Program.Error("symbols cannot be redefined: {0}", name);
        }

        Scope.Symbols.Add(new Symbol
        {
            Tag = tag,
            Name = name,
            Type = type,
            Value = value,
        });
    }

    bool FindSymbol(string name, out Symbol found)
    {
        // Search all scopes, starting from the innermost.
        for (; Scope != null; Scope = Scope.Outer)
        {
            foreach (Symbol sym in Scope.Symbols)
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

    CStructInfo GetStructInfo(string name)
    {
        CStructInfo info;
        if (!Structs.TryGetValue(name, out info)) Program.Error("struct not defined: {0}", name);
        return info;
    }

    /// <summary>
    /// Allocate 'size' bytes in RAM and return the address.
    /// </summary>
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

    int SizeOf(CType type)
    {
        if (type.IsSimple)
        {
            if (type.SimpleType == CSimpleType.UInt8) return 1;
            else if (type.SimpleType == CSimpleType.UInt16) return 2;
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

    void PushAccumulator()
    {
        Stack.Add(new Operand
        {
            Tag = OperandTag.Accumulator,
        });
    }

    void PushImmediate(int n)
    {
        Stack.Add(new Operand
        {
            Tag = OperandTag.Immediate,
            Value = n,
        });
    }

    void PushVariable(string name)
    {
        Symbol sym;
        if (!FindSymbol(name, out sym)) Program.Error("variable not defined: {0}", name);

        Stack.Add(new Operand
        {
            Tag = OperandTag.Variable,
            Value = sym.Value,
        });
    }

    Operand Pop()
    {
        Operand r = Stack[Stack.Count - 1];
        Stack.RemoveAt(Stack.Count - 1);
        return r;
    }

    void EmitOp(string mnemonic, Operand r)
    {
        // TODO: When the operation would affect the accumulator or flag register, flush all acc/flag
        // values on the stack to temporaries.

        if (r.Tag == OperandTag.Accumulator)
        {
            if (mnemonic == "LDA")
            {
                // NOP
            }
            else
            {
                Program.NYI();
            }
        }
        else if (r.Tag == OperandTag.Immediate)
        {
            Emit(mnemonic, r.Value, Asm.Immediate);
        }
        else
        {
            Program.NYI();
        }
    }
}

class Operand
{
    public OperandTag Tag;
    public int Value;
}

enum OperandTag
{
    Accumulator,
    Immediate,
    Variable,
    Flag,
}
