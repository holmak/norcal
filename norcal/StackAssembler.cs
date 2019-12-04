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
            string name, functionName;
            MemoryRegion region;
            CType type, returnType;
            FieldInfo[] fields;
            int number;
            if (op.Match(Tag.Function, out returnType, out functionName, out fields))
            {
                if (Functions.ContainsKey(functionName)) Program.Error("function is already defined: " + functionName);

                Emit(Tag.Function, functionName);

                // Allocate a global variable for each parameter:
                foreach (FieldInfo field in fields)
                {
                    string qualifiedName = string.Format("{0}{1}{2}", functionName, Compiler.NamespaceSeparator, field.Name);
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
                // Remove the type information:
                Emit(Tag.Constant, name, number);
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
                PushVariable(name);
            }
            else if (op.Match(Tag.Call, out functionName))
            {
                if (functionName == Tag.StoreGeneric)
                {
                    Operand value = Pop();
                    Operand dest = Pop();
                    EmitAsm("LDA", value);
                    EmitAsm("STA", dest);
                }
                else if (functionName == Tag.AddGeneric)
                {
                    Operand right = Pop();
                    Operand left = Pop();
                    EmitAsm("CLC");
                    EmitAsm("LDA", left);
                    EmitAsm("ADC", right);
                    PushAccumulator();
                }
                else if (functionName == Tag.SubtractGeneric)
                {
                    Operand right = Pop();
                    Operand left = Pop();
                    EmitAsm("SEC");
                    EmitAsm("LDA", left);
                    EmitAsm("SBC", right);
                    PushAccumulator();
                }
                else
                {
                    // This is a general-purpose call.
                    // TODO: Copy the arguments into the function's call frame.
                    Emit(Expr.MakeAsm("JSR", new AsmOperand(functionName)));
                }
            }
            else if (op.Match(Tag.Return))
            {
                // TODO: "Return" must load top-of-stack into the accumulator, then RTS.
                Emit(Expr.MakeAsm("RTS"));
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
        // TODO: If there are any other accumulator operands on the virtual stack, they must be flushed to temporaries.

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
        if (!Symbols.TryGetValue(name, out sym)) Program.Error("variable not defined: {0}", name);

        Stack.Add(new Operand
        {
            Tag = OperandTag.Variable,
            Name = name,
        });
    }

    Operand Pop()
    {
        Operand r = Stack[Stack.Count - 1];
        Stack.RemoveAt(Stack.Count - 1);
        return r;
    }

    void EmitAsm(string mnemonic)
    {
        Emit(Expr.MakeAsm(mnemonic));
    }

    void EmitAsm(string mnemonic, Operand r)
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
            Emit(Expr.MakeAsm(mnemonic, new AsmOperand(r.Value), Tag.Immediate));
        }
        else if (r.Tag == OperandTag.Variable)
        {
            Emit(Expr.MakeAsm(mnemonic, new AsmOperand(r.Name)));
        }
        else
        {
            Program.NYI();
        }
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

    public string Show()
    {
        if (Tag == OperandTag.Accumulator) return "A";
        else if (Tag == OperandTag.Immediate) return "#" + Value;
        else if (Tag == OperandTag.Variable) return Name;
        else if (Tag == OperandTag.VariableAddress) return "&" + Name;
        else if (Tag == OperandTag.FlagZero) return "ZF";
        else if (Tag == OperandTag.FlagNotZero) return "!ZF";
        else if (Tag == OperandTag.FlagCarry) return "CF";
        else if (Tag == OperandTag.FlatNotCarry) return "!CF";
        else return "???";
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
    FlatNotCarry,
}
