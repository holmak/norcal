using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

class StackAssembler
{
    private List<Expr> Output = new List<Expr>();

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
        Program.NYI();

        // HACK: If you don't define an interrupt handler, it will target address zero.
        // TODO: What should the compiler do if an interrupt handler isn't defined? Is it an error?
        Emit(Asm.Label, "nmi");
        Emit(Asm.Label, "reset");
        Emit(Asm.Label, "brk");

        foreach (Expr op in input)
        {
            string functionName;
            if (op.Match(Stk.Call, out functionName))
            {
                /*
                CFunctionInfo functionInfo;
                if (!Functions.TryGetValue(functionName, out functionInfo)) Program.Error("function not defined: " + functionName);

                // Get the call frame address (and type information) from the function's type entry.
                CType[] paramTypes = functionInfo.ParameterTypes;
                int[] paramAddresses = functionInfo.ParameterAddresses;
                */

                // TODO: Copy the arguments into the function's call frame.
                Emit("JSR", functionName);
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
}

static class Stk
{
    public static readonly string Function = "vsm/function";
    public static readonly string Label = "vsm/label";
    public static readonly string Comment = "vsm/comment";

    public static readonly string PushImmediate = "vsm/push";
    public static readonly string Jump = "vsm/j";
    public static readonly string JumpIfTrue = "vsm/jt";
    public static readonly string JumpIfFalse = "vsm/jf";
    public static readonly string Call = "vsm/call";
    public static readonly string Return = "vsm/ret";
}
