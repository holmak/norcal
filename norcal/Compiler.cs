using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

class Compiler
{
    List<Expr> StackCode = new List<Expr>();
    int NextLabelNumber = 0;
    LoopScope Loop;

    public static List<Expr> Compile(List<Expr> declarations)
    {
        Compiler compiler = new Compiler();
        compiler.CompileDeclarations(declarations);
        return compiler.StackCode;
    }

    void CompileDeclarations(List<Expr> declarations)
    {
        foreach (Expr decl in declarations)
        {
            MemoryRegion region;
            CType returnType, type;
            string name;
            FieldInfo[] fields;
            Expr body;
            if (decl.Match(Tag.Function, out returnType, out name, out fields, out body))
            {
                Emit(Stk.Function, returnType, name, fields);
                CompileExpression(body);

                // Functions that return non-void should never reach this point.
                Emit(Stk.PushImmediate, 0, CType.Void);
                Emit(Stk.Return);
            }
            else if (decl.Match(Tag.Constant, out type, out name, out body))
            {
                // TODO: Make sure the value fits in the specified type.
                int value;
                if (!body.Match(Tag.Int, out value))
                {
                    Program.Error("expression must be constant");
                }

                Emit(Stk.Constant, type, name, value);
            }
            else if (decl.Match(Tag.Variable, out region, out type, out name))
            {
                Emit(Stk.Variable, region, type, name);
            }
            else if (decl.Match(Tag.Struct, out name, out fields))
            {
                Emit(Stk.Struct, name, fields);
            }
            else
            {
                Program.UnhandledCase();
            }
        }
    }

    void CompileExpression(Expr e)
    {
        int value;
        string name, functionName, target, fieldName;
        MemoryRegion region;
        CType type;
        Expr[] args;
        Expr subexpr, init, test, induction, body, left, indexExpr;
        if (e.Match(Tag.Int, out value))
        {
            Emit(Stk.PushImmediate, value);
        }
        else if (e.Match(Tag.Name, out name))
        {
            Emit(Stk.PushVariable, name);
        }
        else if (e.Match(Tag.Empty))
        {
            // NOP
        }
        else if (e.MatchAny(Tag.Sequence, out args))
        {
            foreach (Expr sequenceExpr in args)
            {
                CompileExpression(sequenceExpr);
            }
        }
        else if (e.MatchAny(Tag.Switch, out args))
        {
            string end = MakeUniqueLabel("end_if");

            for (int i = 0; i < args.Length; i += 2)
            {
                string nextClause = MakeUniqueLabel("else");

                // If this clause's condition is false, try the next clause:
                CompileExpression(args[i]);
                Emit(Stk.JumpIfFalse, nextClause);
                // If the condition was true, execute the clause body:
                CompileExpression(args[i + 1]);
                // After executing the body of a clause, skip the rest of the clauses:
                Emit(Stk.Jump, end);
                Emit(Stk.Label, nextClause);
            }

            Emit(Stk.Label, end);
        }
        else if (e.Match(Tag.Continue))
        {
            Emit(Stk.Jump, Loop.ContinueLabel);
        }
        else if (e.Match(Tag.Break))
        {
            Emit(Stk.Jump, Loop.BreakLabel);
        }
        else if (e.Match(Tag.For, out init, out test, out induction, out body))
        {
            Loop = new LoopScope
            {
                Outer = Loop,
                ContinueLabel = MakeUniqueLabel("for_top"),
                BreakLabel = MakeUniqueLabel("for_bottom"),
            };

            CompileExpression(init);
            Emit(Stk.Label, Loop.ContinueLabel);
            CompileExpression(test);
            Emit(Stk.JumpIfFalse, Loop.BreakLabel);
            CompileExpression(body);
            CompileExpression(induction);
            Emit(Stk.Jump, Loop.ContinueLabel);
            Emit(Stk.Label, Loop.BreakLabel);
        }
        else if (e.Match(Tag.Return, out subexpr))
        {
            CompileExpression(subexpr);
            Emit(Stk.Return);
        }
        else if (e.Match(Tag.Field, out left, out fieldName))
        {
            CompileExpression(left);
            Emit(Stk.PushFieldName, fieldName);
            Emit(Stk.Field);
        }
        else if (e.Match(Tag.Index, out left, out indexExpr))
        {
            CompileExpression(left);
            CompileExpression(indexExpr);
            Emit(Stk.Index);
        }
        else if (e.Match(Tag.Cast, out type, out subexpr))
        {
            CompileExpression(subexpr);
        }
        else if (e.Match(Tag.Local, out region, out type, out name))
        {
            Emit(Stk.Variable, region, type, name);
        }
        else if (e.Match(Tag.Scope, out subexpr))
        {
            Emit(Stk.BeginScope);
            CompileExpression(subexpr);
            Emit(Stk.EndScope);
        }
        else if (e.Match(Tag.Label, out target))
        {
            Emit(Stk.Label, target);
        }
        else if (e.Match(Tag.Goto, out target))
        {
            Emit(Stk.Jump, target);
        }
        else if (e.Match(Tag.GotoIf, out subexpr, out target))
        {
            CompileExpression(subexpr);
            Emit(Stk.JumpIfTrue, target);
        }
        else if (e.Match(Tag.GotoIfNot, out subexpr, out target))
        {
            CompileExpression(subexpr);
            Emit(Stk.JumpIfFalse, target);
        }
        else if (e.MatchTag(Tag.Asm))
        {
            // Copy assembly instructions almost verbatim:
            Emit(e.GetArgs().Skip(1).ToArray());
        }
        else if (e.MatchAny(out functionName, out args))
        {
            foreach (Expr arg in args)
            {
                CompileExpression(arg);
            }
            Emit(Stk.Call, functionName);
        }
        else
        {
            Program.UnhandledCase();
        }
    }

    string MakeUniqueLabel(string prefix)
    {
        return string.Format("@{0}_{1}", prefix, NextLabelNumber++);
    }

    void Emit(params object[] args)
    {
        Expr e = Expr.Make(args);
        StackCode.Add(e);
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

class CStructInfo
{
    public int TotalSize;
    public CField[] Fields;
}

class CField
{
    public CType Type;
    public string Name;
    public int Offset;
}

[DebuggerDisplay("{Show(),nq}")]
partial class CFunctionInfo
{
    public CType[] ParameterTypes;
    public CType ReturnType;
    public int[] ParameterAddresses;

    public override bool Equals(object obj)
    {
        throw new NotSupportedException();
    }

    public override int GetHashCode()
    {
        throw new NotSupportedException();
    }

    public string Show()
    {
        var paramTypes = ParameterTypes.Select(x => x.Show());
        return string.Format("function({0}) {1}", string.Join(", ", paramTypes), ReturnType.Show());
    }
}

class LexicalScope
{
    public LexicalScope Outer;
    public List<Symbol> Symbols = new List<Symbol>();

    public LexicalScope Push()
    {
        LexicalScope inner = new LexicalScope();
        inner.Outer = this;
        return inner;
    }
}

class LoopScope
{
    public LoopScope Outer;
    public string ContinueLabel;
    public string BreakLabel;
}

enum MemoryRegion
{
    ZeroPage,
    Ram,
}
