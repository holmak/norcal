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
            CType returnType, type;
            string name;
            FieldInfo[] fields;
            Expr body;
            if (decl.Match(Tag.Function, out returnType, out name, out fields, out body))
            {
                Emit(Tag.Function, returnType, name, fields);
                CompileExpression(body);

                // Functions that return non-void should never reach this point.
                Emit(Tag.PushImmediate, 0);
                Emit(Tag.Return);
            }
            else if (decl.Match(Tag.Constant, out type, out name, out body))
            {
                // TODO: Make sure the value fits in the specified type.
                int value;
                if (!body.Match(Tag.Int, out value))
                {
                    Program.Error("expression must be constant");
                }

                Emit(Tag.Constant, type, name, value);
            }
            else if (decl.MatchTag(Tag.Variable))
            {
                Emit(decl);
            }
            else if (decl.MatchTag(Tag.Struct))
            {
                Emit(decl);
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
            Emit(Tag.PushImmediate, value);
        }
        else if (e.Match(Tag.Name, out name))
        {
            Emit(Tag.PushVariable, name);
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
                Emit(Tag.JumpIfFalse, nextClause);
                // If the condition was true, execute the clause body:
                CompileExpression(args[i + 1]);
                // After executing the body of a clause, skip the rest of the clauses:
                Emit(Tag.Jump, end);
                Emit(Tag.Label, nextClause);
            }

            Emit(Tag.Label, end);
        }
        else if (e.Match(Tag.Continue))
        {
            Emit(Tag.Jump, Loop.ContinueLabel);
        }
        else if (e.Match(Tag.Break))
        {
            Emit(Tag.Jump, Loop.BreakLabel);
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
            Emit(Tag.Label, Loop.ContinueLabel);
            CompileExpression(test);
            Emit(Tag.JumpIfFalse, Loop.BreakLabel);
            CompileExpression(body);
            CompileExpression(induction);
            Emit(Tag.Jump, Loop.ContinueLabel);
            Emit(Tag.Label, Loop.BreakLabel);
        }
        else if (e.Match(Tag.Return, out subexpr))
        {
            CompileExpression(subexpr);
            Emit(Tag.Return);
        }
        else if (e.Match(Tag.Field, out left, out fieldName))
        {
            CompileExpression(left);
            Emit(Tag.PushFieldName, fieldName);
            Emit(Tag.Field);
        }
        else if (e.Match(Tag.Index, out left, out indexExpr))
        {
            CompileExpression(left);
            CompileExpression(indexExpr);
            Emit(Tag.Index);
        }
        else if (e.Match(Tag.Cast, out type, out subexpr))
        {
            CompileExpression(subexpr);
        }
        else if (e.Match(Tag.Local, out region, out type, out name))
        {
            Emit(Tag.Variable, region, type, name);
        }
        else if (e.Match(Tag.Scope, out subexpr))
        {
            Emit(Tag.BeginScope);
            CompileExpression(subexpr);
            Emit(Tag.EndScope);
        }
        else if (e.Match(Tag.Label, out target))
        {
            Emit(Tag.Label, target);
        }
        else if (e.Match(Tag.Jump, out target))
        {
            Emit(e);
        }
        else if (e.Match(Tag.JumpIfTrue, out subexpr, out target))
        {
            CompileExpression(subexpr);
            Emit(Tag.JumpIfTrue, target);
        }
        else if (e.Match(Tag.JumpIfFalse, out subexpr, out target))
        {
            CompileExpression(subexpr);
            Emit(Tag.JumpIfFalse, target);
        }
        else if (e.MatchTag(Tag.Asm))
        {
            // Copy assembly instructions verbatim:
            Emit(e);
        }
        else if (e.MatchAny(out functionName, out args))
        {
            foreach (Expr arg in args)
            {
                CompileExpression(arg);
            }
            Emit(Tag.Call, functionName);
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
        Emit(Expr.Make(args));
    }

    void Emit(Expr e)
    {
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
