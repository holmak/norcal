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
    List<LexicalScope> Scopes = new List<LexicalScope>();
    LoopScope Loop;

    public static readonly string NamespaceSeparator = ":";

    public static List<Expr> Compile(List<Expr> declarations)
    {
        Compiler compiler = new Compiler();
        compiler.CompileDeclarations(declarations);
        return compiler.StackCode;
    }

    void CompileDeclarations(List<Expr> declarations)
    {
        Scopes.Add(new LexicalScope("<global>"));

        foreach (Expr decl in declarations)
        {
            MemoryRegion region;
            CType returnType, type;
            string name;
            FieldInfo[] fields;
            Expr body;
            if (decl.Match(Tag.Function, out returnType, out name, out fields, out body))
            {
                BeginScope(name);

                foreach (FieldInfo f in fields)
                {
                    DefineQualifiedVariableName(f.Name);
                }

                // Find labels in this function and forward-declare them.
                body.ForEach(subexpr =>
                {
                    string label;
                    if (subexpr.Match(Tag.Label, out label))
                    {
                        DefineQualifiedLabelName(label);
                    }
                });

                Emit(Tag.Function, returnType, name, fields);
                CompileExpression(body);

                // Functions that return non-void should never reach this point.
                Emit(Tag.PushImmediate, 0);
                Emit(Tag.Return);

                EndScope();
            }
            else if (decl.Match(Tag.Constant, out type, out name, out body))
            {
                int value;
                if (!body.Match(Tag.Int, out value))
                {
                    Program.Error("expression must be constant");
                }

                Emit(Tag.Constant, type, DefineQualifiedVariableName(name), value);
            }
            else if (decl.Match(Tag.Variable, out region, out type, out name))
            {
                Emit(Tag.Variable, region, type, DefineQualifiedVariableName(name));
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
        string name, functionName, target, fieldName, mnemonic, mode;
        MemoryRegion region;
        CType type;
        Expr[] args;
        Expr subexpr, init, test, induction, body, left, indexExpr;
        AsmOperand operand;
        if (e.Match(Tag.Int, out value))
        {
            Emit(Tag.PushImmediate, value);
        }
        else if (e.Match(Tag.Name, out name))
        {
            Emit(Tag.PushVariable, FindQualifiedName(name));
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

            BeginScope("for");
            CompileExpression(init);
            Emit(Tag.Label, Loop.ContinueLabel);
            CompileExpression(test);
            Emit(Tag.JumpIfFalse, Loop.BreakLabel);
            CompileExpression(body);
            CompileExpression(induction);
            Emit(Tag.Jump, Loop.ContinueLabel);
            Emit(Tag.Label, Loop.BreakLabel);
            EndScope();
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
        else if (e.Match(Tag.Variable, out region, out type, out name))
        {
            Emit(Tag.Variable, region, type, DefineQualifiedVariableName(name));
        }
        else if (e.Match(Tag.Scope, out subexpr))
        {
            BeginScope("scope");
            CompileExpression(subexpr);
            EndScope();
        }
        else if (e.Match(Tag.Label, out target))
        {
            // Labels are declared before this point, so just look it up.
            Emit(Tag.Label, FindQualifiedName(target));
        }
        else if (e.Match(Tag.Jump, out target))
        {
            Emit(e);
        }
        else if (e.Match(Tag.Asm, out mnemonic, out operand, out mode))
        {
            // Replace any identifiers in assembly instructions with the fully qualified equivalent.
            if (operand.Base.HasValue)
            {
                operand = new AsmOperand(FindQualifiedName(operand.Base.Value), operand.Offset);
            }
            Emit(Expr.MakeAsm(mnemonic, operand, mode));
        }
        else if (e.MatchAny(out functionName, out args))
        {
            foreach (Expr arg in args)
            {
                CompileExpression(arg);
            }
            Emit(functionName);
        }
        else
        {
            Program.UnhandledCase();
        }
    }

    void Emit(params object[] args)
    {
        Emit(Expr.Make(args));
    }

    void Emit(Expr e)
    {
        StackCode.Add(e);
    }

    void BeginScope(string prefix)
    {
        LexicalScope outer = Scopes.Last();

        // Find a unique name:
        string qualifiedName = prefix;
        int suffix = 0;
        while (outer.SubscopeNames.Contains(qualifiedName))
        {
            suffix += 1;
            qualifiedName = string.Format("{0}_{1}", prefix, suffix);
        }

        outer.SubscopeNames.Add(qualifiedName);
        Scopes.Add(new LexicalScope(qualifiedName));
    }

    void EndScope()
    {
        Scopes.RemoveAt(Scopes.Count - 1);
    }

    string MakeUniqueLabel(string prefix)
    {
        var table = Scopes[1].QualifiedNames;

        // Find a unique name:
        string name = prefix;
        int suffix = 0;
        while (table.ContainsKey(name))
        {
            suffix += 1;
            name = string.Format("{0}_{1}", prefix, suffix);
        }

        return DefineQualifiedLabelName(name);
    }

    string DefineQualifiedLabelName(string name)
    {
        // Labels have function scope, not full lexical scope.
        LexicalScope functionScope = Scopes[1];
        if (functionScope.QualifiedNames.ContainsKey(name)) Program.Error("symbol already defined: {0}", name);
        string qualifiedName = functionScope.Name + NamespaceSeparator + name;
        functionScope.QualifiedNames.Add(name, qualifiedName);
        return qualifiedName;
    }

    string DefineQualifiedVariableName(string name)
    {
        var table = Scopes.Last().QualifiedNames;
        if (table.ContainsKey(name)) Program.Error("symbol already defined: {0}", name);
        string qualifiers = string.Join(NamespaceSeparator, Scopes.Skip(1).Select(x => x.Name));
        string qualifiedName = (qualifiers.Length > 0) ? (qualifiers + NamespaceSeparator + name) : name;
        table.Add(name, qualifiedName);
        return qualifiedName;
    }

    string FindQualifiedName(string name)
    {
        // Search all scopes, starting from the innermost.
        foreach (LexicalScope scope in Enumerable.Reverse(Scopes))
        {
            string found;
            if (scope.QualifiedNames.TryGetValue(name, out found)) return found;
        }

        Program.Error("symbol not defined: {0}", name);
        return null;
    }
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
    public readonly string Name;
    public readonly HashSet<string> SubscopeNames = new HashSet<string>();
    public readonly Dictionary<string, string> QualifiedNames = new Dictionary<string, string>();

    public LexicalScope(string name)
    {
        Name = name;
    }
}

class LoopScope
{
    public LoopScope Outer;
    public string ContinueLabel;
    public string BreakLabel;
}

struct MemoryRegion
{
    public readonly MemoryRegionTag Tag;
    public readonly int FixedAddress;

    MemoryRegion(MemoryRegionTag tag, int address)
    {
        Tag = tag;
        FixedAddress = address;
    }

    public static readonly MemoryRegion ZeroPage = new MemoryRegion(MemoryRegionTag.ZeroPage, 0);
    public static readonly MemoryRegion Ram = new MemoryRegion(MemoryRegionTag.Ram, 0);
    public static MemoryRegion Fixed(int address) => new MemoryRegion(MemoryRegionTag.Fixed, address);
}

enum MemoryRegionTag
{
    ZeroPage,
    Ram,
    Fixed,
}
