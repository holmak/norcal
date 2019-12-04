using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

/// <summary>
/// Expr is treated as a dynamically-typed tuple of symbols, numbers, and sub-expressions.
/// </summary>
[DebuggerDisplay("{Show(),nq}")]
class Expr
{
    private readonly object[] Args;

    Expr(object[] args)
    {
        foreach (object arg in args)
        {
            if (arg == null) throw new Exception("Null in tuple.");
            if (!(arg is int || arg is string || arg is MemoryRegion || arg is CType || arg is FieldInfo[] || arg is Expr || arg is AsmOperand))
            {
                throw new Exception("Unsupported type in tuple: " + arg.GetType());
            }
        }

        Args = args;
    }

    public static Expr Make(params object[] args) => new Expr(args);

    public static Expr MakeAsm(string mnemonic)
    {
        return MakeAsm(mnemonic, new AsmOperand(0), Tag.Implicit);
    }

    public static Expr MakeAsm(string mnemonic, AsmOperand operand)
    {
        return MakeAsm(mnemonic, operand, Tag.Absolute);
    }

    public static Expr MakeAsm(string mnemonic, AsmOperand operand, string mode)
    {
        if (AsmInfo.ShortJumpInstructions.Contains(mnemonic)) mode = Tag.Relative;
        return Make(Tag.Asm, mnemonic, operand, mode);
    }

    public IEnumerable<object> GetArgs() => Args;

    public bool MatchTag(string tag)
    {
        return (string)Args[0] == tag;
    }

    public bool MatchAnyTag(out string tag)
    {
        if (Args.Length >= 1 && Args[0] is string)
        {
            tag = (string)Args[0];
            return true;
        }
        else
        {
            tag = null;
            return false;
        }
    }

    public bool Match(string tag)
    {
        if (Args.Length == 1 &&
            Args[0] is string && (string)Args[0] == tag)
        {
            return true;
        }

        return false;
    }

    public bool Match<T1>(string tag, out T1 var1)
    {
        if (Args.Length == 2 &&
            Args[0] is string && (string)Args[0] == tag &&
            Args[1] is T1)
        {
            var1 = (T1)Args[1];
            return true;
        }

        var1 = default(T1);
        return false;
    }

    public bool Match<T1, T2>(string tag, out T1 var1, out T2 var2)
    {
        if (Args.Length == 3 &&
            Args[0] is string && (string)Args[0] == tag &&
            Args[1] is T1 &&
            Args[2] is T2)
        {
            var1 = (T1)Args[1];
            var2 = (T2)Args[2];
            return true;
        }

        var1 = default(T1);
        var2 = default(T2);
        return false;
    }

    public bool Match<T1, T2, T3>(string tag, out T1 var1, out T2 var2, out T3 var3)
    {
        if (Args.Length == 4 &&
            Args[0] is string && (string)Args[0] == tag &&
            Args[1] is T1 &&
            Args[2] is T2 &&
            Args[3] is T3)
        {
            var1 = (T1)Args[1];
            var2 = (T2)Args[2];
            var3 = (T3)Args[3];
            return true;
        }

        var1 = default(T1);
        var2 = default(T2);
        var3 = default(T3);
        return false;
    }

    public bool Match<T1, T2, T3, T4>(string tag, out T1 var1, out T2 var2, out T3 var3, out T4 var4)
    {
        if (Args.Length == 5 &&
            Args[0] is string && (string)Args[0] == tag &&
            Args[1] is T1 &&
            Args[2] is T2 &&
            Args[3] is T3 &&
            Args[4] is T4)
        {
            var1 = (T1)Args[1];
            var2 = (T2)Args[2];
            var3 = (T3)Args[3];
            var4 = (T4)Args[4];
            return true;
        }

        var1 = default(T1);
        var2 = default(T2);
        var3 = default(T3);
        var4 = default(T4);
        return false;
    }

    public bool MatchAny<T>(string tag, out T[] vars)
    {
        if (Args.Length >= 1 &&
            Args[0] is string && (string)Args[0] == tag &&
            Args.Skip(1).All(x => x is T))
        {
            vars = Args.Skip(1).Cast<T>().ToArray();
            return true;
        }

        vars = null;
        return false;
    }

    public bool MatchAny<T>(out string tag, out T[] vars)
    {
        if (Args.Length >= 1 &&
            Args[0] is string &&
            Args.Skip(1).All(x => x is T))
        {
            tag = (string)Args[0];
            vars = Args.Skip(1).Cast<T>().ToArray();
            return true;
        }

        tag = null;
        vars = null;
        return false;
    }

    /// <summary>
    /// Run an action on each node in this expression tree.
    /// </summary>
    public void ForEach(Action<Expr> p)
    {
        foreach (Expr subexpr in Args.OfType<Expr>())
        {
            p(subexpr);
            subexpr.ForEach(p);
        }
    }

    /// <summary>
    /// Apply a function to this expression tree.
    /// </summary>
    public Expr Map(Func<Expr, Expr> f)
    {
        object[] results = new object[Args.Length];

        for (int i = 0; i < Args.Length; i++)
        {
            Expr subexpr = Args[i] as Expr;
            if (subexpr != null)
            {
                results[i] = f(subexpr);
            }
            else
            {
                // Return other elements unchanged:
                results[i] = Args[i];
            }
        }

        return Make(results);
    }

    public string Show() => ShowWithOptions(false);

    public string ShowMultiline() => ShowWithOptions(true);

    string ShowWithOptions(bool multiline)
    {
        return ShowStringTree(multiline, ToStringTree());
    }

    /// <summary>
    /// The return value is a "string tree": a tree where each node is a string or an array of subtrees.
    /// </summary>
    object ToStringTree()
    {
        object[] tree = new object[Args.Length];

        for (int i = 0; i < Args.Length; i++)
        {
            int? integer = Args[i] as int?;
            string name = Args[i] as string;
            Expr subexpr = Args[i] as Expr;
            MemoryRegion? region = Args[i] as MemoryRegion?;
            CType type = Args[i] as CType;
            FieldInfo[] fields = Args[i] as FieldInfo[];
            AsmOperand operand = Args[i] as AsmOperand;

            if (integer != null)
            {
                int n = integer.Value;
                tree[i] = (n < 128) ? n.ToString() : "$" + n.ToString("X");
            }
            else if (name != null)
            {
                tree[i] = name;
            }
            else if (subexpr != null)
            {
                tree[i] = subexpr.ToStringTree();
            }
            else if (region != null)
            {
                tree[i] = region.ToString();
            }
            else if (!ReferenceEquals(type, null))
            {
                tree[i] = "<" + type.Show() + ">";
            }
            else if (fields != null)
            {
                tree[i] = "<fields>";
            }
            else if (operand != null)
            {
                tree[i] = operand.Show();
            }
            else
            {
                Program.NYI();
            }
        }

        return tree;
    }

    /// <summary>
    /// Format a "string tree" into a multi-line, indented string.
    /// Each node must be a string or another "string tree".
    /// </summary>
    static string ShowStringTree(bool multiline, object tree)
    {
        if (tree is string) return (string)tree;

        IEnumerable<object> subtrees = (IEnumerable<object>)tree;
        IEnumerable<string> substrings = subtrees.Select(x => ShowStringTree(multiline, x));
        bool small = subtrees.Count() <= 2 || !subtrees.Any(x => x is Array);
        if (small || !multiline)
        {
            return "(" + string.Join(" ", substrings) + ")";
        }
        else
        {
            return "(" + string.Join("\n    ", substrings.Select(s => s.Replace("\n", "\n    "))) + ")";
        }
    }
}
