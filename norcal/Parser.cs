using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

partial class Parser
{
    // Input text:
    string Input;
    int Next = 0;
    FilePosition InputPos;

    // Next token:
    static TokenType NextType;
    static int NextInt;
    static string NextName;
    static FilePosition TokenPos, LastTokenPos;

    public static List<Declaration> ParseFile(string filename)
    {
        Parser p = new Parser(filename);
        return p.ParseFile();
    }

    Parser(string filename)
    {
        Input = File.ReadAllText(filename);
        FetchToken();
    }

    List<Declaration> ParseFile()
    {
        List<Declaration> program = new List<Declaration>();
        while (!TryParse(TokenType.EOF))
        {
            program.Add(ParseDeclaration());
        }
        return program;
    }

    Declaration ParseDeclaration()
    {
        Declaration d = new Declaration();
        if (TryParseName("define"))
        {
            d.Type = DeclarationType.Constant;
            if (!TryParseAnyName(out d.Name)) ParserError("expected a name");
            if (!TryParse(TokenType.EQUALS)) ParserError("expected =");
            d.Body = ParseExpr();
            if (!TryParse(TokenType.SEMICOLON)) ParserError("expected ;");
        }
        else
        {
            d.Type = DeclarationType.Function;
            if (!TryParseName("void")) ParserError("expected 'void'");
            if (!TryParseAnyName(out d.Name)) ParserError("expected function name");
            if (!TryParse(TokenType.LPAREN)) ParserError("expected (");
            if (!TryParse(TokenType.RPAREN)) ParserError("expected )");
            if (!TryParse(TokenType.LBRACE)) ParserError("expected {");
            List<Expr> body = new List<Expr>();
            while (!TryParse(TokenType.RBRACE))
            {
                body.Add(ParseStatement());
            }
            d.Body = MakeSequenceExpr(body);
        }
        return d;
    }

    Expr ParseStatement()
    {
        Expr stmt;
        CType type;
        if (TryParseType(out type))
        {
            // Declare a local variable:
            string localname;
            if (!TryParseAnyName(out localname)) ParserError("expected variable name");
            // If no initial value is specified, initialize to zero.
            Expr value;
            if (TryParse(TokenType.EQUALS)) value = ParseExpr();
            else value = Expr.MakeInt(0);
            if (!TryParse(TokenType.SEMICOLON)) ParserError("expected ;");
            stmt = MakeSequenceExpr(new List<Expr>
            {
                Expr.MakeCall(Expr.MakeName("$local"), Expr.MakeName(localname)),
                MakeAssignExpr(Expr.MakeName(localname), value),
            });
        }
        else if (TryParseName("if"))
        {
            if (!TryParse(TokenType.LPAREN)) ParserError("expected (");
            Expr test = ParseExpr();
            if (!TryParse(TokenType.RPAREN)) ParserError("expected )");
            Expr then;
            if (TryParse(TokenType.LBRACE))
            {
                List<Expr> body = new List<Expr>();
                while (!TryParse(TokenType.RBRACE))
                {
                    body.Add(ParseStatement());
                }
                then = MakeSequenceExpr(body);
            }
            else
            {
                then = ParseStatement();
            }
            stmt = Expr.MakeCall(Expr.MakeName("$switch"), test, then);
        }
        else
        {
            // An expression-statement:
            stmt = ParseExpr();
            if (!TryParse(TokenType.SEMICOLON)) ParserError("expected ;");
        }
        return stmt;
    }

    Expr ParseExpr()
    {
        return ParseCommaExpr();
    }

    // ,
    Expr ParseCommaExpr()
    {
        return ParseAssignExpr();
    }

    // = += -= etc.
    Expr ParseAssignExpr()
    {
        Expr e = ParseLogicalOrExpr();
        if (TryParse(TokenType.EQUALS))
        {
            e = MakeAssignExpr(e, ParseAssignExpr());
        }
        return e;
    }

    // TODO: Incorporate this function into the parsing hierarchy.
    // ? :
    Expr ParseConditionalExpr()
    {
        return ParseLogicalOrExpr();
    }

    // ||
    Expr ParseLogicalOrExpr()
    {
        return ParseLogicalAndExpr();
    }

    // &&
    Expr ParseLogicalAndExpr()
    {
        return ParseBitwiseOrExpr();
    }

    // |
    Expr ParseBitwiseOrExpr()
    {
        return ParseBitwiseAndExpr();
    }

    // &
    Expr ParseBitwiseAndExpr()
    {
        return ParseEqualityExpr();
    }

    // == !=
    Expr ParseEqualityExpr()
    {
        return ParseCompareExpr();
    }

    // > >= etc.
    Expr ParseCompareExpr()
    {
        return ParseShiftExpr();
    }

    // << >>
    Expr ParseShiftExpr()
    {
        return ParseAddExpr();
    }

    // + -
    Expr ParseAddExpr()
    {
        Expr e = ParseMultiplyExpr();
        while (true)
        {
            if (TryParse(TokenType.PLUS))
            {
                e = Expr.MakeCall(Expr.MakeName("$add"), e, ParseMultiplyExpr());
            }
            else if (TryParse(TokenType.MINUS))
            {
                e = Expr.MakeCall(Expr.MakeName("$sub"), e, ParseMultiplyExpr());
            }
            else
            {
                return e;
            }
        }
    }

    // * / %
    Expr ParseMultiplyExpr()
    {
        return ParseCastExpr();
    }

    // (casts)
    Expr ParseCastExpr()
    {
        return ParseUnaryPrefixExpr();
    }

    // Unary prefix operators
    Expr ParseUnaryPrefixExpr()
    {
        if (TryParse(TokenType.STAR))
        {
            return Expr.MakeCall(Expr.MakeName("$load"), ParseSuffixExpr());
        }
        else
        {
            return ParseSuffixExpr();
        }
    }

    // Suffix operators
    Expr ParseSuffixExpr()
    {
        return ParsePrimaryExpr();
    }

    // "Primary" expressions
    Expr ParsePrimaryExpr()
    {
        int n;
        string name;
        if (TryParseInt(out n))
        {
            return Expr.MakeInt(n);
        }
        else if (TryParseAnyName(out name))
        {
            return Expr.MakeName(name);
        }
        else if (TryParse(TokenType.LPAREN))
        {
            Expr e = ParseExpr();
            if (!TryParse(TokenType.RPAREN)) ParserError("expected )");
            return e;
        }
        else
        {
            ParserError("expected an expression");
            return null;
        }
    }

    Expr MakeSequenceExpr(List<Expr> args)
    {
        return Expr.MakeCall(Expr.MakeName("$sequence"), args);
    }

    Expr AddressOf(Expr e)
    {
        Expr inner;
        if (e.MatchUnaryCall("$load", out inner))
        {
            return inner;
        }
        else
        {
            return Expr.MakeCall(Expr.MakeName("$addr_of"), e);
        }
    }

    Expr MakeAssignExpr(Expr dst, Expr src)
    {
        return Expr.MakeCall(Expr.MakeName("$assign"), AddressOf(dst), src);
    }

    bool TryParse(TokenType expected)
    {
        if (NextType == expected)
        {
            FetchToken();
            return true;
        }
        else
        {
            return false;
        }
    }

    bool TryParseInt(out int n)
    {
        if (NextType == TokenType.INT)
        {
            n = NextInt;
            FetchToken();
            return true;
        }
        else
        {
            n = 0;
            return false;
        }
    }

    bool TryParseAnyName(out string name)
    {
        if (NextType == TokenType.NAME)
        {
            name = NextName;
            FetchToken();
            return true;
        }
        else
        {
            name = null;
            return false;
        }
    }

    bool TryParseName(string name)
    {
        if (NextType == TokenType.NAME && name == NextName)
        {
            FetchToken();
            return true;
        }
        else
        {
            return false;
        }
    }

    bool TryParseType(out CType type)
    {
        // TODO: Recognize all type names.
        if (TryParseName("uint16_t"))
        {
            type = CType.UInt16;
            return true;
        }
        else
        {
            type = CType.Void;
            return false;
        }
    }

    void FetchToken()
    {
        NextType = TokenType.INVALID;
        NextInt = 0;
        NextName = null;

        SkipSpaces();

        // Record the location of this token:
        LastTokenPos = TokenPos;
        TokenPos = InputPos;

        // Handle characters in order of ASCII value:

        // (Unprintable characters shouldn't be in the file, and whitespace was already skipped.)
        if (TryRead('\0')) NextType = TokenType.EOF;
        else if (GetNextChar() <= ' ') NextType = TokenType.INVALID;
        else if (TryRead('(')) NextType = TokenType.LPAREN;
        else if (TryRead(')')) NextType = TokenType.RPAREN;
        else if (TryRead('*')) NextType = TokenType.STAR;
        else if (TryRead('+')) NextType = TokenType.PLUS;
        else if (TryRead('-')) NextType = TokenType.MINUS;
        else if (TryRead('/'))
        {
            if (TryRead('/'))
            {
                // This is a single-line comment:
                while (true)
                {
                    char c = GetNextChar();
                    if (c == '\n' || c == '\0') break;
                    FetchChar();
                }

                // Fetch the token following this comment:
                FetchToken();
            }
            else
            {
                NextType = TokenType.SLASH;
            }
        }
        else if (TryRead(';')) NextType = TokenType.SEMICOLON;
        else if (TryRead('=')) NextType = TokenType.EQUALS;
        else if (TryRead('{')) NextType = TokenType.LBRACE;
        else if (TryRead('}')) NextType = TokenType.RBRACE;
        else if (IsNameChar(GetNextChar()))
        {
            // Parse identifiers and numeric literals:
            StringBuilder sb = new StringBuilder();
            while (IsNameChar(GetNextChar()))
            {
                sb.Append(GetNextChar());
                FetchChar();
            }
            string name = sb.ToString();

            if (TryConvertInt(name, out NextInt))
            {
                NextType = TokenType.INT;
            }
            else
            {
                NextType = TokenType.NAME;
                NextName = name;
            }
        }
        else
        {
            ParserError("invalid token");
        }
    }

    static bool IsNameChar(char c)
    {
        return (c == '_') || (c >= '0' && c <= '9') || (c >= 'A' && c < 'Z') || (c >= 'a' && c < 'z');
    }

    void SkipSpaces()
    {
        while (true)
        {
            char c = GetNextChar();
            if (c != ' ' && c != '\t' && c != '\r' && c != '\n') return;
            FetchChar();
        }
    }

    static bool TryConvertInt(string name, out int integer)
    {
        string decimalDigits = "0123456789";

        if (name.Length == 0) Program.Panic("names must not be empty");

        // If the token doesn't start with a digit, it isn't a number.
        integer = 0;
        if (!decimalDigits.Contains(name[0])) return false;

        // TODO: Improve this function.
        bool isHex = false;
        int n = 0;
        foreach (char c in name)
        {
            if (isHex)
            {
                // TODO: Allow lowercase hex digits.
                string hexDigits = "0123456789ABCDEF";
                int index = hexDigits.IndexOf(c);
                if (index >= 0)
                {
                    n = 16 * n + index;
                }
                else
                {
                    return false;
                }
            }
            else
            {
                int index = decimalDigits.IndexOf(c);
                if (index >= 0)
                {
                    n = 10 * n + index;
                }
                else if (c == 'x')
                {
                    // TODO: This will allow any number of digits to appear before
                    // the "x"; hex numbers should really be required to start
                    // as exactly "0x".
                    isHex = true;
                    n = 0;
                }
                else
                {
                    return false;
                }
            }
        }

        integer = n;
        return true;
    }

    char GetNextChar()
    {
        return (Next < Input.Length) ? Input[Next] : '\0';
    }

    int FetchChar()
    {
        if (Next < Input.Length) Next++;
        int c = GetNextChar();
        if (c == '\n')
        {
            InputPos.Line++;
            InputPos.Column = 0;
        }
        else
        {
            InputPos.Column++;
        }
        return c;
    }

    bool TryRead(char c)
    {
        if (GetNextChar() == c)
        {
            FetchChar();
            return true;
        }
        else
        {
            return false;
        }
    }

    void ParserError(string message)
    {
        Program.Error("syntax error: " + message);
    }
}

struct FilePosition
{
    public int Line, Column;
}

enum TokenType
{
    INVALID,
    EOF,

    LPAREN,
    RPAREN,
    STAR,
    PLUS,
    MINUS,
    SLASH,
    EQUALS,
    SEMICOLON,
    LBRACE,
    RBRACE,

    INT,
    NAME,
}
