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
        // TODO: Items with static scope should not be visible outside their file.
        bool isStatic = TryParseName("static");

        Declaration d = new Declaration();
        if (TryParseName("define"))
        {
            d.Kind = DeclarationKind.Constant;
            if (!TryParseType(out d.Type)) ParserError("expected a type");
            if (!TryParseAnyName(out d.Name)) ParserError("expected a name");
            if (!TryParse(TokenType.EQUALS)) ParserError("expected =");
            d.Body = ParseExpr();
            if (!TryParse(TokenType.SEMICOLON)) ParserError("expected ;");
        }
        else if (TryParseName("#define"))
        {
            // A preprocessor-style constant, for backward compatibility.
            d.Kind = DeclarationKind.Constant;
            // TODO: Infer a type for this constant.
            d.Type = CType.UInt16;
            if (!TryParseAnyName(out d.Name)) ParserError("expected a name");
            d.Body = ParseExpr();
        }
        else
        {
            d.Kind = DeclarationKind.Function;
            if (!TryParseType(out d.Type)) ParserError("expected a return type");
            if (!TryParseAnyName(out d.Name)) ParserError("expected function name");
            if (!TryParse(TokenType.LPAREN)) ParserError("expected (");
            d.Fields = new List<NamedField>();
            if (!TryParse(TokenType.RPAREN))
            {
                while (true)
                {
                    CType type;
                    string name;
                    if (!TryParseType(out type)) ParserError("expected type for parameter");
                    if (!TryParseAnyName(out name)) ParserError("expected name for parameter");
                    d.Fields.Add(new NamedField(type, name));
                    if (TryParse(TokenType.RPAREN)) break;
                    if (!TryParse(TokenType.COMMA)) ParserError("expected ,");
                }
            }
            if (!TryParse(TokenType.LBRACE)) ParserError("expected {");
            List<Expr> body = new List<Expr>();
            while (!TryParse(TokenType.RBRACE))
            {
                body.Add(ParseStatement());
            }
            d.Body = Expr.MakeScope(Expr.MakeSequence(body.ToArray()));
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
            // TODO: Include the type in local var declarations.
            stmt = Expr.MakeSequence(new[]
            {
                Expr.MakeLocal(localname),
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
                then = Expr.MakeSequence(body.ToArray());
            }
            else
            {
                then = ParseStatement();
            }
            stmt = Expr.MakeSwitch(test, then);
        }
        else if (TryParseName("return"))
        {
            stmt = Expr.MakeReturn(ParseExpr());
            if (!TryParse(TokenType.SEMICOLON)) ParserError("expected ;");
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
                e = Expr.MakeCall(Builtins.AddGeneric, e, ParseMultiplyExpr());
            }
            else if (TryParse(TokenType.MINUS))
            {
                e = Expr.MakeCall(Builtins.SubtractGeneric, e, ParseMultiplyExpr());
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
            return Expr.MakeCall(Builtins.LoadGeneric, ParseSuffixExpr());
        }
        else
        {
            return ParseSuffixExpr();
        }
    }

    // Suffix operators
    Expr ParseSuffixExpr()
    {
        Expr e = ParsePrimaryExpr();
        while (true)
        {
            if (TryParse(TokenType.LPAREN))
            {
                List<Expr> args = new List<Expr>();
                if (!TryParse(TokenType.RPAREN))
                {
                    while (true)
                    {
                        args.Add(ParseExpr());
                        if (TryParse(TokenType.RPAREN)) break;
                        if (!TryParse(TokenType.COMMA)) ParserError("expected ,");
                    }
                }

                string function;
                if (e.MatchName(out function))
                {
                    e = Expr.MakeCall(function, args);
                }
                else
                {
                    ParserError("functions may only be called by name");
                }
            }
            else
            {
                // No more suffixes.
                break;
            }
        }
        return e;
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

    Expr AddressOf(Expr e)
    {
        Expr inner;
        if (e.MatchUnaryCall(Builtins.LoadGeneric, out inner))
        {
            return inner;
        }
        else
        {
            return Expr.MakeAddressOf(e);
        }
    }

    Expr MakeAssignExpr(Expr dst, Expr src)
    {
        return Expr.MakeCall(Builtins.StoreGeneric, AddressOf(dst), src);
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
        if (TryParseName("void"))
        {
            type = CType.Void;
        }
        else if (TryParseName("uint16_t"))
        {
            type = CType.UInt16;
        }
        else
        {
            type = null;
            return false;
        }

        while (TryParse(TokenType.STAR))
        {
            type = new CType
            {
                Tag = CTypeTag.Pointer,
                Subtype = type,
            };
        }

        return true;
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
        else if (TryRead("#define"))
        {
            NextType = TokenType.NAME;
            NextName = "#define";
        }
        else if (TryRead('#'))
        {
            SkipToNextLine();
        }
        else if (TryRead('(')) NextType = TokenType.LPAREN;
        else if (TryRead(')')) NextType = TokenType.RPAREN;
        else if (TryRead('*')) NextType = TokenType.STAR;
        else if (TryRead('+')) NextType = TokenType.PLUS;
        else if (TryRead(',')) NextType = TokenType.COMMA;
        else if (TryRead('-')) NextType = TokenType.MINUS;
        else if (TryRead('.')) NextType = TokenType.PERIOD;
        else if (TryRead('/'))
        {
            // Skip past single-line comments:
            if (TryRead('/')) SkipToNextLine();
            else NextType = TokenType.SLASH;
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
        return (c == '_') || (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
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

    void SkipToNextLine()
    {
        while (true)
        {
            char c = GetNextChar();
            if (c == '\n' || c == '\0') break;
            FetchChar();
        }

        // Fetch the token following this line:
        FetchToken();
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

    bool TryRead(string s)
    {
        if (SafeSubstring(Input, Next, s.Length) == s)
        {
            Next += s.Length;
            return true;
        }
        return false;
    }

    static string SafeSubstring(string s, int start, int length)
    {
        int maxLength = s.Length - start;
        if (length > maxLength) length = maxLength;
        return s.Substring(start, length);
    }

    void ParserError(string message)
    {
        Program.Error("syntax error (line {0}, column {1}): {2}", TokenPos.Line, TokenPos.Column, message);
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
    COMMA,
    MINUS,
    PERIOD,
    SLASH,
    EQUALS,
    SEMICOLON,
    LBRACE,
    RBRACE,

    INT,
    NAME,
}
