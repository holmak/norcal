using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

partial class Parser
{
    // Input text:
    List<Token> Input;

    public static List<Declaration> ParseFile(string filename)
    {
        Parser p = new Parser(filename);
        return p.ParseFile();
    }

    Parser(string filename)
    {
        Input = Tokenizer.TokenizeFile(filename);
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
            d.Tag = DeclarationTag.Constant;
            d.Type = ExpectType();
            d.Name = ExpectAnyName();
            Expect(TokenType.EQUAL);
            d.Body = ParseExpr();
            Expect(TokenType.SEMICOLON);
        }
        else
        {
            d.Type = ExpectType();

            if (d.Type.IsStruct && TryParse(TokenType.LBRACE))
            {
                d.Tag = DeclarationTag.Struct;
                d.Name = d.Type.Name;
                d.Type = null;
                d.Fields = new List<NamedField>();
                while (!TryParse(TokenType.RBRACE))
                {
                    string fieldName;
                    CType fieldType;
                    fieldType = ExpectType();
                    fieldName = ExpectAnyName();
                    d.Fields.Add(new NamedField(fieldType, fieldName));
                    while (TryParse(TokenType.COMMA))
                    {
                        fieldName = ExpectAnyName();
                        d.Fields.Add(new NamedField(fieldType, fieldName));
                    }
                    Expect(TokenType.SEMICOLON);
                }
            }
            else
            {
                d.Name = ExpectAnyName();

                if (TryParse(TokenType.LPAREN))
                {
                    d.Tag = DeclarationTag.Function;
                    d.Fields = new List<NamedField>();
                    if (!TryParse(TokenType.RPAREN))
                    {
                        while (true)
                        {
                            CType type;
                            string name;
                            type = ExpectType();
                            name = ExpectAnyName();
                            d.Fields.Add(new NamedField(type, name));
                            if (TryParse(TokenType.RPAREN)) break;
                            Expect(TokenType.COMMA);
                        }
                    }
                    Expect(TokenType.LBRACE);
                    List<object> args = new List<object>();
                    args.Add(Tag.Sequence);
                    while (!TryParse(TokenType.RBRACE))
                    {
                        args.Add(ParseStatement(true));
                    }
                    d.Body = Expr.Make(Tag.Scope, Expr.Make(args.ToArray()));
                }
                else
                {
                    d.Tag = DeclarationTag.Variable;
                    ParseArrayDeclaration(ref d.Type);
                    if (TryParse(TokenType.EQUAL)) ParserError("global variables cannot be initialized");
                    Expect(TokenType.SEMICOLON);
                }
            }
        }
        return d;
    }

    void ParseArrayDeclaration(ref CType type)
    {
        if (TryParse(TokenType.LBRACKET))
        {
            int dimension = ExpectInt();
            Expect(TokenType.RBRACKET);
            type = CType.MakeArray(type, dimension);
        }
    }

    // If false, only allow statements that would fit in a "for" initializer.
    Expr ParseStatement(bool allowLong)
    {
        Expr stmt;
        CType type;
        if (TryParseType(out type))
        {
            // Declare a local variable:
            string localname;
            localname = ExpectAnyName();
            ParseArrayDeclaration(ref type);
            // Optionally, an initial value can be assigned:
            if (TryParse(TokenType.EQUAL))
            {
                Expr value = ParseExpr();
                stmt = Expr.Make(new object[]
                {
                    Tag.Sequence,
                    Expr.Make(Tag.Local, type, localname),
                    MakeAssignExpr(Expr.Make(Tag.Name, localname), value),
                });
            }
            else
            {
                stmt = Expr.Make(Tag.Local, type, localname);
            }
            Expect(TokenType.SEMICOLON);
        }
        else if (TryParseName("if"))
        {
            if (!allowLong) Error_NotAllowedInFor();
            Expect(TokenType.LPAREN);
            Expr test = ParseExpr();
            test = Expr.Make(Tag.BoolFromGeneric, test);
            Expect(TokenType.RPAREN);
            Expr then = ParseStatementBlock();
            stmt = Expr.Make(Tag.Switch, test, then);
        }
        else if (TryParseName("for"))
        {
            if (!allowLong) Error_NotAllowedInFor();
            Expect(TokenType.LPAREN);
            Expr init = ParseStatement(false);
            Expr test = ParseExpr();
            Expect(TokenType.SEMICOLON);
            Expr next = ParseExpr();
            Expect(TokenType.RPAREN);
            Expr body = ParseStatementBlock();
            stmt = Expr.Make(Tag.For, init, test, next, body);
        }
        else if (TryParseName("return"))
        {
            if (!allowLong) Error_NotAllowedInFor();
            stmt = Expr.Make(Tag.Return, ParseExpr());
            Expect(TokenType.SEMICOLON);
        }
        else if (TryParseName("__asm"))
        {
            if (!allowLong) Error_NotAllowedInFor();
            while (TryParse(TokenType.NEWLINE)) { /* Skip any number of newlines. */ }
            Expect(TokenType.LBRACE);
            List<object> args = new List<object>();
            args.Add(Tag.Sequence);
            while (!TryParse(TokenType.RBRACE))
            {
                string mnemonic, variable;
                if (TryParse(TokenType.NEWLINE))
                {
                    // Ignore blank lines.
                }
                else
                {
                    mnemonic = ExpectAnyName();
                    if (TryParse(TokenType.NEWLINE))
                    {
                        args.Add(Expr.Make(Tag.Asm, mnemonic));
                    }
                    else if (TryParse(TokenType.NUMBER_SIGN))
                    {
                        int operand = ExpectInt();
                        Expect(TokenType.NEWLINE);
                        args.Add(Expr.Make(Tag.Asm, mnemonic, operand, Asm.Immediate));
                    }
                    else if (TryParseAnyName(out variable))
                    {
                        int offset = 0;
                        if (TryParse(TokenType.PLUS))
                        {
                            offset = ExpectInt();
                        }
                        Expect(TokenType.NEWLINE);
                        args.Add(Expr.Make(Tag.Asm, mnemonic, Expr.Make(Tag.AsmOperand, variable, offset)));
                    }
                    else
                    {
                        ParserError("expected operand or ;");
                    }
                }
            }
            return Expr.Make(args.ToArray());
        }
        else
        {
            // An expression-statement:
            stmt = ParseExpr();
            Expect(TokenType.SEMICOLON);
        }
        return stmt;
    }

    void Error_NotAllowedInFor()
    {
        ParserError("complex statements are not allowed in for initializers");
    }

    Expr ParseStatementBlock()
    {
        // A block can be a single statement, or a series of statements surrounded by braces.
        if (TryParse(TokenType.LBRACE))
        {
            List<object> args = new List<object>();
            args.Add(Tag.Sequence);
            while (!TryParse(TokenType.RBRACE))
            {
                args.Add(ParseStatement(true));
            }
            return Expr.Make(args.ToArray());
        }
        else
        {
            return ParseStatement(true);
        }
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

    // = *= /= %= += -= <<= >>= &= ^= |=
    Expr ParseAssignExpr()
    {
        Expr e = ParseLogicalOrExpr();
        if (TryParse(TokenType.EQUAL))
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
        Expr e = ParseBitwiseXorExpr();
        while (true)
        {
            if (TryParse(TokenType.PIPE))
            {
                e = Expr.Make(Tag.BitwiseOrGeneric, e, ParseBitwiseXorExpr());
            }
            else
            {
                return e;
            }
        }
    }

    // ^
    Expr ParseBitwiseXorExpr()
    {
        Expr e = ParseBitwiseAndExpr();
        while (true)
        {
            if (TryParse(TokenType.CARET))
            {
                e = Expr.Make(Tag.BitwiseXorGeneric, e, ParseBitwiseAndExpr());
            }
            else
            {
                return e;
            }
        }
    }

    // &
    Expr ParseBitwiseAndExpr()
    {
        Expr e = ParseEqualityExpr();
        while (true)
        {
            if (TryParse(TokenType.AMPERSAND))
            {
                e = Expr.Make(Tag.BitwiseAndGeneric, e, ParseEqualityExpr());
            }
            else
            {
                return e;
            }
        }
    }

    // == !=
    Expr ParseEqualityExpr()
    {
        Expr e = ParseCompareExpr();
        while (true)
        {
            if (TryParse(TokenType.DOUBLE_EQUAL))
            {
                e = Expr.Make(Tag.EqualGeneric, e, ParseCompareExpr());
            }
            else if (TryParse(TokenType.NOT_EQUAL))
            {
                e = Expr.Make(Tag.NotEqualGeneric, e, ParseCompareExpr());
            }
            else
            {
                return e;
            }
        }
    }

    // < > <= >=
    Expr ParseCompareExpr()
    {
        Expr e = ParseShiftExpr();
        while (true)
        {
            if (TryParse(TokenType.LESS_THAN))
            {
                e = Expr.Make(Tag.LessThanGeneric, e, ParseShiftExpr());
            }
            else if (TryParse(TokenType.GREATER_THAN))
            {
                e = Expr.Make(Tag.GreaterThanGeneric, e, ParseShiftExpr());
            }
            else
            {
                return e;
            }
        }
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
                e = Expr.Make(Tag.AddGeneric, e, ParseMultiplyExpr());
            }
            else if (TryParse(TokenType.MINUS))
            {
                e = Expr.Make(Tag.SubtractGeneric, e, ParseMultiplyExpr());
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
        Expr e = ParseCastExpr();
        while (true)
        {
            if (TryParse(TokenType.STAR))
            {
                e = Expr.Make(Tag.MultiplyGeneric, e, ParseCastExpr());
            }
            else if (TryParse(TokenType.SLASH))
            {
                e = Expr.Make(Tag.DivideGeneric, e, ParseCastExpr());
            }
            else if (TryParse(TokenType.PERCENT))
            {
                e = Expr.Make(Tag.ModulusGeneric, e, ParseCastExpr());
            }
            else
            {
                return e;
            }
        }
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
            return Expr.Make(Tag.LoadGeneric, ParseUnaryPrefixExpr());
        }
        else if (TryParse(TokenType.AMPERSAND))
        {
            return Expr.Make(Tag.AddressOf, ParseUnaryPrefixExpr());
        }
        else if (TryParse(TokenType.TILDE))
        {
            return Expr.Make(Tag.BitwiseNotGeneric, ParseUnaryPrefixExpr());
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
                List<object> args = new List<object>();

                string function;
                if (!e.Match(Tag.Name, out function))
                {
                    ParserError("functions may only be called by name");
                }

                args.Add(function);
                if (!TryParse(TokenType.RPAREN))
                {
                    while (true)
                    {
                        args.Add(ParseExpr());
                        if (TryParse(TokenType.RPAREN)) break;
                        Expect(TokenType.COMMA);
                    }
                }

                e = Expr.Make(args.ToArray());
            }
            else if (TryParse(TokenType.PERIOD))
            {
                string fieldName;
                fieldName = ExpectAnyName();
                e = Expr.Make(Tag.LoadGeneric, Expr.Make(Tag.Field, e, fieldName));
            }
            else if (TryParse(TokenType.ARROW))
            {
                string fieldName;
                fieldName = ExpectAnyName();
                e = Expr.Make(Tag.LoadGeneric, Expr.Make(Tag.Field, Expr.Make(Tag.LoadGeneric, e), fieldName));
            }
            else if (TryParse(TokenType.LBRACKET))
            {
                Expr index = ParseExpr();
                e = Expr.Make(Tag.LoadGeneric, Expr.Make(Tag.Index, e, index));
                Expect(TokenType.RBRACKET);
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
            return Expr.Make(Tag.Int, n, CType.UInt16);
        }
        else if (TryParseAnyName(out name))
        {
            return Expr.Make(Tag.Name, name);
        }
        else if (TryParse(TokenType.LPAREN))
        {
            Expr e = ParseExpr();
            Expect(TokenType.RPAREN);
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
        if (e.Match(Tag.LoadGeneric, out inner))
        {
            return inner;
        }
        else
        {
            return Expr.Make(Tag.AddressOf, e);
        }
    }

    Expr MakeAssignExpr(Expr dst, Expr src)
    {
        return Expr.Make(Tag.StoreGeneric, AddressOf(dst), src);
    }

    Token PeekToken()
    {
        return Input[0];
    }

    void ConsumeToken()
    {
        // The final "end of file" token is never removed.
        if (Input[0].Tag != TokenType.EOF)
        {
            Input.RemoveAt(0);
        }
    }

    bool TryParse(TokenType expected)
    {
        Token token = PeekToken();
        if (token.Tag == expected)
        {
            ConsumeToken();
            return true;
        }
        else
        {
            return false;
        }
    }

    void Expect(TokenType expected)
    {
        if (!TryParse(expected)) ParserError("expected {0}", TokenInfo.TokenNames[(int)expected]);
    }

    bool TryParseInt(out int n)
    {
        Token token = PeekToken();
        if (token.Tag == TokenType.INT)
        {
            n = token.Int;
            ConsumeToken();
            return true;
        }
        else
        {
            n = 0;
            return false;
        }
    }

    int ExpectInt()
    {
        int n;
        if (!TryParseInt(out n)) ParserError("expected an integer literal");
        return n;
    }

    bool TryParseAnyName(out string name)
    {
        Token token = PeekToken();
        if (token.Tag == TokenType.NAME)
        {
            name = token.Name;
            ConsumeToken();
            return true;
        }
        else
        {
            name = null;
            return false;
        }
    }

    string ExpectAnyName()
    {
        string name;
        if (!TryParseAnyName(out name)) ParserError("expected an identifier");
        return name;
    }

    bool TryParseName(string name)
    {
        Token token = PeekToken();
        if (token.Tag == TokenType.NAME && token.Name == name)
        {
            ConsumeToken();
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
        else if (TryParseName("uint8_t"))
        {
            type = CType.UInt8;
        }
        else if (TryParseName("uint16_t"))
        {
            type = CType.UInt16;
        }
        else if (TryParseName("struct"))
        {
            string name;
            name = ExpectAnyName();
            type = CType.MakeStruct(name);
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

    CType ExpectType()
    {
        CType type;
        if (!TryParseType(out type)) ParserError("expected a type");
        return type;
    }

    void ParserError(string format, params object[] args) => ParserError(string.Format(format, args));

    void ParserError(string message)
    {
        Token token = PeekToken();
        FilePosition pos = token.Position;
        Program.Error("syntax error (\"{0}\", line {1}, column {2}): {3}", pos.Filename, pos.Line + 1, pos.Column + 1, message);
    }
}
