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
            if (!TryParseType(out d.Type)) ParserError("expected a type");
            if (!TryParseAnyName(out d.Name)) ParserError("expected a name");
            if (!TryParse(TokenType.EQUALS)) ParserError("expected =");
            d.Body = ParseExpr();
            if (!TryParse(TokenType.SEMICOLON)) ParserError("expected ;");
        }
        else if (TryParseName("#define"))
        {
            // A preprocessor-style constant, for backward compatibility.
            d.Tag = DeclarationTag.Constant;
            // TODO: Infer a type for this constant.
            d.Type = CType.UInt16;
            if (!TryParseAnyName(out d.Name)) ParserError("expected a name");
            d.Body = ParseExpr();
        }
        else if (TryParseName("struct"))
        {
            d.Tag = DeclarationTag.Struct;
            if (!TryParseAnyName(out d.Name)) ParserError("expected a name");
            if (!TryParse(TokenType.LBRACE)) ParserError("expected {");
            d.Fields = new List<NamedField>();
            while (!TryParse(TokenType.RBRACE))
            {
                string fieldName;
                CType fieldType;
                if (!TryParseType(out fieldType)) ParserError("expected a type");
                if (!TryParseAnyName(out fieldName)) ParserError("expected a name");
                d.Fields.Add(new NamedField(fieldType, fieldName));
                while (TryParse(TokenType.COMMA))
                {
                    if (!TryParseAnyName(out fieldName)) ParserError("expected a name");
                    d.Fields.Add(new NamedField(fieldType, fieldName));
                }
                if (!TryParse(TokenType.SEMICOLON)) ParserError("expected ;");
            }
        }
        else
        {
            if (!TryParseType(out d.Type)) ParserError("expected a type");
            if (!TryParseAnyName(out d.Name)) ParserError("expected a name");

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
                        if (!TryParseType(out type)) ParserError("expected type for parameter");
                        if (!TryParseAnyName(out name)) ParserError("expected name for parameter");
                        d.Fields.Add(new NamedField(type, name));
                        if (TryParse(TokenType.RPAREN)) break;
                        if (!TryParse(TokenType.COMMA)) ParserError("expected ,");
                    }
                }
                if (!TryParse(TokenType.LBRACE)) ParserError("expected {");
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
                if (TryParse(TokenType.EQUALS)) ParserError("global variables cannot be initialized");
                if (!TryParse(TokenType.SEMICOLON)) ParserError("expected ;");
            }
        }
        return d;
    }

    void ParseArrayDeclaration(ref CType type)
    {
        if (TryParse(TokenType.LBRACKET))
        {
            int dimension;
            if (!TryParseInt(out dimension)) ParserError("expected array size");
            if (!TryParse(TokenType.RBRACKET)) ParserError("expected ]");
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
            if (!TryParseAnyName(out localname)) ParserError("expected variable name");
            ParseArrayDeclaration(ref type);
            // Optionally, an initial value can be assigned:
            if (TryParse(TokenType.EQUALS))
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
            if (!TryParse(TokenType.SEMICOLON)) ParserError("expected ;");
        }
        else if (TryParseName("if"))
        {
            if (!allowLong) Error_NotAllowedInFor();
            if (!TryParse(TokenType.LPAREN)) ParserError("expected (");
            Expr test = ParseExpr();
            test = Expr.Make(Tag.BoolFromGeneric, test);
            if (!TryParse(TokenType.RPAREN)) ParserError("expected )");
            Expr then = ParseStatementBlock();
            stmt = Expr.Make(Tag.Switch, test, then);
        }
        else if (TryParseName("for"))
        {
            if (!allowLong) Error_NotAllowedInFor();
            if (!TryParse(TokenType.LPAREN)) ParserError("expected (");
            Expr init = ParseStatement(false);
            Expr test = ParseExpr();
            if (!TryParse(TokenType.SEMICOLON)) ParserError("expected ;");
            Expr next = ParseExpr();
            if (!TryParse(TokenType.RPAREN)) ParserError("expected )");
            Expr body = ParseStatementBlock();
            stmt = Expr.Make(Tag.For, init, test, next, body);
        }
        else if (TryParseName("return"))
        {
            if (!allowLong) Error_NotAllowedInFor();
            stmt = Expr.Make(Tag.Return, ParseExpr());
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
            return Expr.Make(Tag.LoadGeneric, ParseUnaryPrefixExpr());
        }
        else if (TryParse(TokenType.AMPERSAND))
        {
            return Expr.Make(Tag.AddressOf, ParseUnaryPrefixExpr());
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
                        if (!TryParse(TokenType.COMMA)) ParserError("expected ,");
                    }
                }

                e = Expr.Make(args.ToArray());
            }
            else if (TryParse(TokenType.PERIOD))
            {
                string fieldName;
                if (!TryParseAnyName(out fieldName)) ParserError("expected a field name");
                e = Expr.Make(Tag.LoadGeneric, Expr.Make(Tag.Field, e, fieldName));
            }
            else if (TryParse(TokenType.ARROW))
            {
                string fieldName;
                if (!TryParseAnyName(out fieldName)) ParserError("expected a field name");
                e = Expr.Make(Tag.LoadGeneric, Expr.Make(Tag.Field, Expr.Make(Tag.LoadGeneric, e), fieldName));
            }
            else if (TryParse(TokenType.LBRACKET))
            {
                Expr index = ParseExpr();
                e = Expr.Make(Tag.LoadGeneric, Expr.Make(Tag.Index, e, index));
                if (!TryParse(TokenType.RBRACKET)) ParserError("expected ]");
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
            if (!TryParseAnyName(out name)) ParserError("expected a name");
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

    void ParserError(string message)
    {
        Token token = PeekToken();
        FilePosition pos = token.Position;
        Program.Error("syntax error (line {0}, column {1}): {2}", pos.Line, pos.Column, message);
    }
}
