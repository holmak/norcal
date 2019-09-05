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
                List<Expr> body = new List<Expr>();
                while (!TryParse(TokenType.RBRACE))
                {
                    body.Add(ParseStatement(true));
                }
                d.Body = Expr.MakeScope(Expr.MakeSequence(body.ToArray()));
            }
            else
            {
                d.Tag = DeclarationTag.Variable;
                if (TryParse(TokenType.EQUALS)) ParserError("global variables cannot be initialized");
                if (!TryParse(TokenType.SEMICOLON)) ParserError("expected ;");
            }
        }
        return d;
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
            // Optionally, an initial value can be assigned:
            if (TryParse(TokenType.EQUALS))
            {
                Expr value = ParseExpr();
                stmt = Expr.MakeSequence(new[]
                {
                    Expr.MakeLocal(type, localname),
                    MakeAssignExpr(Expr.MakeName(localname), value),
                });
            }
            else
            {
                stmt = Expr.MakeLocal(type, localname);
            }
            if (!TryParse(TokenType.SEMICOLON)) ParserError("expected ;");
        }
        else if (TryParseName("if"))
        {
            if (!allowLong) Error_NotAllowedInFor();
            if (!TryParse(TokenType.LPAREN)) ParserError("expected (");
            Expr test = ParseExpr();
            test = Expr.MakeCall(Builtins.BoolFromGeneric, test);
            if (!TryParse(TokenType.RPAREN)) ParserError("expected )");
            Expr then = ParseStatementBlock();
            stmt = Expr.MakeSwitch(test, then);
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
            stmt = Expr.MakeFor(init, test, next, body);
        }
        else if (TryParseName("return"))
        {
            if (!allowLong) Error_NotAllowedInFor();
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

    void Error_NotAllowedInFor()
    {
        ParserError("complex statements are not allowed in for initializers");
    }

    Expr ParseStatementBlock()
    {
        // A block can be a single statement, or a series of statements surrounded by braces.
        if (TryParse(TokenType.LBRACE))
        {
            List<Expr> body = new List<Expr>();
            while (!TryParse(TokenType.RBRACE))
            {
                body.Add(ParseStatement(true));
            }
            return Expr.MakeSequence(body.ToArray());
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
            return Expr.MakeInt(n, CType.UInt16);
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
