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
    string CurrentFunctionName = "(global)";

    public static List<Expr> ParseFile(string filename)
    {
        Parser p = new Parser(filename);
        return p.ParseFile();
    }

    Parser(string filename)
    {
        Input = Tokenizer.TokenizeFile(filename);
    }

    List<Expr> ParseFile()
    {
        List<Expr> program = new List<Expr>();
        while (!TryParse(TokenType.EOF))
        {
            program.Add(ParseDeclaration());
        }
        return program;
    }

    Expr ParseDeclaration()
    {
        while (TryParseName("static"))
        {
            // TODO: Emit a warning.
        }

        if (TryParseName("define"))
        {
            CType type = ExpectType();
            string name = ExpectAnyName();
            Expect(TokenType.EQUAL);
            Expr body = ParseExpr();
            Expect(TokenType.SEMICOLON);
            return Expr.Make(Tag.Constant, type, name, body);
        }
        else
        {
            MemoryRegion region;
            if (TryParseMemoryRegionQualifier(out region))
            {
                // TODO: If there is a memory region qualifier here, and this is not a global variable
                // declaration, it is an error.
            }

            CType type = ExpectType();

            if (type.IsStruct && TryParse(TokenType.LBRACE))
            {
                List<FieldInfo> fields = new List<FieldInfo>();
                while (!TryParse(TokenType.RBRACE))
                {
                    // Individual fields can't be assigned to a region of memory.
                    region = MemoryRegion.Ram;
                    CType fieldType = ExpectType();
                    string fieldName = ExpectAnyName();
                    fields.Add(new FieldInfo(region, fieldType, fieldName));
                    while (TryParse(TokenType.COMMA))
                    {
                        fieldName = ExpectAnyName();
                        fields.Add(new FieldInfo(region, fieldType, fieldName));
                    }
                    Expect(TokenType.SEMICOLON);
                }

                return Expr.Make(Tag.Struct, type.Name, fields.ToArray());
            }
            else
            {
                string name = ExpectAnyName();

                if (TryParse(TokenType.LPAREN))
                {
                    CurrentFunctionName = name;
                    List<FieldInfo> fields = new List<FieldInfo>();
                    if (!TryParse(TokenType.RPAREN))
                    {
                        while (true)
                        {
                            region = ParseMemoryRegionQualifier();
                            CType fieldType = ExpectType();
                            string fieldName = ExpectAnyName();
                            fields.Add(new FieldInfo(region, fieldType, fieldName));
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
                    Expr body = Expr.Make(args.ToArray());
                    return Expr.Make(Tag.Function, type, name, fields.ToArray(), body);
                }
                else
                {
                    ParseArrayDeclaration(ref type);
                    if (TryParse(TokenType.EQUAL)) ParserError("global variables cannot be initialized");
                    Expect(TokenType.SEMICOLON);
                    return Expr.Make(Tag.Variable, region, type, name);
                }
            }
        }
    }

    bool TryParseMemoryRegionQualifier(out MemoryRegion region)
    {
        if (TryParseName("__zeropage"))
        {
            region = MemoryRegion.ZeroPage;
            return true;
        }
        else if (TryParseName("__location"))
        {
            Expect(TokenType.LPAREN);
            int location = ExpectInt();
            Expect(TokenType.RPAREN);
            region = MemoryRegion.Fixed(location);
            return true;
        }
        else
        {
            region = MemoryRegion.Ram;
            return false;
        }
    }

    MemoryRegion ParseMemoryRegionQualifier()
    {
        MemoryRegion region;
        if (TryParseMemoryRegionQualifier(out region)) return region;
        else return MemoryRegion.Ram;
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
        MemoryRegion region;
        CType type;
        if (TryParseMemoryRegionQualifier(out region))
        {
            // Declare a local variable:
            type = ExpectType();
            stmt = ParseRestOfLocalDeclaration(region, type);
        }
        else if (TryParseType(out type))
        {
            // Declare a local variable:
            region = MemoryRegion.Ram;
            stmt = ParseRestOfLocalDeclaration(region, type);
        }
        else if (TryParseName("if"))
        {
            if (!allowLong) Error_NotAllowedInFor();

            // Create a switch expression with one or more test/body pairs:
            List<object> expr = new List<object>();
            expr.Add(Tag.Switch);

            // Parse the test:
            Expect(TokenType.LPAREN);
            expr.Add(ParseExpr());
            Expect(TokenType.RPAREN);

            // Parse the body:
            expr.Add(ParseStatementBlock());

            // Parse additional else statements:
            while (TryParseName("else"))
            {
                if (TryParseName("if"))
                {
                    // Parse the test:
                    Expect(TokenType.LPAREN);
                    expr.Add(ParseExpr());
                    Expect(TokenType.RPAREN);

                    // Parse the body:
                    expr.Add(ParseStatementBlock());
                }
                else
                {
                    // Create a fake test that is always true:
                    expr.Add(Expr.Make(Tag.Int, 1));

                    // Parse the body:
                    expr.Add(ParseStatementBlock());

                    // An "else" that is not an "else if" means it's time to stop
                    break;
                }
            }

            stmt = Expr.Make(expr.ToArray());
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
        else if (TryParseName("while"))
        {
            if (!allowLong) Error_NotAllowedInFor();
            Expect(TokenType.LPAREN);
            Expr test = ParseExpr();
            Expect(TokenType.RPAREN);
            Expr body = ParseStatementBlock();
            stmt = Expr.Make(Tag.For, Expr.Make(Tag.Empty), test, Expr.Make(Tag.Empty), body);
        }
        else if (TryParseName("continue"))
        {
            if (!allowLong) Error_NotAllowedInFor();
            stmt = Expr.Make(Tag.Continue);
            Expect(TokenType.SEMICOLON);
        }
        else if (TryParseName("break"))
        {
            if (!allowLong) Error_NotAllowedInFor();
            stmt = Expr.Make(Tag.Break);
            Expect(TokenType.SEMICOLON);
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
                if (TryParse(TokenType.NEWLINE))
                {
                    // Ignore blank lines.
                }
                else
                {
                    string symbol = ExpectAnyName();

                    if (TryParse(TokenType.NEWLINE))
                    {
                        args.Add(Expr.MakeAsm(symbol));
                    }
                    else if (TryParse(TokenType.COLON))
                    {
                        Expect(TokenType.NEWLINE);
                        args.Add(Expr.Make(Tag.Label, symbol));
                    }
                    else if (TryParse(TokenType.NUMBER_SIGN))
                    {
                        AsmOperand operand = ParseAssemblyOperand(AddressMode.Immediate);
                        Expect(TokenType.NEWLINE);
                        args.Add(Expr.MakeAsm(symbol, operand));
                    }
                    else if (TryParse(TokenType.LPAREN))
                    {
                        AsmOperand operand = ParseAssemblyOperand(AddressMode.IndirectY);
                        Expect(TokenType.RPAREN);
                        Expect(TokenType.COMMA);
                        ExpectKeyword("Y");
                        Expect(TokenType.NEWLINE);
                        args.Add(Expr.MakeAsm(symbol, operand));
                    }
                    else
                    {
                        AsmOperand operand = ParseAssemblyOperand(AddressMode.Absolute);
                        Expect(TokenType.NEWLINE);
                        args.Add(Expr.MakeAsm(symbol, operand));
                    }
                }
            }
            return Expr.Make(args.ToArray());
        }
        else
        {
            // An expression-statement:
            // (Indicate that the resulting value must be discarded.)
            stmt = Expr.Make(Tag.Drop, ParseExpr());
            Expect(TokenType.SEMICOLON);
        }
        return stmt;
    }

    Expr ParseRestOfLocalDeclaration(MemoryRegion region, CType type)
    {
        Expr stmt;
        string localname = ExpectAnyName();
        ParseArrayDeclaration(ref type);
        // Optionally, an initial value can be assigned:
        if (TryParse(TokenType.EQUAL))
        {
            Expr value = ParseExpr();
            stmt = Expr.Make(
                Tag.Sequence,
                Expr.Make(Tag.Variable, region, type, localname),
                Expr.Make(Tag.Drop, MakeAssignExpr(Expr.Make(Tag.Name, localname), value)));
        }
        else
        {
            stmt = Expr.Make(Tag.Variable, region, type, localname);
        }
        Expect(TokenType.SEMICOLON);
        return stmt;
    }

    AsmOperand ParseAssemblyOperand(AddressMode mode)
    {
        string name;
        int number;
        if (TryParseInt(out number))
        {
            return new AsmOperand(number, mode);
        }
        else if (TryParseAnyName(out name))
        {
            number = 0;
            if (TryParse(TokenType.PLUS))
            {
                number = ExpectInt();
            }
            return new AsmOperand(name, number, mode);
        }
        else
        {
            ParserError("expected an operand");
            return null;
        }
    }

    void Error_NotAllowedInFor()
    {
        ParserError("complex statements are not allowed in for initializers");
    }

    Expr ParseStatementBlock()
    {
        List<object> args = new List<object>();
        args.Add(Tag.Sequence);

        // A block can be a single statement, or a series of statements surrounded by braces:
        if (TryParse(TokenType.LBRACE))
        {
            while (!TryParse(TokenType.RBRACE))
            {
                args.Add(ParseStatement(true));
            }
        }
        else
        {
            args.Add(ParseStatement(true));
        }

        // Add an empty expression at the end to force the type of this sequence expression to be void:
        args.Add(Expr.Make(Tag.Empty));
        return Expr.Make(args.ToArray());

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
        Expr e = ParseConditionalExpr();
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
        Expr e = ParseLogicalOrExpr();
        if (TryParse(TokenType.QUESTION_MARK))
        {
            Expr ifTrue = ParseExpr();
            Expect(TokenType.COLON);
            Expr ifFalse = ParseConditionalExpr();
            e = Expr.Make(Tag.Switch, e, ifTrue, Expr.Make(Tag.Int, 1), ifFalse);
        }

        return e;
    }

    // ||
    Expr ParseLogicalOrExpr()
    {
        Expr e = ParseLogicalAndExpr();
        while (true)
        {
            if (TryParse(TokenType.LOGICAL_OR))
            {
                Expr right = ParseLogicalAndExpr();
                e = Expr.Make(Tag.Switch,
                    e, Expr.Make(Tag.Int, 1),
                    Expr.Make(Tag.Int, 1), right);
            }
            else
            {
                return e;
            }
        }
    }

    // &&
    Expr ParseLogicalAndExpr()
    {
        Expr e = ParseBitwiseOrExpr();
        while (true)
        {
            if (TryParse(TokenType.LOGICAL_AND))
            {
                Expr right = ParseBitwiseOrExpr();
                e = Expr.Make(Tag.Switch,
                    Expr.Make(Tag.LogicalNot, e), Expr.Make(Tag.Int, 0),
                    Expr.Make(Tag.Int, 1), right);
            }
            else
            {
                return e;
            }
        }
    }

    // |
    Expr ParseBitwiseOrExpr()
    {
        Expr e = ParseBitwiseXorExpr();
        while (true)
        {
            if (TryParse(TokenType.PIPE))
            {
                e = Expr.Make(Tag.BitwiseOr, e, ParseBitwiseXorExpr());
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
                e = Expr.Make(Tag.BitwiseXor, e, ParseBitwiseAndExpr());
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
                e = Expr.Make(Tag.BitwiseAnd, e, ParseEqualityExpr());
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
                e = Expr.Make(Tag.Equal, e, ParseCompareExpr());
            }
            else if (TryParse(TokenType.NOT_EQUAL))
            {
                e = Expr.Make(Tag.NotEqual, e, ParseCompareExpr());
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
                e = Expr.Make(Tag.LessThan, e, ParseShiftExpr());
            }
            else if (TryParse(TokenType.LESS_THAN_OR_EQUAL))
            {
                e = Expr.Make(Tag.LessThanOrEqual, e, ParseShiftExpr());
            }
            else if (TryParse(TokenType.GREATER_THAN))
            {
                e = Expr.Make(Tag.GreaterThan, e, ParseShiftExpr());
            }
            else if (TryParse(TokenType.GREATER_THAN_OR_EQUAL))
            {
                e = Expr.Make(Tag.GreaterThanOrEqual, e, ParseShiftExpr());
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
        Expr e = ParseAddExpr();
        while (true)
        {
            if (TryParse(TokenType.SHIFT_LEFT))
            {
                e = Expr.Make(Tag.ShiftLeft, e, ParseAddExpr());
            }
            else if (TryParse(TokenType.SHIFT_RIGHT))
            {
                e = Expr.Make(Tag.ShiftRight, e, ParseAddExpr());
            }
            else
            {
                return e;
            }
        }
    }

    // + -
    Expr ParseAddExpr()
    {
        Expr e = ParseMultiplyExpr();
        while (true)
        {
            if (TryParse(TokenType.PLUS))
            {
                e = Expr.Make(Tag.Add, e, ParseMultiplyExpr());
            }
            else if (TryParse(TokenType.MINUS))
            {
                e = Expr.Make(Tag.Subtract, e, ParseMultiplyExpr());
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
                e = Expr.Make(Tag.Multiply, e, ParseCastExpr());
            }
            else if (TryParse(TokenType.SLASH))
            {
                e = Expr.Make(Tag.Divide, e, ParseCastExpr());
            }
            else if (TryParse(TokenType.PERCENT))
            {
                e = Expr.Make(Tag.Modulus, e, ParseCastExpr());
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
            return Expr.Make(Tag.Load, ParseUnaryPrefixExpr());
        }
        else if (TryParse(TokenType.AMPERSAND))
        {
            return Expr.Make(Tag.AddressOf, ParseUnaryPrefixExpr());
        }
        else if (TryParse(TokenType.TILDE))
        {
            return Expr.Make(Tag.BitwiseNot, ParseUnaryPrefixExpr());
        }
        else if (TryParse(TokenType.LOGICAL_NOT))
        {
            return Expr.Make(Tag.LogicalNot, ParseUnaryPrefixExpr());
        }
        else if (TryParse(TokenType.INCREMENT))
        {
            return Expr.Make(Tag.Preincrement, AddressOf(ParseUnaryPrefixExpr()));
        }
        else if (TryParse(TokenType.DECREMENT))
        {
            return Expr.Make(Tag.Predecrement, AddressOf(ParseUnaryPrefixExpr()));
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
                e = Expr.Make(Tag.Load, Expr.Make(Tag.Field, e, fieldName));
            }
            else if (TryParse(TokenType.ARROW))
            {
                string fieldName;
                fieldName = ExpectAnyName();
                e = Expr.Make(Tag.Load, Expr.Make(Tag.Field, Expr.Make(Tag.Load, e), fieldName));
            }
            else if (TryParse(TokenType.INCREMENT))
            {
                e = Expr.Make(Tag.Postincrement, AddressOf(e));
            }
            else if (TryParse(TokenType.DECREMENT))
            {
                e = Expr.Make(Tag.Postdecrement, AddressOf(e));
            }
            else if (TryParse(TokenType.LBRACKET))
            {
                Expr index = ParseExpr();
                e = Expr.Make(Tag.Load, Expr.Make(Tag.Index, e, index));
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
            return Expr.Make(Tag.Int, n);
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
        if (e.Match(Tag.Load, out inner))
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
        return Expr.Make(Tag.Store, dst, src);
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

    void ExpectKeyword(string name)
    {
        if (!TryParseName(name)) ParserError("expected '{0}'", name);
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
