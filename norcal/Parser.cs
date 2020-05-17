using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

partial class Parser
{
    List<Token> Input;
    List<string> UndefinedLabels = new List<string>();
    FilePosition SourcePosition = FilePosition.Unknown;
    int NextStringID = 0;

    public static Expr ParseFiles(IEnumerable<string> filenames)
    {
        Parser p = new Parser(filenames);
        return p.ParseAll();
    }

    Parser(IEnumerable<string> filenames)
    {
        // Lex all the input files:
        Input = new List<Token>();
        foreach (string filename in filenames)
        {
            Input.AddRange(Tokenizer.TokenizeFile(filename));
        }
        Input.Add(new Token
        {
            Tag = TokenType.EOF,
        });
    }

    Expr Make(params object[] args)
    {
        return Make(Expr.Make(args));
    }

    Expr Make(Expr e)
    {
        return e.WithSource(SourcePosition);
    }

    Expr MakeSequence(IEnumerable<Expr> items)
    {
        List<object> list = new List<object>();
        list.Add(Tag.Sequence);
        list.AddRange(items);
        return Expr.Make(list.ToArray()).WithSource(SourcePosition);
    }

    Expr ParseAll()
    {
        List<Expr> declarations = new List<Expr>();
        while (!TryParse(TokenType.EOF))
        {
            declarations.Add(ParseDeclaration());
        }
        return MakeSequence(declarations);
    }

    Expr ParseDeclaration()
    {
        while (TryParseName("static"))
        {
            Program.Warning(SourcePosition, "warning: all input is treated as a single translation unit, so 'static' has no effect");
        }

        if (TryParseName("define"))
        {
            CType type = ExpectType();
            string name = ExpectAnyName();
            Expect(TokenType.EQUAL);
            Expr value = ParseExpr();
            Expect(TokenType.SEMICOLON);
            return Make(Tag.Constant, type, name, value);
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

            if (type.IsStructOrUnion && TryParse(TokenType.LBRACE))
            {
                List<FieldInfo> fields = new List<FieldInfo>();
                while (!TryParse(TokenType.RBRACE))
                {
                    CType fieldType = ExpectType();
                    string fieldName = ExpectAnyName();
                    ParseArrayDeclaration(ref fieldType);
                    fields.Add(new FieldInfo(fieldType, fieldName, 0));
                    while (TryParse(TokenType.COMMA))
                    {
                        fieldName = ExpectAnyName();
                        fields.Add(new FieldInfo(fieldType, fieldName, 0));
                    }
                    Expect(TokenType.SEMICOLON);
                }
                Expect(TokenType.SEMICOLON);

                string tag = (type.Tag == CTypeTag.Struct) ? Tag.Struct : Tag.Union;
                return Make(tag, type.Name, fields.ToArray());
            }
            else
            {
                string name = ExpectAnyName();

                if (TryParse(TokenType.LPAREN))
                {
                    // Labels can be referenced before they are defined, but they must be declared by the end
                    // of the function in which they are referenced.
                    if (UndefinedLabels.Count > 0)
                    {
                        ParserError("label not defined: {0}", UndefinedLabels[0]);
                    }

                    List<FieldInfo> fields = new List<FieldInfo>();
                    if (!TryParse(TokenType.RPAREN))
                    {
                        while (true)
                        {
                            CType fieldType = ExpectType();
                            string fieldName = ExpectAnyName();
                            fields.Add(new FieldInfo(fieldType, fieldName, 0));
                            if (TryParse(TokenType.RPAREN)) break;
                            Expect(TokenType.COMMA);
                        }
                    }

                    List<Expr> statements = new List<Expr>();
                    Expect(TokenType.LBRACE);
                    while (!TryParse(TokenType.RBRACE))
                    {
                        statements.Add(ParseStatement(true));
                    }

                    return Make(Tag.Function, type, name, fields.ToArray(), MakeSequence(statements));
                }
                else
                {
                    ParseArrayDeclaration(ref type);
                    if (TryParse(TokenType.EQUAL))
                    {
                        if (region.Tag == MemoryRegionTag.ProgramRom)
                        {
                            List<int> values = new List<int>();
                            if (type.IsArray)
                            {
                                // Parse a comma separated list; a final comma is allowed, but not required.
                                Expect(TokenType.LBRACE);
                                if (!TryParse(TokenType.RBRACE))
                                {
                                    while (true)
                                    {
                                        int n = ExpectInt();
                                        values.Add(n);
                                        if (TryParse(TokenType.COMMA))
                                        {
                                            if (TryParse(TokenType.RBRACE)) break;
                                        }
                                        else
                                        {
                                            Expect(TokenType.RBRACE);
                                            break;
                                        }
                                    }
                                }
                            }
                            else
                            {
                                values.Add(ExpectInt());
                            }

                            Expect(TokenType.SEMICOLON);
                            return Make(Tag.ReadonlyData, type, name, values.ToArray());
                        }
                        else
                        {
                            ParserError("global variables cannot be initialized");
                            return null;
                        }
                    }
                    else
                    {
                        Expect(TokenType.SEMICOLON);
                        return Make(Tag.Variable, region, type, name);
                    }
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
        else if (TryParseName("__oam"))
        {
            region = MemoryRegion.Oam;
            return true;
        }
        else if (TryParseName("__ram"))
        {
            region = MemoryRegion.Ram;
            return true;
        }
        else if (TryParseName("__prg_rom"))
        {
            region = MemoryRegion.ProgramRom;
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
            Expr dimension;
            if (TryParse(TokenType.RBRACKET))
            {
                dimension = Expr.Make(Tag.Empty);
            }
            else
            {
                dimension = ParseExpr();
                Expect(TokenType.RBRACKET);
            }
            type = CType.MakeArray(type, dimension);
        }
    }

    /// <summary>
    /// If false, only allow statements that would fit in a "for" initializer.
    /// </summary>
    Expr ParseStatement(bool allowLong)
    {
        CType type;
        if (TryParseType(out type))
        {
            // Declare a local variable:
            return ParseRestOfLocalDeclaration(type);
        }
        else if (TryParseName("if"))
        {
            if (!allowLong) Error_NotAllowedInFor();

            List<object> parts = new List<object>();
            parts.Add(Tag.If);

            Expect(TokenType.LPAREN);
            parts.Add(ParseExpr());
            Expect(TokenType.RPAREN);
            parts.Add(ParseStatementBlock());

            // Parse additional else statements:
            while (TryParseName("else"))
            {
                if (TryParseName("if"))
                {
                    Expect(TokenType.LPAREN);
                    parts.Add(ParseExpr());
                    Expect(TokenType.RPAREN);
                    parts.Add(ParseStatementBlock());
                }
                else
                {
                    parts.Add(Make(Tag.Integer, 1));
                    parts.Add(ParseStatementBlock());

                    // An "else" that is not an "else if" means it's time to stop:
                    break;
                }
            }

            return Make(parts.ToArray());
        }
        else if (TryParseName("for"))
        {
            if (!allowLong) Error_NotAllowedInFor();

            Expect(TokenType.LPAREN);
            Expr init = ParseStatement(false);
            Expr test = ParseExpr();
            Expect(TokenType.SEMICOLON);
            Expr induct = ParseExpr();
            Expect(TokenType.RPAREN);
            Expr body = ParseStatementBlock();
            return Make(Tag.For, init, test, induct, body);
        }
        else if (TryParseName("while"))
        {
            if (!allowLong) Error_NotAllowedInFor();

            Expect(TokenType.LPAREN);
            Expr test = ParseExpr();
            Expect(TokenType.RPAREN);
            Expr body = ParseStatementBlock();
            return Make(Tag.For, Make(Tag.Empty), test, Make(Tag.Empty), body);
        }
        else if (TryParseName("continue"))
        {
            if (!allowLong) Error_NotAllowedInFor();
            Expect(TokenType.SEMICOLON);
            return Make(Tag.Continue);
        }
        else if (TryParseName("break"))
        {
            if (!allowLong) Error_NotAllowedInFor();
            Expect(TokenType.SEMICOLON);
            return Make(Tag.Break);
        }
        else if (TryParseName("return"))
        {
            if (!allowLong) Error_NotAllowedInFor();

            if (TryParse(TokenType.SEMICOLON))
            {
                return Make(Tag.Return);
            }
            else
            {
                Expr result = ParseExpr();
                Expect(TokenType.SEMICOLON);
                return Make(Tag.Return, result);
            }
        }
        else if (TryParseName("goto"))
        {
            string label = ExpectAnyName();
            Expect(TokenType.SEMICOLON);
            return Make(Tag.Jump, label);
        }
        else if (TryParseName("__asm"))
        {
            if (!allowLong) Error_NotAllowedInFor();

            while (TryParse(TokenType.NEWLINE)) { /* Skip any number of newlines. */ }
            Expect(TokenType.LBRACE);
            List<Expr> parts = new List<Expr>();
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
                        parts.Add(Expr.MakeAsm(symbol));
                    }
                    else if (TryParse(TokenType.COLON))
                    {
                        Expect(TokenType.NEWLINE);
                        parts.Add(Expr.Make(Tag.Label, symbol));
                    }
                    else if (TryParse(TokenType.NUMBER_SIGN))
                    {
                        AsmOperand operand = ParseAssemblyOperand(AddressMode.Immediate);
                        Expect(TokenType.NEWLINE);
                        parts.Add(Expr.MakeAsm(symbol, operand));
                    }
                    else if (TryParse(TokenType.PLUS))
                    {
                        AsmOperand operand = ParseAssemblyOperand(AddressMode.Relative);
                        Expect(TokenType.NEWLINE);
                        parts.Add(Expr.MakeAsm(symbol, operand));
                    }
                    else if (TryParse(TokenType.LPAREN))
                    {
                        AsmOperand operand = ParseAssemblyOperand(AddressMode.Indirect);
                        if (TryParse(TokenType.RPAREN))
                        {
                            if (TryParse(TokenType.COMMA))
                            {
                                Expect(TokenType.COMMA);
                                ExpectKeyword("Y");
                                operand = operand.WithMode(AddressMode.IndirectY);
                            }
                            else
                            {
                                // This is the non-indexed "indirect" address mode.
                            }
                        }
                        else if (TryParse(TokenType.COMMA))
                        {
                            ExpectKeyword("X");
                            Expect(TokenType.RPAREN);
                            operand = operand.WithMode(AddressMode.IndirectX);
                        }
                        else
                        {
                            ParserError("expected (zp,X) or (zp),Y operand");
                        }
                        Expect(TokenType.NEWLINE);
                        parts.Add(Expr.MakeAsm(symbol, operand));
                    }
                    else
                    {
                        AsmOperand operand = ParseAssemblyOperand(AddressMode.Absolute);
                        if (TryParse(TokenType.COMMA))
                        {
                            ExpectKeyword("X");
                            operand = operand.WithMode(AddressMode.AbsoluteX);
                        }
                        Expect(TokenType.NEWLINE);
                        parts.Add(Expr.MakeAsm(symbol, operand));
                    }
                }
            }
            return MakeSequence(parts);
        }
        else if (Input.Count >= 2 && Input[0].Tag == TokenType.NAME && Input[1].Tag == TokenType.COLON)
        {
            string label = ExpectAnyName();
            Expect(TokenType.COLON);
            return Make(Tag.Label, label);
        }
        else
        {
            // An expression-statement:
            Expr e = ParseExpr();
            Expect(TokenType.SEMICOLON);
            return e;
        }
    }

    Expr ParseRestOfLocalDeclaration(CType type)
    {
        string name = ExpectAnyName();
        ParseArrayDeclaration(ref type);
        Expr e = Make(Tag.Variable, type, name);
        // Optionally, assign an initial value:
        if (TryParse(TokenType.EQUAL))
        {
            Expr init = ParseExpr();
            e = Make(Tag.Sequence, e, Make(Tag.Assign, Expr.Make(Tag.Name, name), init));
        }
        Expect(TokenType.SEMICOLON);
        return e;
    }

    AsmOperand ParseAssemblyOperand(AddressMode mode)
    {
        ImmediateModifier modifier = ImmediateModifier.None;
        if (TryParse(TokenType.LESS_THAN)) modifier = ImmediateModifier.LowByte;
        else if (TryParse(TokenType.GREATER_THAN)) modifier = ImmediateModifier.HighByte;

        string name;
        int number;
        if (TryParseInt(out number))
        {
            return new AsmOperand(Maybe.Nothing, number, mode, modifier);
        }
        else if (TryParseAnyName(out name))
        {
            number = 0;
            if (TryParse(TokenType.PLUS))
            {
                number = ExpectInt();
            }
            return new AsmOperand(name, number, mode, modifier);
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
        // A block can be a single statement, or a series of statements surrounded by braces:
        if (TryParse(TokenType.LBRACE))
        {
            List<Expr> parts = new List<Expr>();
            while (!TryParse(TokenType.RBRACE))
            {
                parts.Add(ParseStatement(true));
            }
            return MakeSequence(parts);
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
        Dictionary<TokenType, string> modifyAssignOperators = new Dictionary<TokenType, string>
        {
            { TokenType.PLUS_EQUALS, Tag.Add },
            { TokenType.MINUS_EQUALS, Tag.Subtract },
            { TokenType.STAR_EQUALS, Tag.Multiply },
            { TokenType.SLASH_EQUALS, Tag.Divide },
            { TokenType.PERCENT_EQUALS, Tag.Modulus },
            { TokenType.SHIFT_LEFT_EQUALS, Tag.ShiftLeft },
            { TokenType.SHIFT_RIGHT_EQUALS, Tag.ShiftRight },
            { TokenType.PIPE_EQUALS, Tag.BitwiseOr },
            { TokenType.AMPERSAND_EQUALS, Tag.BitwiseAnd },
            { TokenType.CARET_EQUALS, Tag.BitwiseXor },
        };

        Expr left = ParseConditionalExpr();
        string op;
        if (TryParse(TokenType.EQUAL))
        {
            return Make(Tag.Assign, left, ParseAssignExpr());
        }
        else if (modifyAssignOperators.TryGetValue(PeekToken().Tag, out op))
        {
            ConsumeToken();
            return Make(Tag.AssignModify, op, left, ParseAssignExpr());
        }
        else
        {
            return left;
        }
    }

    // ? :
    Expr ParseConditionalExpr()
    {
        Expr e = ParseLogicalOrExpr();
        if (TryParse(TokenType.QUESTION_MARK))
        {
            Expr trueCase = ParseExpr();
            Expect(TokenType.COLON);
            Expr falseCase = ParseConditionalExpr();
            e = Make(Tag.Conditional, e, trueCase, falseCase);
        }
        return e;
    }

    // ||
    Expr ParseLogicalOrExpr()
    {
        Expr e = ParseLogicalAndExpr();
        while (TryParse(TokenType.LOGICAL_OR))
        {
            Expr right = ParseLogicalAndExpr();
            e = Make(Tag.LogicalOr, e, right);
        }
        return e;
    }

    // &&
    Expr ParseLogicalAndExpr()
    {
        Expr e = ParseBitwiseOrExpr();
        while (TryParse(TokenType.LOGICAL_AND))
        {
            Expr right = ParseBitwiseOrExpr();
            e = Make(Tag.LogicalAnd, e, right);
        }
        return e;
    }

    // |
    Expr ParseBitwiseOrExpr()
    {
        Expr e = ParseBitwiseXorExpr();
        while (TryParse(TokenType.PIPE))
        {
            Expr right = ParseBitwiseXorExpr();
            e = Make(Tag.BitwiseOr, e, right);
        }
        return e;
    }

    // ^
    Expr ParseBitwiseXorExpr()
    {
        Expr e = ParseBitwiseAndExpr();
        while (TryParse(TokenType.CARET))
        {
            Expr right = ParseBitwiseAndExpr();
            e = Make(Tag.BitwiseXor, e, right);
        }
        return e;
    }

    // &
    Expr ParseBitwiseAndExpr()
    {
        Expr e = ParseEqualityExpr();
        while (TryParse(TokenType.AMPERSAND))
        {
            Expr right = ParseEqualityExpr();
            e = Make(Tag.BitwiseAnd, e, right);
        }
        return e;
    }

    // == !=
    Expr ParseEqualityExpr()
    {
        Dictionary<TokenType, string> operators = new Dictionary<TokenType, string>
        {
            { TokenType.DOUBLE_EQUAL, Tag.Equal },
            { TokenType.NOT_EQUAL, Tag.NotEqual },
        };

        return ParseInfixOperators(ParseCompareExpr, operators);
    }

    // < > <= >=
    Expr ParseCompareExpr()
    {
        Dictionary<TokenType, string> operators = new Dictionary<TokenType, string>
        {
            { TokenType.LESS_THAN, Tag.LessThan },
            { TokenType.LESS_THAN_OR_EQUAL, Tag.LessThanOrEqual },
            { TokenType.GREATER_THAN, Tag.GreaterThan },
            { TokenType.GREATER_THAN_OR_EQUAL, Tag.GreaterThanOrEqual },
        };

        return ParseInfixOperators(ParseShiftExpr, operators);
    }

    // << >>
    Expr ParseShiftExpr()
    {
        Dictionary<TokenType, string> operators = new Dictionary<TokenType, string>
        {
            { TokenType.SHIFT_LEFT, Tag.ShiftLeft },
            { TokenType.SHIFT_RIGHT, Tag.ShiftRight },
        };

        return ParseInfixOperators(ParseAddExpr, operators);
    }

    // + -
    Expr ParseAddExpr()
    {
        Dictionary<TokenType, string> operators = new Dictionary<TokenType, string>
        {
            { TokenType.PLUS, Tag.Add },
            { TokenType.MINUS, Tag.Subtract },
        };

        return ParseInfixOperators(ParseMultiplyExpr, operators);
    }

    // * / %
    Expr ParseMultiplyExpr()
    {
        Dictionary<TokenType, string> operators = new Dictionary<TokenType, string>
        {
            { TokenType.STAR, Tag.Multiply },
            { TokenType.SLASH, Tag.Divide },
            { TokenType.PERCENT, Tag.Modulus },
        };

        return ParseInfixOperators(ParseCastExpr, operators);
    }

    // (casts)
    Expr ParseCastExpr()
    {
        return ParseUnaryPrefixExpr();
    }

    // Unary prefix operators
    Expr ParseUnaryPrefixExpr()
    {
        Dictionary<TokenType, string> prefixes = new Dictionary<TokenType, string>
        {
            { TokenType.STAR, Tag.Load },
            { TokenType.AMPERSAND, Tag.AddressOf },
            { TokenType.TILDE, Tag.BitwiseNot },
            { TokenType.LOGICAL_NOT, Tag.LogicalNot },
            { TokenType.INCREMENT, Tag.PreIncrement },
            { TokenType.DECREMENT, Tag.PreDecrement },
        };

        TokenType nextToken = PeekToken().Tag;
        string op;
        if (prefixes.TryGetValue(nextToken, out op))
        {
            ConsumeToken();
            Expr e = ParseUnaryPrefixExpr();
            return Make(op, e);
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
                List<object> parts = new List<object>();
                parts.Add(Tag.Call);
                parts.Add(e);

                if (!TryParse(TokenType.RPAREN))
                {
                    while (true)
                    {
                        parts.Add(ParseExpr());
                        if (TryParse(TokenType.RPAREN)) break;
                        Expect(TokenType.COMMA);
                    }
                }

                e = Expr.Make(parts.ToArray());
            }
            else if (TryParse(TokenType.PERIOD))
            {
                string fieldName = ExpectAnyName();
                e = Make(Tag.Field, e, fieldName);
            }
            else if (TryParse(TokenType.ARROW))
            {
                string fieldName = ExpectAnyName();
                e = Make(Tag.Field, Make(Tag.Load, e), fieldName);
            }
            else if (TryParse(TokenType.INCREMENT))
            {
                e = Make(Tag.PostIncrement, e);
            }
            else if (TryParse(TokenType.DECREMENT))
            {
                e = Make(Tag.PostDecrement, e);
            }
            else if (TryParse(TokenType.LBRACKET))
            {
                Expr index = ParseExpr();
                Expect(TokenType.RBRACKET);
                e = Make(Tag.Index, e, index);
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
        string name, s;
        if (TryParseInt(out n))
        {
            return Make(Tag.Integer, n);
        }
        else if (TryParseString(out s))
        {
            // Generate a readonly array of data and use it immediately.
            name = string.Format("$string{0}", NextStringID++);

            int[] values = new int[s.Length + 1];
            for (int i = 0; i < s.Length; i++)
            {
                int c = s[i];
                if (c > 127) ParserError("strings can only contain ASCII characters");
                values[i] = c;
            }
            values[s.Length] = '\0';

            return Make(Tag.Sequence,
                Make(Tag.ReadonlyData, CType.MakeArray(CType.UInt8, s.Length + 1), name, values),
                Make(Tag.Name, name));
        }
        else if (TryParseAnyName(out name))
        {
            return Make(Tag.Name, name);
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

    Expr ParseInfixOperators(Func<Expr> parseSubexpression, Dictionary<TokenType, string> operators)
    {
        Expr e = parseSubexpression();
        string op;
        while (operators.TryGetValue(PeekToken().Tag, out op))
        {
            ConsumeToken();
            Expr right = parseSubexpression();
            e = Make(op, e, right);
        }
        return e;
    }

    Token PeekToken()
    {
        return Input[0];
    }

    void ConsumeToken()
    {
        SourcePosition = Input[0].Position;

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

    bool TryParseString(out string s)
    {
        Token token = PeekToken();
        if (token.Tag == TokenType.STRING)
        {
            s = token.Name;
            ConsumeToken();
            return true;
        }
        else
        {
            s = null;
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
        else if (TryParseName("uint8_t") || TryParseName("u8") || TryParseName("char") || TryParseName("bool"))
        {
            type = CType.UInt8;
        }
        else if (TryParseName("uint16_t") || TryParseName("u16"))
        {
            type = CType.UInt16;
        }
        else if (TryParseName("struct"))
        {
            string name = ExpectAnyName();
            type = CType.MakeStruct(name);
        }
        else if (TryParseName("union"))
        {
            string name = ExpectAnyName();
            type = CType.MakeUnion(name);
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

    [DebuggerStepThrough]
    void ParserError(string format, params object[] args)
    {
        Program.Error(SourcePosition, format, args);
    }
}
