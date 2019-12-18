using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

partial class Parser
{
    List<Token> Input;
    List<Expr> StackCode = new List<Expr>();
    List<LexicalScope> Scopes = new List<LexicalScope>();
    LoopScope Loop = null;
    List<string> UndefinedLabels = new List<string>();

    public static IReadOnlyList<Expr> ParseFiles(IEnumerable<string> filenames)
    {
        Parser p = new Parser(filenames);
        p.ParseAll();
        return p.StackCode;
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

        Scopes.Add(new LexicalScope("<global>"));
    }

    void Emit(params object[] args)
    {
        Emit(Expr.Make(args));
    }

    void Emit(Expr e)
    {
        StackCode.Add(e);
    }

    void ParseAll()
    {
        while (!TryParse(TokenType.EOF))
        {
            ParseDeclaration();
        }
    }

    void ParseDeclaration()
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
            int value = ExpectInt();
            Expect(TokenType.SEMICOLON);
            Emit(Tag.Constant, type, DefineQualifiedVariableName(name), value);
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

                Emit(Tag.Struct, type.Name, fields);
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
                            region = ParseMemoryRegionQualifier();
                            CType fieldType = ExpectType();
                            string fieldName = ExpectAnyName();
                            fields.Add(new FieldInfo(region, fieldType, fieldName));
                            if (TryParse(TokenType.RPAREN)) break;
                            Expect(TokenType.COMMA);
                        }
                    }

                    Emit(Tag.Function, type, name, fields.ToArray());

                    BeginScope(name);

                    foreach (FieldInfo f in fields)
                    {
                        DefineQualifiedVariableName(f.Name);
                    }

                    Expect(TokenType.LBRACE);
                    while (!TryParse(TokenType.RBRACE))
                    {
                        ParseStatement(true);
                    }

                    // TODO: Return statement analysis will make this fallback code unnecessary.
                    // Functions that return non-void should never reach this point.
                    Emit(Tag.PushImmediate, 0);
                    Emit(Tag.Return);

                    EndScope();
                }
                else
                {
                    ParseArrayDeclaration(ref type);
                    if (TryParse(TokenType.EQUAL)) ParserError("global variables cannot be initialized");
                    Expect(TokenType.SEMICOLON);
                    Emit(Tag.Variable, region, type, DefineQualifiedVariableName(name));
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

    /// <summary>
    /// If false, only allow statements that would fit in a "for" initializer.
    /// </summary>
    void ParseStatement(bool allowLong)
    {
        MemoryRegion region;
        CType type;
        if (TryParseMemoryRegionQualifier(out region))
        {
            // Declare a local variable:
            type = ExpectType();
            ParseRestOfLocalDeclaration(region, type);
        }
        else if (TryParseType(out type))
        {
            // Declare a local variable:
            region = MemoryRegion.Ram;
            ParseRestOfLocalDeclaration(region, type);
        }
        else if (TryParseName("if"))
        {
            if (!allowLong) Error_NotAllowedInFor();

            // Create a switch expression with one or more test/body pairs:
            string elseLabel = MakeUniqueLabel("else");
            string endLabel = MakeUniqueLabel("end");

            // Parse the test:
            Expect(TokenType.LPAREN);
            ParseExpr();
            Expect(TokenType.RPAREN);
            Emit(Tag.JumpIfFalse, elseLabel);

            // Parse the body:
            ParseStatementBlock();
            Emit(Tag.Jump, endLabel);

            // Parse additional else statements:
            Emit(Tag.Label, elseLabel);
            while (TryParseName("else"))
            {
                if (TryParseName("if"))
                {
                    elseLabel = MakeUniqueLabel("else");

                    // Parse the test:
                    Expect(TokenType.LPAREN);
                    ParseExpr();
                    Expect(TokenType.RPAREN);
                    Emit(Tag.JumpIfFalse, elseLabel);

                    // Parse the body:
                    ParseStatementBlock();
                    Emit(Tag.Jump, endLabel);

                    Emit(Tag.Label, elseLabel);
                }
                else
                {
                    ParseStatementBlock();

                    // An "else" that is not an "else if" means it's time to stop:
                    break;
                }
            }

            Emit(Tag.Label, endLabel);
        }
        else if (TryParseName("for"))
        {
            if (!allowLong) Error_NotAllowedInFor();

            BeginScope("for");
            Loop = new LoopScope
            {
                Outer = Loop,
                ContinueLabel = MakeUniqueLabel("for_top"),
                BreakLabel = MakeUniqueLabel("for_bottom"),
            };

            Expect(TokenType.LPAREN);
            // Initialization:
            ParseStatement(false);
            Emit(Tag.Label, Loop.ContinueLabel);
            // Test:
            ParseExpr();
            Emit(Tag.JumpIfFalse, Loop.BreakLabel);
            Expect(TokenType.SEMICOLON);
            // Induction:
            // TODO: The induction step must go after the body!
            ParseExpr();
            Expect(TokenType.RPAREN);
            ParseStatementBlock();
            // TODO: The induction code goes here.
            Emit(Tag.Drop);
            Emit(Tag.Jump, Loop.ContinueLabel);
            Emit(Tag.Label, Loop.BreakLabel);

            Loop = Loop.Outer;
            EndScope();
        }
        else if (TryParseName("while"))
        {
            if (!allowLong) Error_NotAllowedInFor();

            BeginScope("while");
            Loop = new LoopScope
            {
                Outer = Loop,
                ContinueLabel = MakeUniqueLabel("while_top"),
                BreakLabel = MakeUniqueLabel("while_bottom"),
            };

            Expect(TokenType.LPAREN);
            Emit(Tag.Label, Loop.ContinueLabel);
            // Test:
            ParseExpr();
            Emit(Tag.JumpIfFalse, Loop.BreakLabel);
            Expect(TokenType.RPAREN);
            ParseStatementBlock();
            Emit(Tag.Jump, Loop.ContinueLabel);
            Emit(Tag.Label, Loop.BreakLabel);

            Loop = Loop.Outer;
            EndScope();
        }
        else if (TryParseName("continue"))
        {
            if (!allowLong) Error_NotAllowedInFor();
            Emit(Tag.Jump, Loop.ContinueLabel);
            Expect(TokenType.SEMICOLON);
        }
        else if (TryParseName("break"))
        {
            if (!allowLong) Error_NotAllowedInFor();
            Emit(Tag.Jump, Loop.BreakLabel);
            Expect(TokenType.SEMICOLON);
        }
        else if (TryParseName("return"))
        {
            if (!allowLong) Error_NotAllowedInFor();
            ParseExpr();
            Emit(Tag.Return);
            Expect(TokenType.SEMICOLON);
        }
        else if (TryParseName("__asm"))
        {
            if (!allowLong) Error_NotAllowedInFor();
            while (TryParse(TokenType.NEWLINE)) { /* Skip any number of newlines. */ }
            Expect(TokenType.LBRACE);
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
                        Emit(Expr.MakeAsm(symbol));
                    }
                    else if (TryParse(TokenType.COLON))
                    {
                        Expect(TokenType.NEWLINE);
                        Emit(Expr.Make(Tag.Label, FindQualifiedLabelName(symbol)));
                    }
                    else if (TryParse(TokenType.NUMBER_SIGN))
                    {
                        AsmOperand operand = ParseAssemblyOperand(AddressMode.Immediate);
                        Expect(TokenType.NEWLINE);
                        Emit(Expr.MakeAsm(symbol, operand));
                    }
                    else if (TryParse(TokenType.LPAREN))
                    {
                        AsmOperand operand = ParseAssemblyOperand(AddressMode.IndirectY);
                        Expect(TokenType.RPAREN);
                        Expect(TokenType.COMMA);
                        ExpectKeyword("Y");
                        Expect(TokenType.NEWLINE);
                        Emit(Expr.MakeAsm(symbol, operand));
                    }
                    else
                    {
                        AsmOperand operand = ParseAssemblyOperand(AddressMode.Absolute);
                        Expect(TokenType.NEWLINE);
                        Emit(Expr.MakeAsm(symbol, operand));
                    }
                }
            }
        }
        else
        {
            // An expression-statement:
            // (Indicate that the resulting value must be discarded.)
            ParseExpr();
            Emit(Tag.Drop);
            Expect(TokenType.SEMICOLON);
        }
    }

    void ParseRestOfLocalDeclaration(MemoryRegion region, CType type)
    {
        string name = ExpectAnyName();
        ParseArrayDeclaration(ref type);
        Emit(Tag.Variable, region, type, DefineQualifiedVariableName(name));

        // Optionally, an initial value can be assigned:
        if (TryParse(TokenType.EQUAL))
        {
            Emit(Tag.PushVariable, FindQualifiedName(name));
            ParseExpr();
            Emit(Tag.Store);
            Emit(Tag.Drop);
        }

        Expect(TokenType.SEMICOLON);
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
            return new AsmOperand(FindQualifiedLabelName(name), number, mode);
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

    void ParseStatementBlock()
    {
        // A block can be a single statement, or a series of statements surrounded by braces:
        if (TryParse(TokenType.LBRACE))
        {
            while (!TryParse(TokenType.RBRACE))
            {
                ParseStatement(true);
            }
        }
        else
        {
            ParseStatement(true);
        }
    }

    void ParseExpr()
    {
        ParseCommaExpr();
    }

    // ,
    void ParseCommaExpr()
    {
        ParseAssignExpr();
    }

    // = *= /= %= += -= <<= >>= &= ^= |=
    void ParseAssignExpr()
    {
        ParseConditionalExpr();
        if (TryParse(TokenType.EQUAL))
        {
            ParseAssignExpr();
            Emit(Tag.Store);
        }
    }

    // ? :
    void ParseConditionalExpr()
    {
        ParseLogicalOrExpr();
        if (TryParse(TokenType.QUESTION_MARK))
        {
            string otherwise = MakeUniqueLabel("cond_else");
            string end = MakeUniqueLabel("cond_end");

            Emit(Tag.JumpIfFalse, otherwise);
            ParseExpr();
            Emit(Tag.Jump, end);
            Emit(Tag.Label, otherwise);
            Expect(TokenType.COLON);
            ParseConditionalExpr();
            Emit(Tag.Label, end);
        }
    }

    // ||
    void ParseLogicalOrExpr()
    {
        ParseLogicalAndExpr();
        while (TryParse(TokenType.LOGICAL_OR))
        {
            string testTrue = MakeUniqueLabel("test_true");
            string testFalse = MakeUniqueLabel("test_false");
            string testEnd = MakeUniqueLabel("test_end");

            Emit(Tag.JumpIfTrue, testTrue);
            ParseLogicalAndExpr();
            Emit(Tag.JumpIfFalse, testFalse);
            Emit(Tag.Label, testTrue);
            Emit(Tag.PushImmediate, 1);
            Emit(Tag.Jump, testEnd);
            Emit(Tag.Label, testFalse);
            Emit(Tag.PushImmediate, 0);
            Emit(Tag.Label, testEnd);
        }
    }

    // &&
    void ParseLogicalAndExpr()
    {
        ParseBitwiseOrExpr();
        while (TryParse(TokenType.LOGICAL_AND))
        {
            string testFalse = MakeUniqueLabel("test_false");
            string testEnd = MakeUniqueLabel("test_end");

            Emit(Tag.JumpIfFalse, testFalse);
            ParseLogicalAndExpr();
            Emit(Tag.JumpIfFalse, testFalse);
            Emit(Tag.PushImmediate, 1);
            Emit(Tag.Jump, testEnd);
            Emit(Tag.Label, testFalse);
            Emit(Tag.PushImmediate, 0);
            Emit(Tag.Label, testEnd);
        }
    }

    // |
    void ParseBitwiseOrExpr()
    {
        ParseBitwiseXorExpr();
        while (TryParse(TokenType.PIPE))
        {
            ParseBitwiseXorExpr();
            Emit(Tag.BitwiseOr);
        }
    }

    // ^
    void ParseBitwiseXorExpr()
    {
        ParseBitwiseAndExpr();
        while (TryParse(TokenType.CARET))
        {
            ParseBitwiseAndExpr();
            Emit(Tag.BitwiseXor);
        }
    }

    // &
    void ParseBitwiseAndExpr()
    {
        ParseEqualityExpr();
        while (TryParse(TokenType.AMPERSAND))
        {
            ParseEqualityExpr();
            Emit(Tag.BitwiseAnd);
        }
    }

    // == !=
    void ParseEqualityExpr()
    {
        Dictionary<TokenType, string> operators = new Dictionary<TokenType, string>
        {
            { TokenType.DOUBLE_EQUAL, Tag.Equal },
            { TokenType.NOT_EQUAL, Tag.NotEqual },
        };

        ParseInfixOperators(ParseCompareExpr, operators);
    }

    // < > <= >=
    void ParseCompareExpr()
    {
        Dictionary<TokenType, string> operators = new Dictionary<TokenType, string>
        {
            { TokenType.LESS_THAN, Tag.LessThan },
            { TokenType.LESS_THAN_OR_EQUAL, Tag.LessThanOrEqual },
            { TokenType.GREATER_THAN, Tag.GreaterThan },
            { TokenType.GREATER_THAN_OR_EQUAL, Tag.GreaterThanOrEqual },
        };

        ParseInfixOperators(ParseShiftExpr, operators);
    }

    // << >>
    void ParseShiftExpr()
    {
        Dictionary<TokenType, string> operators = new Dictionary<TokenType, string>
        {
            { TokenType.SHIFT_LEFT, Tag.ShiftLeft },
            { TokenType.SHIFT_RIGHT, Tag.ShiftRight },
        };

        ParseInfixOperators(ParseAddExpr, operators);
    }

    // + -
    void ParseAddExpr()
    {
        Dictionary<TokenType, string> operators = new Dictionary<TokenType, string>
        {
            { TokenType.PLUS, Tag.Add },
            { TokenType.MINUS, Tag.Subtract },
        };

        ParseInfixOperators(ParseMultiplyExpr, operators);
    }

    // * / %
    void ParseMultiplyExpr()
    {
        Dictionary<TokenType, string> operators = new Dictionary<TokenType, string>
        {
            { TokenType.STAR, Tag.Multiply },
            { TokenType.SLASH, Tag.Divide },
            { TokenType.PERCENT, Tag.Modulus },
        };

        ParseInfixOperators(ParseCastExpr, operators);
    }

    // (casts)
    void ParseCastExpr()
    {
        ParseUnaryPrefixExpr();
    }

    // Unary prefix operators
    void ParseUnaryPrefixExpr()
    {
        Dictionary<TokenType, string> prefixes = new Dictionary<TokenType, string>
        {
            { TokenType.STAR, Tag.Load },
            { TokenType.AMPERSAND, Tag.AddressOf },
            { TokenType.TILDE, Tag.BitwiseNot },
            { TokenType.LOGICAL_NOT, Tag.LogicalNot },
            { TokenType.INCREMENT, Tag.Preincrement },
            { TokenType.DECREMENT, Tag.Predecrement },
        };

        string op;
        if (prefixes.TryGetValue(PeekToken().Tag, out op))
        {
            ParseUnaryPrefixExpr();
            Emit(op);
        }
        else
        {
            ParseSuffixExpr();
        }
    }

    // Suffix operators
    void ParseSuffixExpr()
    {
        ParsePrimaryExpr();
        while (true)
        {
            if (TryParse(TokenType.LPAREN))
            {
                if (!TryParse(TokenType.RPAREN))
                {
                    while (true)
                    {
                        ParseExpr();
                        if (TryParse(TokenType.RPAREN)) break;
                        Expect(TokenType.COMMA);
                    }
                }
                Emit(Tag.Call);
            }
            else if (TryParse(TokenType.PERIOD))
            {
                Emit(Tag.Field, ExpectAnyName());
            }
            else if (TryParse(TokenType.ARROW))
            {
                Emit(Tag.Load);
                Emit(Tag.Field, ExpectAnyName());
            }
            else if (TryParse(TokenType.INCREMENT))
            {
                Emit(Tag.Postincrement);
            }
            else if (TryParse(TokenType.DECREMENT))
            {
                Emit(Tag.Postdecrement);
            }
            else if (TryParse(TokenType.LBRACKET))
            {
                ParseExpr();
                Emit(Tag.Index);
                Expect(TokenType.RBRACKET);
            }
            else
            {
                // No more suffixes.
                break;
            }
        }
    }

    // "Primary" expressions
    void ParsePrimaryExpr()
    {
        int n;
        string name;
        if (TryParseInt(out n))
        {
            Emit(Tag.PushImmediate, n);
        }
        else if (TryParseAnyName(out name))
        {
            Emit(Tag.PushVariable, FindQualifiedName(name));
        }
        else if (TryParse(TokenType.LPAREN))
        {
            ParseExpr();
            Expect(TokenType.RPAREN);
        }
        else
        {
            ParserError("expected an expression");
        }
    }

    void ParseInfixOperators(Action parseSubexpression, Dictionary<TokenType, string> operators)
    {
        parseSubexpression();
        string op;
        while (operators.TryGetValue(PeekToken().Tag, out op))
        {
            ConsumeToken();
            parseSubexpression();
            Emit(op);
        }
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
        if (Scopes.Count <= 1) Program.Panic("cannot end global scope");
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

        string qualifiedName = functionScope.Name + Program.NamespaceSeparator + name;

        // If this symbol is a label that was referenced before it was defined, that's fine. Otherwise, error.
        if (UndefinedLabels.Contains(name))
        {
            UndefinedLabels.Remove(name);
        }
        else if (functionScope.QualifiedNames.ContainsKey(name))
        {
            Program.Error("symbol already defined: {0}", name);
        }
        else
        {
            functionScope.QualifiedNames.Add(name, qualifiedName);
        }

        return qualifiedName;
    }

    string DefineQualifiedVariableName(string name)
    {
        var table = Scopes.Last().QualifiedNames;
        if (table.ContainsKey(name)) Program.Error("symbol already defined: {0}", name);
        string qualifiers = string.Join(Program.NamespaceSeparator, Scopes.Skip(1).Select(x => x.Name));
        string qualifiedName = (qualifiers.Length > 0) ? (qualifiers + Program.NamespaceSeparator + name) : name;
        table.Add(name, qualifiedName);
        return qualifiedName;
    }

    bool TryFindQualifiedName(string name, out string qualifiedName)
    {
        // Search all scopes, starting from the innermost.
        foreach (LexicalScope scope in Enumerable.Reverse(Scopes))
        {
            if (scope.QualifiedNames.TryGetValue(name, out qualifiedName)) return true;
        }
        qualifiedName = null;
        return false;
    }

    string FindQualifiedName(string name)
    {
        string qualifiedName = null;
        if (!TryFindQualifiedName(name, out qualifiedName)) Program.Error("symbol not defined: {0}", name);
        return qualifiedName;
    }

    /// <summary>
    /// Labels can be referenced before they are defined.
    /// </summary>
    string FindQualifiedLabelName(string name)
    {
        // Forward-declare the label:
        UndefinedLabels.Add(name);
        return DefineQualifiedLabelName(name);
    }

    void ParserError(string format, params object[] args) => ParserError(string.Format(format, args));

    void ParserError(string message)
    {
        Token token = PeekToken();
        FilePosition pos = token.Position;
        Program.Error("syntax error (\"{0}\", line {1}, column {2}): {3}", pos.Filename, pos.Line + 1, pos.Column + 1, message);
    }
}
