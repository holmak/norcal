using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

class Tokenizer
{
    string Input;
    int Next = 0;
    FilePosition InputPos;
    bool InAssembly;

    public static List<Token> TokenizeFile(string filename)
    {
        Tokenizer tokenizer = new Tokenizer();
        tokenizer.Input = File.ReadAllText(filename);
        tokenizer.InputPos = new FilePosition(filename, 0, 0);
        return tokenizer.Tokenize();
    }

    public List<Token> Tokenize()
    {
        List<Token> tokens = new List<Token>();
        FilePosition pos, lastPos;
        pos = InputPos;
        while (true)
        {
            TokenType tag = TokenType.INVALID;
            int tokenInt = 0;
            string tokenName = null;

            SkipSpaces();

            // Record the location of this token:
            lastPos = pos;
            pos = InputPos;

            // Handle characters in order of ASCII value:

            // (Unprintable characters shouldn't be in the file, and whitespace was already skipped.)
            if (TryRead('\0')) return tokens;
            else if (TryRead("\n")) tag = TokenType.NEWLINE;
            else if (GetNextChar() <= ' ') tag = TokenType.INVALID;
            else if (TryRead('!'))
            {
                if (TryRead("=")) tag = TokenType.NOT_EQUAL;
                else tag = TokenType.LOGICAL_NOT;
            }
            else if (TryRead('"'))
            {
                tag = TokenType.STRING;
                tokenName = "";
                while (true)
                {
                    char c = GetNextChar();
                    if (c == '\0') Program.Error(InputPos, "unexpected end of file in string");
                    if (c == '\n') Program.Error(InputPos, "unexpected end of line in string");
                    if (c == '"')
                    {
                        FetchChar();
                        break;
                    }
                    tokenName += c;
                    FetchChar();
                }
            }
            else if (TryRead('#'))
            {
                if (InAssembly)
                {
                    tag = TokenType.NUMBER_SIGN;
                }
                else
                {
                    Program.Warning(InputPos, "preprocessor directives are ignored");
                    SkipToNextLine();
                    continue;
                }
            }
            else if (TryRead('%'))
            {
                if (TryRead('=')) tag = TokenType.PERCENT_EQUALS;
                else tag = TokenType.PERCENT;
            }
            else if (TryRead('&'))
            {
                if (TryRead('&')) tag = TokenType.LOGICAL_AND;
                else if (TryRead('=')) tag = TokenType.AMPERSAND_EQUALS;
                else tag = TokenType.AMPERSAND;
            }
            else if (TryRead('(')) tag = TokenType.LPAREN;
            else if (TryRead(')')) tag = TokenType.RPAREN;
            else if (TryRead('*'))
            {
                if (TryRead('=')) tag = TokenType.STAR_EQUALS;
                else tag = TokenType.STAR;
            }
            else if (TryRead('+'))
            {
                if (TryRead('+')) tag = TokenType.INCREMENT;
                else if (TryRead('=')) tag = TokenType.PLUS_EQUALS;
                else tag = TokenType.PLUS;
            }
            else if (TryRead(',')) tag = TokenType.COMMA;
            else if (TryRead('-'))
            {
                if (TryRead('>')) tag = TokenType.ARROW;
                else if (TryRead('-')) tag = TokenType.DECREMENT;
                else if (TryRead('=')) tag = TokenType.MINUS_EQUALS;
                else tag = TokenType.MINUS;
            }
            else if (TryRead('.')) tag = TokenType.PERIOD;
            else if (TryRead('/'))
            {
                // Skip past single-line comments:
                if (TryRead('/'))
                {
                    SkipToNextLine();
                    continue;
                }
                else
                {
                    if (TryRead('=')) tag = TokenType.SLASH_EQUALS;
                    else tag = TokenType.SLASH;
                }
            }
            else if (TryRead(':')) tag = TokenType.COLON;
            else if (TryRead(';')) tag = TokenType.SEMICOLON;
            else if (TryRead('<'))
            {
                if (TryRead('=')) tag = TokenType.LESS_THAN_OR_EQUAL;
                else if (TryRead('<'))
                {
                    if (TryRead('=')) tag = TokenType.SHIFT_LEFT_EQUALS;
                    else tag = TokenType.SHIFT_LEFT;
                }
                else tag = TokenType.LESS_THAN;
            }
            else if (TryRead('='))
            {
                if (TryRead('=')) tag = TokenType.DOUBLE_EQUAL;
                else tag = TokenType.EQUAL;
            }
            else if (TryRead('>'))
            {
                if (TryRead('=')) tag = TokenType.GREATER_THAN_OR_EQUAL;
                else if (TryRead('>'))
                {
                    if (TryRead('=')) tag = TokenType.SHIFT_RIGHT_EQUALS;
                    else tag = TokenType.SHIFT_RIGHT;
                }
                else tag = TokenType.GREATER_THAN;
            }
            else if (TryRead('?')) tag = TokenType.QUESTION_MARK;
            else if (TryRead('[')) tag = TokenType.LBRACKET;
            else if (TryRead(']')) tag = TokenType.RBRACKET;
            else if (TryRead('^'))
            {
                if (TryRead('=')) tag = TokenType.CARET_EQUALS;
                else tag = TokenType.CARET;
            }
            else if (TryRead('{')) tag = TokenType.LBRACE;
            else if (TryRead('|'))
            {
                if (TryRead('|')) tag = TokenType.LOGICAL_OR;
                else if (TryRead('=')) tag = TokenType.PIPE_EQUALS;
                else tag = TokenType.PIPE;
            }
            else if (TryRead('}')) tag = TokenType.RBRACE;
            else if (TryRead('~')) tag = TokenType.TILDE;
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

                if (TryConvertInt(pos, name, out tokenInt))
                {
                    tag = TokenType.INT;
                }
                else
                {
                    tag = TokenType.NAME;
                    tokenName = name;
                }
            }
            else
            {
                // HACK: Show the input that could not be tokenized:
                Program.Error(pos, "invalid token: " + new string(Input.Skip(Next).Take(15).ToArray()));
            }

            tokens.Add(new Token
            {
                Tag = tag,
                Int = tokenInt,
                Name = tokenName,
                Position = pos,
            });

            if (!InAssembly && tag == TokenType.NAME && tokenName == "__asm")
            {
                InAssembly = true;
            }

            if (InAssembly && tag == TokenType.RBRACE)
            {
                InAssembly = false;
            }
        }
    }

    static bool IsNameChar(char c)
    {
        return (c == '_') || (c == '$') || (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
    }

    void SkipSpaces()
    {
        string whitespace = InAssembly ? " \t\r" : " \t\r\n";
        while (true)
        {
            char c = GetNextChar();
            if (!whitespace.Contains(c)) return;
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
    }

    static bool TryConvertInt(FilePosition pos, string original, out int integer)
    {
        string decimalDigits = "0123456789";
        string hexDigits = "0123456789ABCDEF";

        if (original.Length == 0) Program.Panic(pos, "names must not be empty");
        string literal = original.ToUpperInvariant();

        // If the token doesn't start with a digit, it isn't a number.
        integer = 0;
        char first = literal[0];
        if (!decimalDigits.Contains(first) && first != '$') return false;

        int numberBase = 10;
        if (literal.StartsWith("0X"))
        {
            literal = literal.Substring(2);
            numberBase = 16;
        }
        else if (literal.StartsWith("$"))
        {
            literal = literal.Substring(1);
            numberBase = 16;
        }
        else if (literal.StartsWith("0B"))
        {
            literal = literal.Substring(2);
            numberBase = 2;
        }

        if (literal.Length == 0)
        {
            Program.Error(pos, "number contains no digits: " + original);
        }

        string allowedDigits = hexDigits.Substring(0, numberBase);
        if (!literal.All(x => allowedDigits.Contains(x)))
        {
            Program.Error(pos, "number contains invalid characters: " + original);
        }

        integer = 0;
        foreach (char c in literal)
        {
            int index = allowedDigits.IndexOf(c);
            integer = numberBase * integer + index;
        }
        return true;
    }

    char GetNextChar()
    {
        return (Next < Input.Length) ? Input[Next] : '\0';
    }

    void FetchChar()
    {
        if (Next < Input.Length) Next++;
        char c = GetNextChar();
        if (c == '\n')
        {
            InputPos = new FilePosition(InputPos.Filename, InputPos.Line + 1, 0);
        }
        else
        {
            InputPos = new FilePosition(InputPos.Filename, InputPos.Line, InputPos.Column + 1);
        }
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
}

[DebuggerDisplay("{Show(),nq}")]
struct Token
{
    public TokenType Tag;
    public int Int;
    public string Name;
    public FilePosition Position;

    public string Show()
    {
        if (Tag == TokenType.INT) return Int.ToString();
        else if (Tag == TokenType.NAME) return Name;
        else if (Tag == TokenType.STRING) return string.Format("\"{0}\"", Name);
        else return Tag.ToString();
    }
}

enum TokenType
{
    INVALID,
    EOF,
    NEWLINE,

    LOGICAL_NOT,
    NOT_EQUAL,
    NUMBER_SIGN,
    PERCENT,
    AMPERSAND,
    LPAREN,
    RPAREN,
    STAR,
    PLUS,
    COMMA,
    MINUS,
    ARROW,
    PERIOD,
    SLASH,
    COLON,
    SEMICOLON,
    LESS_THAN,
    LESS_THAN_OR_EQUAL,
    EQUAL,
    DOUBLE_EQUAL,
    GREATER_THAN,
    GREATER_THAN_OR_EQUAL,
    QUESTION_MARK,
    LBRACKET,
    RBRACKET,
    CARET,
    LBRACE,
    PIPE,
    RBRACE,
    TILDE,
    INCREMENT,
    DECREMENT,
    SHIFT_LEFT,
    SHIFT_RIGHT,
    LOGICAL_OR,
    LOGICAL_AND,
    PLUS_EQUALS,
    MINUS_EQUALS,
    STAR_EQUALS,
    SLASH_EQUALS,
    PERCENT_EQUALS,
    SHIFT_LEFT_EQUALS,
    SHIFT_RIGHT_EQUALS,
    AMPERSAND_EQUALS,
    PIPE_EQUALS,
    CARET_EQUALS,

    INT,
    NAME,
    STRING,
}

static class TokenInfo
{
    public static string[] TokenNames = new string[]
    {
        "(invalid)",
        "EOF",
        "newline",

        "!",
        "!=",
        "#",
        "%",
        "&",
        "(",
        ")",
        "*",
        "+",
        ",",
        "-",
        "->",
        ".",
        "/",
        ":",
        ";",
        "<",
        "<=",
        "=",
        "==",
        ">",
        ">=",
        "?",
        "[",
        "]",
        "^",
        "{",
        "|",
        "}",
        "~",
        "++",
        "--",
        "<<",
        ">>",
        "||",
        "&&",
        "+=",
        "-=",
        "*=",
        "/=",
        "%=",
        "<<=",
        ">>=",
        "&=",
        "|=",
        "^=",

        "(int)",
        "(name)",
        "(string)",
    };
}

struct FilePosition
{
    public readonly string Filename;
    public readonly int Line, Column;

    public FilePosition(string filename, int line, int column)
    {
        Filename = filename;
        Line = line;
        Column = column;
    }

    public static readonly FilePosition Unknown = new FilePosition("<unknown>", 0, 0);

    public override string ToString()
    {
        return string.Format("{0} (line {1}, column {2})", Filename, Line + 1, Column + 1);
    }
}
