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
        tokenizer.InputPos.Filename = filename;
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
            if (TryRead('\0')) tag = TokenType.EOF;
            else if (TryRead("\n")) tag = TokenType.NEWLINE;
            else if (GetNextChar() <= ' ') tag = TokenType.INVALID;
            else if (TryRead('!'))
            {
                if (TryRead("=")) tag = TokenType.NOT_EQUAL;
                else tag = TokenType.NOT;
            }
            else if (TryRead('"'))
            {
                tag = TokenType.STRING;
                tokenName = "";
                while (true)
                {
                    char c = GetNextChar();
                    if (c == '\0') Error(InputPos, "unexpected end of file in string");
                    if (c == '\n') Error(InputPos, "unexpected end of line in string");
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
                    Warning(InputPos, "preprocessor directives are ignored");
                    SkipToNextLine();
                    continue;
                }
            }
            else if (TryRead('%')) tag = TokenType.PERCENT;
            else if (TryRead('&')) tag = TokenType.AMPERSAND;
            else if (TryRead('(')) tag = TokenType.LPAREN;
            else if (TryRead(')')) tag = TokenType.RPAREN;
            else if (TryRead('*')) tag = TokenType.STAR;
            else if (TryRead('+')) tag = TokenType.PLUS;
            else if (TryRead(',')) tag = TokenType.COMMA;
            else if (TryRead('-'))
            {
                if (TryRead('>')) tag = TokenType.ARROW;
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
                    tag = TokenType.SLASH;
                }
            }
            else if (TryRead(':')) tag = TokenType.COLON;
            else if (TryRead(';')) tag = TokenType.SEMICOLON;
            else if (TryRead('<')) tag = TokenType.LESS_THAN;
            else if (TryRead('='))
            {
                if (TryRead('=')) tag = TokenType.DOUBLE_EQUAL;
                else tag = TokenType.EQUAL;
            }
            else if (TryRead('>')) tag = TokenType.GREATER_THAN;
            else if (TryRead('?')) tag = TokenType.QUESTION_MARK;
            else if (TryRead('[')) tag = TokenType.LBRACKET;
            else if (TryRead(']')) tag = TokenType.RBRACKET;
            else if (TryRead('^')) tag = TokenType.CARET;
            else if (TryRead('{')) tag = TokenType.LBRACE;
            else if (TryRead('|')) tag = TokenType.PIPE;
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
                Error(pos, "invalid token");
            }

            tokens.Add(new Token
            {
                Tag = tag,
                Int = tokenInt,
                Name = tokenName,
                Position = pos,
            });

            if (tag == TokenType.EOF)
            {
                break;
            }

            if (!InAssembly && tag == TokenType.NAME && tokenName == "__asm")
            {
                InAssembly = true;
            }

            if (InAssembly && tag == TokenType.RBRACE)
            {
                InAssembly = false;
            }
        }

        return tokens;
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

        if (original.Length == 0) Program.Panic("names must not be empty");
        string literal = original.ToUpperInvariant();

        // If the token doesn't start with a digit, it isn't a number.
        integer = 0;
        char first = literal[0];
        if (!decimalDigits.Contains(first) && first != '$') return false;

        bool isHex = false;
        if (literal.StartsWith("0X"))
        {
            literal = literal.Substring(2);
            isHex = true;
        }
        else if (literal.StartsWith("$"))
        {
            literal = literal.Substring(1);
            isHex = true;
        }

        if (literal.Length == 0)
        {
            Error(pos, "number contains no digits: " + original);
        }

        string allowedDigits = isHex ? hexDigits : decimalDigits;
        if (!literal.All(x => allowedDigits.Contains(x)))
        {
            Error(pos, "number contains invalid characters: " + original);
        }

        int numberBase = isHex ? 16 : 10;
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
            InputPos.Line++;
            InputPos.Column = 0;
        }
        else
        {
            InputPos.Column++;
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

    static void Warning(FilePosition pos, string message)
    {
        Program.Warning("warning (\"{0}\", line {1}, column {2}): {3}", pos.Filename, pos.Line, pos.Column, message);
    }

    static void Error(FilePosition pos, string message)
    {
        Program.Error("syntax error (\"{0}\", line {1}, column {2}): {3}", pos.Filename, pos.Line, pos.Column, message);
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

    NOT,
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
    EQUAL,
    DOUBLE_EQUAL,
    GREATER_THAN,
    QUESTION_MARK,
    LBRACKET,
    RBRACKET,
    CARET,
    LBRACE,
    PIPE,
    RBRACE,
    TILDE,

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
        "=",
        "==",
        ">",
        "?",
        "[",
        "]",
        "^",
        "{",
        "|",
        "{",
        "~",

        "(int)",
        "(name)",
        "(string)",
    };
}

struct FilePosition
{
    public string Filename;
    public int Line, Column;
}
