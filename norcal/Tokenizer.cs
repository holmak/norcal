﻿using System;
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

    public static List<Token> TokenizeFile(string filename)
    {
        Tokenizer tokenizer = new Tokenizer();
        tokenizer.Input = File.ReadAllText(filename);
        return tokenizer.Tokenize();
    }

    public List<Token> Tokenize()
    {
        List<Token> tokens = new List<Token>();
        FilePosition pos, lastPos;
        pos.Line = 0;
        pos.Column = 0;
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
            else if (GetNextChar() <= ' ') tag = TokenType.INVALID;
            else if (TryRead("#define"))
            {
                tag = TokenType.NAME;
                tokenName = "#define";
            }
            else if (TryRead('#'))
            {
                SkipToNextLine();
                continue;
            }
            else if (TryRead('(')) tag = TokenType.LPAREN;
            else if (TryRead(')')) tag = TokenType.RPAREN;
            else if (TryRead('*')) tag = TokenType.STAR;
            else if (TryRead('+')) tag = TokenType.PLUS;
            else if (TryRead(',')) tag = TokenType.COMMA;
            else if (TryRead('-')) tag = TokenType.MINUS;
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
            else if (TryRead(';')) tag = TokenType.SEMICOLON;
            else if (TryRead('=')) tag = TokenType.EQUALS;
            else if (TryRead('{')) tag = TokenType.LBRACE;
            else if (TryRead('}')) tag = TokenType.RBRACE;
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

                if (TryConvertInt(name, out tokenInt))
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
        }

        return tokens;
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

    static void Error(FilePosition pos, string message)
    {
        Program.Error("syntax error (line {0}, column {1}): {2}", pos.Line, pos.Column, message);
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
        else return Tag.ToString();
    }
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

struct FilePosition
{
    public int Line, Column;
}