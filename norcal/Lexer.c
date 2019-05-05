#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Common.h"

#define TEST_PREPROCESSOR false
#define TEST_LEXER false

static char *Input;
static FilePos NextCharPos;

// Next token:
static TokenType NextType;
static int NextInt;
static char *NextName;
static FilePos TokenPos, LastTokenPos;

static char *ReadEntireFile(char *filename)
{
    FILE *f = fopen(filename, "r");
    if (!f) Error("cannot read input file");
    size_t capacity = 1024 * 1024;
    char *text = XAlloc(capacity);
    capacity -= 1; // Leave room for the string terminator.
    size_t len = 0;
    while (true)
    {
        if (capacity == 0) Error("input file is too big");
        size_t result = fread(text + len, 1, capacity, f);
        if (result < 0) Error("error reading input file");
        if (result == 0) break;
        capacity -= result;
        len += result;
    }
    text[len] = 0;
    return text;
}

static int GetNextChar()
{
    return *Input;
}

static int FetchChar()
{
    if (*Input) Input++;
    int c = *Input;
    if (c == '\n')
    {
        NextCharPos.Line++;
        NextCharPos.Column = 0;
    }
    else
    {
        NextCharPos.Column++;
    }
    return c;
}

static bool TryRead(char c)
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

static bool TryConvertInt(char *name, int32_t *integer)
{
    char *decimalDigits = "0123456789";

    // If the token doesn't start with a digit, it isn't a number.
    *integer = 0;
    if (!strchr(decimalDigits, *name)) return false;

    // TODO: Improve this function.
    bool isHex = false;
    int n = 0;
    for (char *p = name; *p; p++)
    {
        char c = *p;
        if (isHex)
        {
            char *hexdigits = "0123456789ABCDEF";
            char *found = strchr(hexdigits, c);
            if (found)
            {
                n = 16 * n + (int32_t)(found - hexdigits);
            }
            else
            {
                return false;
            }
        }
        else
        {
            if (strchr(decimalDigits, c))
            {
                n = 10 * n + (c - '0');
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

    *integer = n;
    return true;
}

static bool IsNameChar(char c)
{
    return (c == '_') || (c >= '0' && c <= '9') || (c >= 'A' && c < 'Z') || (c >= 'a' && c < 'z');
}

static void SkipSpaces()
{
    while (true)
    {
        char c = GetNextChar();
        if (c != ' ' && c != '\t' && c != '\r' && c != '\n') return;
        FetchChar();
    }
}

static void FetchToken()
{
    NextType = TO_INVALID;
    NextInt = 0;
    NextName = NULL;

    SkipSpaces();

    // Record the location of this token:
    LastTokenPos = TokenPos;
    TokenPos = NextCharPos;

    // Handle characters in order of ASCII value:
    // (Unprintable characters shouldn't be in the file, and whitespace was already skipped.)
    if (TryRead('\0')) NextType = TO_EOF;
    else if (GetNextChar() <= ' ') NextType = TO_INVALID;
    else if (TryRead('(')) NextType = TO_LPAREN;
    else if (TryRead(')')) NextType = TO_RPAREN;
    else if (TryRead('*')) NextType = TO_STAR;
    else if (TryRead('+')) NextType = TO_PLUS;
    else if (TryRead('-')) NextType = TO_MINUS;
    else if (TryRead(';')) NextType = TO_SEMICOLON;
    else if (TryRead('=')) NextType = TO_EQUALS;
    else if (TryRead('{')) NextType = TO_LBRACE;
    else if (TryRead('}')) NextType = TO_RBRACE;
    else if (IsNameChar(GetNextChar()))
    {
        // Parse identifiers and numeric literals:
        char name[128];
        int len = 0;
        while (IsNameChar(GetNextChar()))
        {
            if (len >= sizeof(name)) Error("identifier is too long");
            name[len] = GetNextChar();
            len++;
            FetchChar();
        }
        name[len] = '\0';

        if (TryConvertInt(name, &NextInt))
        {
            NextType = TO_INT;
        }
        else
        {
            NextType = TO_NAME;
            NextName = _strdup(name);
        }
    }
    else
    {
        Error("invalid token");
    }
}

FilePos GetNextTokenPosition()
{
    return TokenPos;
}

void InitLexer(char *filename)
{
    Input = ReadEntireFile(filename);
    NextCharPos = (FilePos){ 0, 0 };

    if (TEST_PREPROCESSOR)
    {
        FILE *f = fopen("preproc.out", "w");
        if (!f) Error("fopen");
        while (true)
        {
            int c = GetNextChar();
            if (!c) break;
            FilePos pos = NextCharPos;
            fprintf(f, "%d,%d: ", pos.Line, pos.Column);
            if (c == '\n') fputs("\\n", f);
            else fputc(c, f);
            fputc('\n', f);
            fflush(f);
            FetchChar();
        }
        fputs("<eof>\n", f);
        fclose(f);
        exit(0);
    }

    FetchToken();

    if (TEST_LEXER)
    {
        FILE *f = fopen("lexer.out", "w");
        if (!f) Error("fopen");
        while (NextType != TO_EOF)
        {
            if (NextType == TO_INT) fprintf(f, "%d\n", NextInt);
            else if (NextType == TO_NAME) fprintf(f, "%s\n", NextName);
            else fprintf(f, "TO_??? = %d\n", NextType);
            fflush(f);
            FetchToken();
        }
        fputs("<eof>\n", f);
        fclose(f);
        exit(0);
    }
}

bool TryParseInt(int32_t *n)
{
    if (NextType == TO_INT)
    {
        *n = NextInt;
        FetchToken();
        return true;
    }
    else
    {
        *n = 0;
        return false;
    }
}

bool TryParseAnyName(char **s)
{
    if (NextType == TO_NAME)
    {
        *s = NextName;
        FetchToken();
        return true;
    }
    else
    {
        *s = NULL;
        return false;
    }
}

bool TryParseName(char *s)
{
    if (NextType == TO_NAME && !strcmp(s, NextName))
    {
        FetchToken();
        return true;
    }
    else
    {
        return false;
    }
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

TokenType ReadNextToken()
{
    TokenType type = NextType;
    FetchToken();
    return type;
}
