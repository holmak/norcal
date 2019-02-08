#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <string.h>
#include "Common.h"

static FILE *File;
static int NextChar;

// Next token:
static TokenType NextType;
static int NextInt;
static char *NextName;

static int FetchChar()
{
    int c = NextChar;
    NextChar = fgetc(File);
    return c;
}

static bool TryRead(char c)
{
    if (NextChar == c)
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
                *integer = 0;
                return false;
            }
        }
        else
        {
            if (strchr("0123456789", c))
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
                *integer = 0;
                return false;
            }
        }
    }

    *integer = n;
    return true;
}

static bool IsNameChar(char c)
{
    return (c == '_') || (c >= '0' && c < '9') || (c >= 'A' && c < 'Z') || (c >= 'a' && c < 'z');
}

static void FetchToken()
{
    NextType = TO_INVALID;
    NextInt = 0;
    NextName = NULL;

    // Skip spaces:
    while (strchr(" \t\n", NextChar)) FetchChar();

    // Handle characters in order of ASCII value:
    // (Unprintable characters shouldn't be in the file, and whitespace was already skipped.)
    if (TryRead(EOF)) NextType = TO_EOF;
    else if (NextChar <= ' ') NextType = TO_INVALID;
    else if (TryRead('(')) NextType = TO_LPAREN;
    else if (TryRead(')')) NextType = TO_RPAREN;
    else if (TryRead('*')) NextType = TO_STAR;
    else if (TryRead('+')) NextType = TO_PLUS;
    else if (TryRead('-')) NextType = TO_MINUS;
    else if (TryRead(';')) NextType = TO_SEMICOLON;
    else if (TryRead('=')) NextType = TO_EQUALS;
    else if (TryRead('{')) NextType = TO_LBRACE;
    else if (TryRead('}')) NextType = TO_RBRACE;
    else if (IsNameChar(NextChar))
    {
        // Parse identifiers and numeric literals:
        char name[128];
        int len = 0;
        while (IsNameChar(NextChar))
        {
            if (len >= sizeof(name)) Error("identifier is too long");
            name[len] = NextChar;
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
        NYI();
    }
}

void InitLexer(char *filename)
{
    File = fopen(filename, "r");
    if (!File) Error("cannot read input file");
    FetchToken();
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
