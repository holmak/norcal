#define _CRT_SECURE_NO_WARNINGS
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include "Common.h"

typedef struct Reader
{
    FILE *File;
    int Next;
} Reader;

static Expr *MakeExpr(ExprType type)
{
    Expr *e = XAlloc(sizeof(*e));
    memset(e, 0, sizeof(*e));
    e->Type = type;
    return e;
}

static void AppendArg(Expr *expr, Expr *arg)
{
    if (!expr->Args)
    {
        expr->Args = arg;
    }
    else
    {
        Expr *p = expr->Args;
        while (p->Next) p = p->Next;
        p->Next = arg;
    }
}

static int Read(Reader *r)
{
    int c = r->Next;
    r->Next = fgetc(r->File);
    return c;
}

static bool TryRead(Reader *r, char c)
{
    if (r->Next == c)
    {
        Read(r);
        return true;
    }
    else
    {
        return false;
    }
}

static bool TryReadAny(Reader *r, char *chars)
{
    if (strchr(chars, r->Next))
    {
        Read(r);
        return true;
    }
    else
    {
        return false;
    }
}

static void SkipSpaces(Reader *r)
{
    while (TryReadAny(r, " \t\n")) { /* skip */ }
}

static bool TryParseInteger(char *name, int32_t *integer)
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

static Expr *ParseList(Reader *r);

static Expr *Parse(Reader *r)
{
    if (TryRead(r, EOF))
    {
        Error("unexpected end of file");
        return NULL;
    }
    else if (TryRead(r, '('))
    {
        Expr *e = ParseList(r);
        SkipSpaces(r);
        if (!TryRead(r, ')')) Error("expected )");
        return e;
    }
    else
    {
        char name[128];
        int len = 0;
        while (!isspace(r->Next) && r->Next != ')')
        {
            if (len >= sizeof(name)) Error("identifier is too long");
            name[len] = Read(r);
            len++;
        }
        name[len] = '\0';

        int32_t n;
        if (TryParseInteger(name, &n))
        {
            Expr *e = MakeExpr(EXPR_INT);
            e->Int = n;
            return e;
        }
        else
        {
            Expr *e = MakeExpr(EXPR_NAME);
            e->Name = _strdup(name);
            return e;
        }
    }
}

static Expr *ParseList(Reader *r)
{
    Expr *e = MakeExpr(EXPR_SEQUENCE);
    while (true)
    {
        SkipSpaces(r);
        if (r->Next == ')' || r->Next == EOF) break;
        AppendArg(e, Parse(r));
    }
    return e;
}

Expr *ParseFile(char *filename)
{
    Reader reader;
    reader.File = fopen(filename, "r");
    assert(reader.File);
    Read(&reader);
    return ParseList(&reader);
}
