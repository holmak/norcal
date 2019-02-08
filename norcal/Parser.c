#include <stdio.h>
#include <string.h>
#include "Common.h"

static Expr *ParseExpr();

static Expr *MakeExpr(ExprType type)
{
    Expr *e = XAlloc(sizeof(*e));
    memset(e, 0, sizeof(*e));
    e->Type = type;
    return e;
}

static Declaration *MakeDecl(DeclType type)
{
    Declaration *d = XAlloc(sizeof(*d));
    memset(d, 0, sizeof(*d));
    d->Type = type;
    return d;
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

static Expr *MakeNameExpr(char *name)
{
    Expr *e = MakeExpr(EXPR_NAME);
    e->Name = name;
    return e;
}

static Expr *MakeUnaryExpr(Expr *func, Expr *arg)
{
    Expr *e = MakeExpr(EXPR_CALL);
    AppendArg(e, func);
    AppendArg(e, arg);
    return e;
}

static Expr *MakeBinaryExpr(Expr *func, Expr *left, Expr *right)
{
    Expr *e = MakeExpr(EXPR_CALL);
    AppendArg(e, func);
    AppendArg(e, left);
    AppendArg(e, right);
    return e;
}

static Expr *AddressOf(Expr *e)
{
    Expr *inner;
    if (MatchUnaryCall(e, "*", &inner))
    {
        return inner;
    }
    else
    {
        return MakeUnaryExpr(MakeNameExpr("&"), e);
    }
}

// "Primary" expressions
static Expr *ParsePrimaryExpr()
{
    int32_t n;
    char *name;
    if (TryParseInt(&n))
    {
        Expr *e = MakeExpr(EXPR_INT);
        e->Int = n;
        return e;
    }
    else if (TryParseAnyName(&name))
    {
        return MakeNameExpr(name);
    }
    else if (TryParse(TO_LPAREN))
    {
        Expr *e = ParseExpr();
        if (!TryParse(TO_RPAREN)) Error("expected )");
        return e;
    }
    else
    {
        Error("expected an expression");
        return NULL;
    }
}

// Suffix operators
static Expr *ParseSuffixExpr()
{
    return ParsePrimaryExpr();
}

// Unary prefix operators
static Expr *ParseUnaryPrefixExpr()
{
    if (TryParse(TO_STAR))
    {
        return MakeUnaryExpr(MakeNameExpr("*"), ParseSuffixExpr());
    }
    else
    {
        return ParseSuffixExpr();
    }
}

// (casts)
static Expr *ParseCastExpr()
{
    return ParseUnaryPrefixExpr();
}

// * / %
static Expr *ParseMultiplyExpr()
{
    return ParseCastExpr();
}

// + -
static Expr *ParseAddExpr()
{
    Expr *e = ParseMultiplyExpr();
    while (true)
    {
        if (TryParse(TO_PLUS))
        {
            e = MakeBinaryExpr(MakeNameExpr("+"), e, ParseMultiplyExpr());
        }
        else if (TryParse(TO_MINUS))
        {
            e = MakeBinaryExpr(MakeNameExpr("-"), e, ParseMultiplyExpr());
        }
        else
        {
            return e;
        }
    }
}

// << >>
static Expr *ParseShiftExpr()
{
    return ParseAddExpr();
}

// > >= etc.
static Expr *ParseCompareExpr()
{
    return ParseShiftExpr();
}

// == !=
static Expr *ParseEqualityExpr()
{
    return ParseCompareExpr();
}

// &
static Expr *ParseBitwiseAndExpr()
{
    return ParseEqualityExpr();
}

// |
static Expr *ParseBitwiseOrExpr()
{
    return ParseBitwiseAndExpr();
}

// &&
static Expr *ParseLogicalAndExpr()
{
    return ParseBitwiseOrExpr();
}

// ||
static Expr *ParseLogicalOrExpr()
{
    return ParseLogicalAndExpr();
}

// TODO: Incorporate this function into the parsing hierarchy.
// ? :
static Expr *ParseConditionalExpr()
{
    return ParseLogicalOrExpr();
}

// = += -= etc.
static Expr *ParseAssignExpr()
{
    Expr *e = ParseLogicalOrExpr();
    if (TryParse(TO_EQUALS))
    {
        e = MakeBinaryExpr(MakeNameExpr("="), AddressOf(e), ParseAssignExpr());
    }
    return e;
}

// ,
static Expr *ParseCommaExpr()
{
    return ParseAssignExpr();
}

static Expr *ParseExpr()
{
    return ParseCommaExpr();
}

static Declaration *ParseDeclaration()
{
    Declaration *d = MakeDecl(DECL_FUNCTION);
    if (!TryParseName("void")) Error("expected 'void'");
    if (!TryParseAnyName(&d->Name)) Error("expected function name");
    if (!TryParse(TO_LPAREN)) Error("expected (");
    if (!TryParse(TO_RPAREN)) Error("expected )");
    if (!TryParse(TO_LBRACE)) Error("expected {");
    d->Body = MakeExpr(EXPR_SEQUENCE);
    while (!TryParse(TO_RBRACE))
    {
        AppendArg(d->Body, ParseExpr());
        if (!TryParse(TO_SEMICOLON)) Error("expected ;");
    }
    return d;
}

static char *TokenToString(TokenType token)
{
    switch (token)
    {
    case TO_LPAREN: return "(";
    case TO_RPAREN: return ")";
    case TO_STAR: return "*";
    case TO_PLUS: return "+";
    case TO_MINUS: return "-";
    case TO_EQUALS: return "=";
    case TO_SEMICOLON: return ";";
    case TO_LBRACE: return "{";
    case TO_RBRACE: return "}";
    default:
        NYI();
        return "NYI";
    }
}

void TestLexer()
{
    printf("Lexer output: ");
    while (true)
    {
        int32_t n;
        char * name;
        if (TryParseInt(&n))
        {
            printf("0x%X ", n);
        }
        else if (TryParseAnyName(&name))
        {
            printf("'%s' ", name);
        }
        else if (TryParse(TO_EOF))
        {
            printf("<EOF>\n");
            break;
        }
        else
        {
            TokenType token = ReadNextToken();
            printf("%s ", TokenToString(token));
        }
    }
}

Declaration *ParseFile(char *filename)
{
    InitLexer(filename);
    Declaration *program = NULL;
    while (!TryParse(TO_EOF))
    {
        Declaration *decl = ParseDeclaration();
        // TODO: Append to the end of the list.
        decl->Next = program;
        program = decl;
    }
    return program;
}
