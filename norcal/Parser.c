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

void AppendExpr(Expr **list, Expr *e)
{
    if (*list)
    {
        Expr *p = *list;
        while (p->Next) p = p->Next;
        p->Next = e;
    }
    else
    {
        *list = e;
    }
}

static void AppendArg(Expr *call, Expr *arg)
{
    AppendExpr(&call->Args, arg);
}

Expr *MakeIntExpr(int32_t n)
{
    Expr *e = MakeExpr(EXPR_INT);
    e->Int = n;
    return e;
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

Expr *MakeLoadExpr(int32_t address)
{
    return MakeUnaryExpr(MakeNameExpr("$load"), MakeIntExpr(address));
}

static Expr *MakeSequenceExpr()
{
    Expr *e = MakeExpr(EXPR_CALL);
    AppendArg(e, MakeNameExpr("$sequence"));
    return e;
}

static Expr *AddressOf(Expr *e)
{
    Expr *inner;
    if (MatchUnaryCall(e, "$load", &inner))
    {
        return inner;
    }
    else
    {
        return MakeUnaryExpr(MakeNameExpr("$addr_of"), e);
    }
}

static Expr *MakeAssignExpr(Expr *dst, Expr *src)
{
    return MakeBinaryExpr(MakeNameExpr("$assign"), AddressOf(dst), src);
}

static bool TryParseType(Type *type)
{
    // TODO: Recognize all type names.
    if (TryParseName("uint16_t"))
    {
        *type = TYPE_UINT16;
        return true;
    }
    else
    {
        type = NULL;
        return false;
    }
}

// "Primary" expressions
static Expr *ParsePrimaryExpr()
{
    int32_t n;
    char *name;
    if (TryParseInt(&n))
    {
        return MakeIntExpr(n);
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
        return MakeUnaryExpr(MakeNameExpr("$load"), ParseSuffixExpr());
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
            e = MakeBinaryExpr(MakeNameExpr("$add"), e, ParseMultiplyExpr());
        }
        else if (TryParse(TO_MINUS))
        {
            e = MakeBinaryExpr(MakeNameExpr("$sub"), e, ParseMultiplyExpr());
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
        e = MakeAssignExpr(e, ParseAssignExpr());
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

static Expr *ParseStatement()
{
    Expr *stmt;
    Type type;
    if (TryParseType(&type))
    {
        // Declare a local variable:
        char *localname;
        if (!TryParseAnyName(&localname)) Error("expected variable name");
        // If no initial value is specified, initialize to zero.
        Expr *value;
        if (TryParse(TO_EQUALS)) value = ParseExpr();
        else value = MakeIntExpr(0);
        if (!TryParse(TO_SEMICOLON)) Error("expected ;");
        stmt = MakeSequenceExpr();
        AppendArg(stmt, MakeUnaryExpr(MakeNameExpr("$local"), MakeNameExpr(localname)));
        AppendArg(stmt, MakeAssignExpr(MakeNameExpr(localname), value));
    }
    else if (TryParseName("if"))
    {
        if (!TryParse(TO_LPAREN)) Error("expected (");
        Expr *test = ParseExpr();
        if (!TryParse(TO_RPAREN)) Error("expected )");
        Expr *then;
        if (TryParse(TO_LBRACE))
        {
            then = MakeSequenceExpr();
            while (!TryParse(TO_RBRACE))
            {
                AppendArg(then, ParseStatement());
            }
        }
        else
        {
            then = ParseStatement();
        }
        stmt = MakeBinaryExpr(MakeNameExpr("$switch"), test, then);
    }
    else
    {
        // An expression-statement:
        stmt = ParseExpr();
        if (!TryParse(TO_SEMICOLON)) Error("expected ;");
    }
    return stmt;
}

static Declaration *ParseDeclaration()
{
    Declaration *d;
    if (TryParseName("define"))
    {
        d = MakeDecl(DECL_CONSTANT);
        if (!TryParseAnyName(&d->Name)) Error("expected a name");
        if (!TryParse(TO_EQUALS)) Error("expected =");
        d->Body = ParseExpr();
        if (!TryParse(TO_SEMICOLON)) Error("expected ;");
    }
    else
    {
        d = MakeDecl(DECL_FUNCTION);
        if (!TryParseName("void")) Error("expected 'void'");
        if (!TryParseAnyName(&d->Name)) Error("expected function name");
        if (!TryParse(TO_LPAREN)) Error("expected (");
        if (!TryParse(TO_RPAREN)) Error("expected )");
        if (!TryParse(TO_LBRACE)) Error("expected {");
        d->Body = MakeSequenceExpr();
        while (!TryParse(TO_RBRACE))
        {
            Expr *stmt = ParseStatement();
            AppendArg(d->Body, stmt);
        }
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
