#define _CRT_SECURE_NO_WARNINGS
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "Common.h"

static Expr *MakeExpr(ExprType type)
{
    Expr *e = XAlloc(sizeof(*e));
    memset(e, 0, sizeof(*e));
    e->Type = type;
    return e;
}

static void AppendArg(Expr *expr, Expr *arg)
{
    ExprList *last = XAlloc(sizeof(*last));
    last->E = arg;
    last->Next = NULL;

    if (!expr->Args)
    {
        expr->Args = last;
    }
    else
    {
        ExprList *p = expr->Args;
        while (p->Next) p = p->Next;
        p->Next = last;
    }
}

static Expr *Int(int32_t i)
{
    Expr *e = MakeExpr(EXPR_INT);
    e->Int = i;
    return e;
}

static Expr *Indirect(Expr *addr)
{
    Expr *e = MakeExpr(EXPR_INDIRECT);
    AppendArg(e, addr);
    return e;
}

static Expr *Assign(Expr *dest, Expr *src)
{
    Expr *e = MakeExpr(EXPR_ASSIGN);
    AppendArg(e, dest);
    AppendArg(e, src);
    return e;
}

// TODO: Support any number of subexpressions.
static Expr *Sequence(Expr *a, Expr *b)
{
    Expr *e = MakeExpr(EXPR_SEQUENCE);
    AppendArg(e, a);
    AppendArg(e, b);
    return e;
}

Expr *ParseFile(char *filename)
{
    // TODO: Parse the input file.
    FILE *f = fopen(filename, "r");
    assert(f);

    return Sequence(
        Assign(Indirect(Int(0x6000)), Int(42)),
        Assign(Indirect(Int(0x6002)), Int(99)));
}
