#define _CRT_SECURE_NO_WARNINGS
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "Common.h"

static void AppendArg(Expr *call, Expr *item)
{
    NYI();
}

static Expr *MakeExpr(ExprType type)
{
    Expr *e = XAlloc(sizeof(*e));
    memset(e, 0, sizeof(*e));
    e->Type = type;
    return e;
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
    e->Left = addr;
    return e;
}

static Expr *Assign(Expr *dest, Expr *src)
{
    Expr *e = MakeExpr(EXPR_ASSIGN);
    e->Left = dest;
    e->Right = src;
    return e;
}

Expr *Parse(char *filename)
{
    // TODO: Parse the input file.
    FILE *f = fopen(filename, "r");
    assert(f);

    return Assign(Indirect(Int(0x6002)), Int(42));
}
