#include <stdio.h>
#include "Common.h"

static void GetOneArg(Expr *e, Expr **arg)
{
    if (e->Args && !e->Args->Next)
    {
        *arg = e->Args;
    }
    else
    {
        Panic("expression has wrong number of arguments; expected one");
    }
}

static void GetTwoArgs(Expr *e, Expr **left, Expr **right)
{
    if (e->Args && e->Args->Next && !e->Args->Next->Next)
    {
        *left = e->Args;
        *right = e->Args->Next;
    }
    else
    {
        Panic("expression has wrong number of arguments; expected two");
    }
}

bool MatchIntExpr(Expr *e, int32_t *n)
{
    if (e->Type == EXPR_INT)
    {
        *n = e->Int;
        return true;
    }
    else
    {
        *n = 0;
        return false;
    }
}

bool MatchUnaryExpr(Expr *e, ExprType type, Expr **arg)
{
    if (e->Type == type)
    {
        GetOneArg(e, arg);
        return true;
    }
    else
    {
        *arg = NULL;
        return false;
    }
}

bool MatchBinaryExpr(Expr *e, ExprType type, Expr **left, Expr **right)
{
    if (e->Type == type)
    {
        GetTwoArgs(e, left, right);
        return true;
    }
    else
    {
        *left = NULL;
        *right = NULL;
        return false;
    }
}

void PrintExpr(Expr *e)
{
    int n;
    if (MatchIntExpr(e, &n))
    {
        // Guess whether the number should be printed in hex or in decimal:
        if (n >= 512) printf("0x%X", n);
        else printf("%d", n);
    }
    else if (e->Type == EXPR_NAME)
    {
        printf("%s", e->Name);
    }
    else if (e->Type == EXPR_SEQUENCE)
    {
        printf("(");
        for (Expr *p = e->Args; p; p = p->Next)
        {
            PrintExpr(p);
            printf(p->Next ? " " : ")");
        }
    }
    else
    {
        NYI();
    }
}
