#include <stdio.h>
#include <string.h>
#include "Common.h"

static bool TryGetTwoArgs(Expr *e, Expr **a, Expr **b)
{
    if (e->Args && e->Args->Next && !e->Args->Next->Next)
    {
        *a = e->Args;
        *b = e->Args->Next;
        return true;
    }
    else
    {
        return false;
    }
}

static bool TryGetThreeArgs(Expr *e, Expr **a, Expr **b, Expr **c)
{
    if (e->Args && e->Args->Next && e->Args->Next->Next && !e->Args->Next->Next->Next)
    {
        *a = e->Args;
        *b = e->Args->Next;
        *c = e->Args->Next->Next;
        return true;
    }
    else
    {
        return false;
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

bool MatchUnaryCall(Expr *e, char *funcName, Expr **arg)
{
    Expr *func;
    return e->Type == EXPR_CALL && TryGetTwoArgs(e, &func, arg) &&
        func->Type == EXPR_NAME && !strcmp(func->Name, funcName);
}

bool MatchBinaryCall(Expr *e, char *funcName, Expr **left, Expr **right)
{
    Expr *func;
    return e->Type == EXPR_CALL && TryGetThreeArgs(e, &func, left, right) &&
        func->Type == EXPR_NAME && !strcmp(func->Name, funcName);
}

static void PrintExpr(Expr *e)
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
    else
    {
        printf("(");
        for (Expr *p = e->Args; p; p = p->Next)
        {
            PrintExpr(p);
            printf(p->Next ? " " : ")");
        }
    }
}

void PrintProgram(Declaration *program)
{
    for (Declaration *decl = program; decl; decl = decl->Next)
    {
        if (decl->Type == DECL_FUNCTION)
        {
            printf("%s()\n    ", decl->Name);
            PrintExpr(decl->Body);
            printf("\n\n");
        }
        else if (decl->Type == DECL_CONSTANT)
        {
            printf("define %s = ", decl->Name);
            PrintExpr(decl->Body);
            printf("\n\n");
        }
        else
        {
            Panic("unhandled declaration type");
        }
    }
}
