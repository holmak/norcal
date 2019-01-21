#include <stdio.h>
#include "Common.h"

size_t CountArgs(Expr *e)
{
    size_t count = 0;
    for (Expr *p = e->Args; p; p = p->Next) count++;
    return count;
}

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

static bool MatchIntExpr(Expr *e, int32_t *n)
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

static bool MatchUnaryExpr(Expr *e, ExprType type, Expr **arg)
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

static bool MatchBinaryExpr(Expr *e, ExprType type, Expr **left, Expr **right)
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

static uint16_t GetGlobalAddress(Expr *e)
{
    Expr *inner;
    int32_t address;
    if (MatchUnaryExpr(e, EXPR_INDIRECT, &inner) && MatchIntExpr(inner, &address))
    {
        return (uint16_t)address;
    }
    else
    {
        NYI();
        return 0;
    }
}

static void CompileExpression(Expr *e)
{
    int n;
    Expr *left, *right;
    if (MatchIntExpr(e, &n))
    {
        Emit(DEX);
        Emit(DEX);
        Emit_U8(LDA_IMM, n & 0xFF);
        Emit_U8(STA_ZP_X, 0);
        Emit_U8(LDA_IMM, (n >> 8) & 0xFF);
        Emit_U8(STA_ZP_X, 1);
    }
    else if (MatchUnaryExpr(e, EXPR_INDIRECT, &left))
    {
        NYI();
    }
    else if (MatchBinaryExpr(e, EXPR_ASSIGN, &left, &right))
    {
        uint16_t address = GetGlobalAddress(left);
        CompileExpression(right);
        Emit_U8(LDA_ZP_X, 0);
        Emit_U16(STA_ABS, address);
        Emit_U8(LDA_ZP_X, 1);
        Emit_U16(STA_ABS, address + 1);
        Emit(INX);
        Emit(INX);
    }
    else if (e->Type == EXPR_SEQUENCE)
    {
        for (Expr *p = e->Args; p; p = p->Next)
        {
            CompileExpression(p);
        }
    }
    else
    {
        NYI();
    }
}

void CompileProgram(Expr *e)
{
    // Prologue:
    Emit_U8(LDX_IMM, 0);

    CompileExpression(e);
}
