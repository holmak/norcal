#include <stdio.h>
#include "Common.h"

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
