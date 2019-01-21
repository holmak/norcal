#include <stdio.h>
#include "Common.h"

static uint16_t GetGlobalAddress(Expr *e)
{
    if (e->Type == EXPR_INDIRECT && e->Left->Type == EXPR_INT)
    {
        return e->Left->Int;
    }
    else
    {
        NYI();
        return 0;
    }
}

static void CompileExpression(Expr *e)
{
    switch (e->Type)
    {
    case EXPR_INT:
        Emit(DEX);
        Emit(DEX);
        Emit_U8(LDA_IMM, e->Int & 0xFF);
        Emit_U8(STA_ZP_X, 0);
        Emit_U8(LDA_IMM, (e->Int >> 8) & 0xFF);
        Emit_U8(STA_ZP_X, 1);
        break;
    case EXPR_INDIRECT:
        NYI();
        break;
    case EXPR_ASSIGN:
    {
        uint16_t address = GetGlobalAddress(e->Left);
        CompileExpression(e->Right);
        Emit_U8(LDA_ZP_X, 0);
        Emit_U16(STA_ABS, address);
        Emit_U8(LDA_ZP_X, 1);
        Emit_U16(STA_ABS, address + 1);
        Emit(INX);
        Emit(INX);
        break;
    }
    case EXPR_SEQUENCE:
        CompileExpression(e->Left);
        CompileExpression(e->Right);
        break;
    default:
        NYI();
        break;
    }
}

void CompileProgram(Expr *e)
{
    // Prologue:
    Emit(LDX_IMM, 0);

    CompileExpression(e);
}
