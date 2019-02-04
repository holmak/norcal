#include <stdio.h>
#include <string.h>
#include "Common.h"

static uint16_t GetGlobalAddress(Expr *e)
{
    Expr *inner;
    int32_t address;
    if (MatchUnaryCall(e, "*", &inner) && MatchIntExpr(inner, &address))
    {
        return (uint16_t)address;
    }
    else
    {
        NYI();
        return 0;
    }
}

// TODO: Once a symbol table is implemented, reserve two bytes of zero page
// for a temporary address variable.
#define TempPtr 0x10

// TODO: Remove this helper function once EXPR_ASSIGN is turned into a CALL expression.
static bool MatchAssignExpr(Expr *e, Expr **left, Expr **right)
{
    if (e->Type == EXPR_ASSIGN)
    {
        *left = e->Args;
        *right = e->Args->Next;
        return true;
    }
    else
    {
        return false;
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
    else if (MatchUnaryCall(e, "*", &left))
    {
        CompileExpression(left);
        // Copy address from stack into a zero-page pointer variable:
        Emit_U8(LDA_ZP_X, 0);
        Emit_U8(STA_ZP, TempPtr);
        Emit_U8(LDA_ZP_X, 1);
        Emit_U8(STA_ZP, TempPtr + 1);
        // Read through the pointer and write it back to the stack:
        Emit_U8(LDA_ZP_X_IND, 0);
        Emit_U8(STA_ZP_X, 0);
        Emit_U8(LDY_IMM, 1);
        Emit_U8(LDA_ZP_Y_IND, TempPtr);
        Emit_U8(STA_ZP_X, 1);
    }
    else if (e->Type == EXPR_CALL)
    {
        if (!e->Args) Panic("invalid call syntax");
        if (e->Args->Type != EXPR_NAME) Error("invalid function name; expressions are not accepted");
        char *func = e->Args->Name;

        int argCount = 0;
        for (Expr *p = e->Args->Next; p; p = p->Next)
        {
            CompileExpression(p);
            argCount++;
        }

        if (!strcmp(func, "+"))
        {
            if (argCount != 2) Panic("wrong number of arguments to binary operator");
            Emit(CLC);
            Emit_U8(LDA_ZP_X, 2);
            Emit_U8(ADC_ZP_X, 0);
            Emit_U8(STA_ZP_X, 2);
            Emit_U8(LDA_ZP_X, 3);
            Emit_U8(ADC_ZP_X, 1);
            Emit_U8(STA_ZP_X, 3);
            Emit(INX);
            Emit(INX);
        }
        else if (!strcmp(func, "-"))
        {
            if (argCount != 2) Panic("wrong number of arguments to binary operator");
            Emit(SEC);
            Emit_U8(LDA_ZP_X, 2);
            Emit_U8(SBC_ZP_X, 0);
            Emit_U8(STA_ZP_X, 2);
            Emit_U8(LDA_ZP_X, 3);
            Emit_U8(SBC_ZP_X, 1);
            Emit_U8(STA_ZP_X, 3);
            Emit(INX);
            Emit(INX);
        }
        else
        {
            Error("unknown function");
        }
    }
    else if (MatchAssignExpr(e, &left, &right))
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
            // TODO: Drop the result of each expression except the last.
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
