#include <stdio.h>
#include <string.h>
#include "Common.h"

// TODO: Once a symbol table is implemented, reserve two bytes of zero page
// for a temporary address variable.
#define TempPtr 0x10

static void CompileExpression(Expr *e)
{
    int n;
    Expr *left;
    if (MatchIntExpr(e, &n))
    {
        Emit(DEX);
        Emit(DEX);
        Emit_U8(LDA_IMM, n & 0xFF);
        Emit_U8(STA_ZP_X, 0);
        Emit_U8(LDA_IMM, (n >> 8) & 0xFF);
        Emit_U8(STA_ZP_X, 1);
    }
    else if (e->Type == EXPR_CALL)
    {
        if (!e->Args) Panic("no function specified for call");
        if (e->Args->Type != EXPR_NAME) Error("calling via function pointer is not yet implemented");
        char *func = e->Args->Name;

        int argCount = 0;
        for (Expr *p = e->Args->Next; p; p = p->Next)
        {
            CompileExpression(p);
            argCount++;
        }

        if (MatchUnaryCall(e, "*", &left) && argCount == 1)
        {
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
        else if (!strcmp(func, "+"))
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
        else if (!strcmp(func, "="))
        {
            if (argCount != 2) Panic("wrong number of arguments to binary operator");

            // Get the destination address:
            Emit_U8(LDA_ZP_X, 2);
            Emit_U8(STA_ZP, TempPtr);
            Emit_U8(LDA_ZP_X, 3);
            Emit_U8(STA_ZP, TempPtr + 1);
            // Copy the value to the destination, and also back onto the stack.
            Emit_U8(LDY_IMM, 0);
            Emit_U8(LDA_ZP_X, 0);
            Emit_U8(STA_ZP_X, 2);
            Emit_U8(STA_ZP_Y_IND, TempPtr);
            Emit(INY);
            Emit_U8(LDA_ZP_X, 1);
            Emit_U8(STA_ZP_X, 3);
            Emit_U8(STA_ZP_Y_IND, TempPtr);
            Emit(INX);
            Emit(INX);
        }
        else
        {
            Error("unknown function");
        }
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

void CompileProgram(Declaration *program)
{
    // Prologue:
    Emit_U8(LDX_IMM, 0);

    // TODO: The various vectors must jump to appropriate specially-named functions.

    for (Declaration *decl = program; decl; decl = decl->Next)
    {
        if (decl->Type == DECL_FUNCTION)
        {
            // TODO: Add the function to the symbol table.
            CompileExpression(decl->Body);
        }
        else
        {
            Panic("unhandled declaration type");
        }
    }
}
