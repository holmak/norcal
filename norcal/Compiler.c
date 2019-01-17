#include <stdio.h>
#include "Common.h"

uint16_t GetGlobalAddress(Expr *e)
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

void Compile(Expr *e)
{
    switch (e->Type)
    {
    case EXPR_INT:
        printf("; int %d\n", e->Int);
        printf("    lda #$%02X\n", (uint8_t)e->Int);
        printf("    ldx #$%02X\n", (uint8_t)(e->Int >> 8));
        break;
    case EXPR_INDIRECT:
        NYI();
        break;
    case EXPR_ASSIGN:
    {
        uint16_t address = GetGlobalAddress(e->Left);
        Compile(e->Right);
        printf("; =\n");
        printf("    lda $00,X\n");
        printf("    sta $%04X\n", address);
        printf("    lda $01,X\n");
        printf("    sta $%04X\n", address + 1);
        printf("    inx\n");
        printf("    inx\n");
        break;
    }
    default:
        NYI();
        break;
    }
}
