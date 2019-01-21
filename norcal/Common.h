#include <stdbool.h>
#include <stdint.h>

typedef enum ExprType
{
    EXPR_INT,
    EXPR_INDIRECT,
    EXPR_ASSIGN,
    EXPR_SEQUENCE,
} ExprType;

typedef struct Expr Expr;

struct Expr
{
    ExprType Type;
    int32_t Int;
    Expr *Args;
    Expr *Next;
};

typedef enum Opcode Opcode;
enum Opcode
{
    STA_ABS      = 0x8D,
    STA_ZP_X     = 0x95,
    LDY_IMM      = 0xA0,
    LDA_ZP_X_IND = 0xA1,
    LDX_IMM      = 0xA2,
    LDA_IMM      = 0xA9,
    LDA_ZP_X     = 0xB5,
    DEX          = 0xCA,
    INX          = 0xE8,
};

Expr *ParseFile(char *filename);
size_t CountArgs(Expr *e);
void CompileProgram(Expr *e);
void Emit(Opcode op);
void Emit_U8(Opcode op, uint8_t arg);
void Emit_U16(Opcode op, uint16_t arg);
void WriteImage(char *filename);

void *XAlloc(size_t size);
void Panic(char *message);
void Error(char *message);
void NYI();
