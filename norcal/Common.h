#include <stdbool.h>
#include <stdint.h>

typedef enum ExprType
{
    EXPR_INT,
    EXPR_INDIRECT,
    EXPR_ASSIGN,
} ExprType;

typedef struct Expr Expr;
struct Expr
{
    ExprType Type;
    int32_t Int;
    Expr *Left, *Right;
};

Expr *Parse(char *filename);
void Compile(Expr *e);
void WriteImage(char *filename);

void *XAlloc(size_t size);
void Panic(char *message);
void NYI();
