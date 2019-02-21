#include <stdbool.h>
#include <stdint.h>

typedef enum TokenType
{
    TO_INVALID,
    TO_EOF,

    TO_LPAREN,
    TO_RPAREN,
    TO_STAR,
    TO_PLUS,
    TO_MINUS,
    TO_EQUALS,
    TO_SEMICOLON,
    TO_LBRACE,
    TO_RBRACE,

    TO_INT,
    TO_NAME,
} TokenType;

typedef enum ExprType
{
    EXPR_INT,
    EXPR_NAME,
    EXPR_CALL,
    EXPR_SEQUENCE,
} ExprType;

typedef struct Expr Expr;

struct Expr
{
    ExprType Type;
    int32_t Int;
    char *Name;
    Expr *Args;
    Expr *Next;
};

typedef enum DeclType
{
    DECL_FUNCTION,
    DECL_CONSTANT,
} DeclType;

typedef struct Declaration Declaration;

struct Declaration
{
    DeclType Type;
    char *Name;
    Expr *Body;
    Declaration *Next;
};

typedef enum Opcode Opcode;
enum Opcode
{
    CLC          = 0x18,
    SEC          = 0x38,
    ADC_ZP_X     = 0x75,
    STA_ZP       = 0x85,
    STA_ABS      = 0x8D,
    STA_ZP_Y_IND = 0x91,
    STA_ZP_X     = 0x95,
    LDY_IMM      = 0xA0,
    LDA_ZP_X_IND = 0xA1,
    LDX_IMM      = 0xA2,
    LDA_IMM      = 0xA9,
    LDA_ZP_Y_IND = 0xB1,
    LDA_ZP_X     = 0xB5,
    INY          = 0xC8,
    DEX          = 0xCA,
    INX          = 0xE8,
    SBC_ZP_X     = 0xF5,
};

// Lexer:
void InitLexer(char *filename);
bool TryParseInt(int32_t *n);
bool TryParseAnyName(char **s);
bool TryParseName(char *s);
bool TryParse(TokenType expected);
TokenType ReadNextToken();

Declaration *ParseFile(char *filename);

void CompileProgram(Declaration *program);

void Emit(Opcode op);
void Emit_U8(Opcode op, uint8_t arg);
void Emit_U16(Opcode op, uint16_t arg);
void WriteImage(char *filename);

// Syntax trees:
bool MatchIntExpr(Expr *e, int32_t *n);
bool MatchUnaryCall(Expr *e, char *funcName, Expr **arg);
bool MatchBinaryCall(Expr *e, char *funcName, Expr **left, Expr **right);
void PrintProgram(Declaration *program);

void *XAlloc(size_t size);
void Panic(char *message);
void Error(char *message);
void NYI();
